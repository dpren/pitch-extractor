module PitchExtractor where

import Data.List
import Data.Text as Text     (pack, unpack, replace, head, Text)
import qualified Control.Foldl as F
import qualified Turtle as T
import Prelude hiding        (FilePath, head)
import Filesystem.Path.CurrentOS as Path
import Data.Monoid           ((<>))
import TextShow
import Control.Monad         (when, unless)
import Control.Concurrent

import GetYinPitches         (extractPitchTo)
import YouTubeDownloader     (searchYoutube, download)
import Utils.MediaConversion (convertToMp4)
import Utils.Misc            (toTxt, exec, dropDotFiles, mkdirDestructive, successData)
import Types


runPitchExtractor :: IO ()
runPitchExtractor = do
  args <- T.arguments
  currentDir <- T.pwd
  -------- File system setup --------
  let searchQuery   = args !! 0
      maxResults    = args !! 1
      outputBase    = currentDir </> "vid-output"
      outputDir     = outputBase </> Path.fromText (replace " " "_" searchQuery)
      sourceDir     = currentDir </> "vid-source"
      sourceMp4Dir  = currentDir </> "vid-source-mp4"
      tempDir       = currentDir </> ".temp"

  T.echo "files system setup..."
  baseAlreadyExists <- T.testdir outputBase
  unless baseAlreadyExists (T.mkdir outputBase)

  mkdirDestructive sourceDir
  mkdirDestructive sourceMp4Dir
  mkdirDestructive tempDir
  mkdirDestructive outputDir

  -------- Get video ids --------
  T.echo "\nlooking for vids..."
  videoIds <- searchYoutube searchQuery maxResults

  let lessHugeThing :: VideoId -> IO ()
      lessHugeThing = hugeThing outputDir sourceDir sourceMp4Dir tempDir

      -------- Download --------
      produce :: Chan (Maybe VideoId) -> VideoId -> IO ()
      produce ch videoId = do
        T.echo $ "  downloading: " <> (fromId videoId)
        dldVid <- download sourceDir videoId
        case dldVid of
          (T.ExitFailure _, err)  -> T.echo $ "Download error: " <> (fromId videoId)
          (T.ExitSuccess, stdout) -> writeChan ch (Just videoId)

      -------- Process --------
      consume :: Chan (Maybe VideoId) -> IO Text
      consume ch = do
        maybeStr <- readChan ch
        case maybeStr of
          Just videoId -> do
            T.echo $ "  processing: " <> (fromId videoId)
            lessHugeThing videoId
            consume ch
          Nothing -> return "Done."

  mapM_ print videoIds

  chan <- newChan
  p <- forkJoin $ mapM_ (produce chan) videoIds >>
                  writeChan chan Nothing
  c <- forkJoin $ consume chan
  takeMVar c >>= T.echo

  -------- Cleanup --------
  T.rmtree tempDir
  -- T.rmtree sourceDir
  -- T.rmtree sourceMp4Dir

  T.echo $ "Successful videos extracted to: " <> (toTxt outputDir)


-- todo: make this a record
hugeThing :: T.FilePath ->
             T.FilePath ->
             T.FilePath ->
             T.FilePath ->
             VideoId -> IO ()
hugeThing outputDir sourceDir sourceMp4Dir tempDir videoId = do
  -------- Convert source to 44.1k mp4 --------
  srcPath <- T.fold (T.find (T.has $ T.text (fromId videoId)) sourceDir) F.head
  case srcPath of
    Nothing -> T.echo $ "Video file not found: " <> (fromId videoId)
    Just path -> do
      let sourceDirFiles   = [T.filename path]
          sourcePathsOrig  = map (sourceDir </>) sourceDirFiles
          sourcePathsMp4   = map (\x -> sourceMp4Dir </> x `replaceExtension` "mp4") sourceDirFiles
          sourcePathsInOut = zip sourcePathsOrig sourcePathsMp4

      mp4ConversionOutputs <- mapM convertToMp4 sourcePathsInOut

      let successfulMp4Paths = successData mp4ConversionOutputs :: [T.FilePath]

      -------- Pitch extraction from source-mp4 --------
      mapM_ (extractPitchTo outputDir tempDir) successfulMp4Paths



forkJoin :: IO a -> IO (MVar a)
forkJoin task = do
  mv <- newEmptyMVar
  forkIO (task >>= putMVar mv)
  return mv
