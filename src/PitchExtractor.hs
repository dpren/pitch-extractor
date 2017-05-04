module PitchExtractor where

import Data.List
import Data.Text as Text     (pack, unpack, replace, head, Text)
import qualified Control.Foldl as F
import qualified Turtle as T
import Prelude hiding        (FilePath, head)
import Filesystem.Path.CurrentOS as Path
import Data.Monoid           ((<>))
import Control.Monad         (unless)
import Control.Concurrent

import Yin                   (extractPitchTo)
import YouTube               (searchYoutube, download)
import Util.Media            (convertToMp4Cmd, normalizeVidsIfPresent)
import Util.Misc             (toTxt, mkdirDestructive, uniqPathName)
import Types


runPitchExtractor :: IO ()
runPitchExtractor = T.sh (do
  args <- T.arguments
  currentDir <- T.pwd
  -------- File system setup --------
  T.echo "files system setup..."
  let searchQuery     = args !! 0
      maxTotalResults = args !! 1
      searchQueryName = T.fromText (replace " " "_" searchQuery)
      outputBase      = currentDir </> "vid-output"

  outputDir <- uniqPathName (outputBase </> searchQueryName)
  let outputName   = T.basename outputDir
      tempPrefix   = ".temp-" :: Text
      tempName     = tempPrefix <> (toTxt outputName)

  tempBase <- T.using (T.mktempdir currentDir tempName)
  let tempDir      =   tempBase </> "temp-wav"
      sourceDir    =   tempBase </> "vid-source-download"
      sourceMp4Dir =   tempBase </> "vid-source-mp4"

  baseAlreadyExists <- T.testdir outputBase
  unless baseAlreadyExists (T.mkdir outputBase)

  mkdirDestructive outputDir
  mkdirDestructive tempDir
  mkdirDestructive sourceDir
  mkdirDestructive sourceMp4Dir

  let videoDirs = VideoDirs {
      out    = outputDir
    , src    = sourceDir
    , srcMp4 = sourceMp4Dir
    , tmp    = tempDir
  }

  T.liftIO $ ioTasks videoDirs searchQuery maxTotalResults

  T.echo $ "Successful videos extracted to: " <> (toTxt outputDir)
  )

ioTasks :: VideoDirs -> Text -> Text -> IO ()
ioTasks vDirs searchQuery maxTotalResults = do
  -------- Get video ids --------
  T.echo "\nlooking for vids..."
  videoIds <- searchYoutube searchQuery maxTotalResults
  mapM_ print videoIds

  -------- Concurrently download/process --------
  chan <- newChan
  p <- forkJoin $ mapM_ (produce chan (src vDirs)) videoIds >>
                  writeChan chan Nothing
  c <- forkJoin $ consume chan vDirs
  takeMVar c >>= T.echo

  -------- Normalize --------
  T.echo "normalizing..."
  normalizeVidsIfPresent (out vDirs)


processVideo :: VideoDirs -> VideoId -> IO ()
processVideo vDirs videoId = do
  -- Build filepath references
  srcPath <- T.fold (T.find (T.has $ T.text (fromId videoId)) (src vDirs)) F.head
  case srcPath of
    Nothing -> T.echo $ "Video file not found: " <> (fromId videoId)
    Just srcPath -> do
      let srcDirFileName = T.filename srcPath
          srcPathOrig    = (src vDirs) </> srcDirFileName
          srcPathMp4     = (srcMp4 vDirs) </> srcDirFileName `replaceExtension` "mp4"
      -- Convert source to 44.1k mp4 to use for extraction
      cmdOutput <- convertToMp4Cmd srcPathOrig srcPathMp4
      case cmdOutput of
        (T.ExitFailure n, err) -> T.echo err
        (T.ExitSuccess, _) -> extractPitchTo (out vDirs) (tmp vDirs) srcPathMp4


-- Download
produce :: Chan (Maybe VideoId) -> T.FilePath -> VideoId -> IO ()
produce ch sourceDir videoId = do
  T.echo $ "  downloading: " <> (fromId videoId)
  dldVid <- download sourceDir videoId
  case dldVid of
    (T.ExitFailure _, err)  -> T.echo $ "Download error: " <> (fromId videoId)
    (T.ExitSuccess, stdout) -> writeChan ch (Just videoId)

-- Process
consume :: Chan (Maybe VideoId) -> VideoDirs -> IO Text
consume ch videoDirs = do
  maybeStr <- readChan ch
  case maybeStr of
    Just videoId -> do
      T.echo $ "  processing: " <> (fromId videoId)
      processVideo videoDirs videoId
      consume ch videoDirs
    Nothing -> return "Done."


forkJoin :: IO a -> IO (MVar a)
forkJoin task = do
  mv <- newEmptyMVar
  forkIO (task >>= putMVar mv)
  return mv
