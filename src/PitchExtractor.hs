module PitchExtractor where

import Data.List
import Data.Text as Text     (pack, unpack, replace, head, Text)
import TextShow              (showt)
import qualified Control.Foldl as F
import qualified Turtle as T
import Prelude hiding        (FilePath, head)
import Filesystem.Path.CurrentOS as Path
import Data.Monoid           ((<>))
import Control.Monad         (unless)
import Control.Concurrent

import Yin                   (extractPitchTo)
import YouTube               (searchYoutube, download)
import Util.Media            (convertToMkvCmd, normalizeVidsIfPresent)
import Util.Misc             (echoTxt, toTxt, mkdirDestructive, uniqPathName, getPythonPath, formatDouble)
import Types


runPitchExtractor :: IO ()
runPitchExtractor = T.sh (do
  args <- T.arguments
  currentDir <- T.pwd
  -------- File system setup --------
  getPythonPath

  echoTxt "files system setup..."
  let searchQuery     = args !! 0
      maxTotalResults = args !! 1
      searchQueryName = unpack (replace " " "_" searchQuery)
      outputBase      = currentDir T.</> "vid-output"

  outputDir <- uniqPathName (outputBase T.</> searchQueryName)
  let outputName   = T.basename outputDir
      tempPrefix   = ".temp-" :: Text
      tempName     = tempPrefix <> (toTxt outputName)

  tempBase <- T.using (T.mktempdir currentDir tempName)
  let tempDir      =   tempBase T.</> "temp-wav"
      sourceDir    =   tempBase T.</> "vid-source-download"
      sourceMkvDir =   tempBase T.</> "vid-source-mkv"

  baseAlreadyExists <- T.testdir outputBase
  unless baseAlreadyExists (T.mkdir outputBase)

  mkdirDestructive outputDir
  mkdirDestructive tempDir
  mkdirDestructive sourceDir
  mkdirDestructive sourceMkvDir

  let videoDirs = VideoDirs {
      out    = outputDir
    , src    = sourceDir
    , srcMkv = sourceMkvDir
    , tmp    = tempDir
  }

  T.liftIO $ ioTasks videoDirs searchQuery maxTotalResults

  echoTxt $ "Successful videos extracted to: " <> (toTxt outputDir)
  )

ioTasks :: VideoDirs -> Text -> Text -> IO ()
ioTasks vDirs searchQuery maxTotalResults = do
  -------- Get video ids --------
  echoTxt "looking for vids..."
  videoIds <- nub <$> searchYoutube searchQuery maxTotalResults
  mapM_ print videoIds

  -------- Concurrently download/process --------
  chan <- newChan
  p <- forkJoin $ mapM_ (produce chan (src vDirs)) videoIds >>
                  writeChan chan Nothing
  c <- forkJoin $ consume chan vDirs videoIds
  takeMVar c >>= echoTxt

  -------- Normalize --------
  echoTxt "normalizing..."
  normalizeVidsIfPresent (out vDirs)


processVideo :: VideoDirs -> VideoId -> IO ()
processVideo vDirs videoId = do
  -- Build filepath references
  srcPath <- T.fold (T.find (T.has $ T.text (fromId videoId)) (src vDirs)) F.head
  case srcPath of
    Nothing -> echoTxt $ "Video file not found: " <> (fromId videoId)
    Just srcPath -> do
      let srcDirFileName = T.filename srcPath
          srcPathOrig    = (src vDirs) T.</> srcDirFileName
          srcPathMkv     = (srcMkv vDirs) T.</> T.dropExtension srcDirFileName T.<.> "mkv"
      -- Convert source to 44.1k mkv to use for extraction
      cmdOutput <- convertToMkvCmd srcPathOrig srcPathMkv
      T.rm srcPathOrig
      case cmdOutput of
        (T.ExitFailure n, err) -> echoTxt err
        (T.ExitSuccess, _) -> extractPitchTo (out vDirs) (tmp vDirs) srcPathMkv


-- Download
produce :: Chan (Maybe VideoId) -> T.FilePath -> VideoId -> IO ()
produce ch sourceDir videoId = do
  echoTxt $ "  downloading: " <> (fromId videoId)
  dldVid <- download sourceDir videoId
  case dldVid of
    (T.ExitFailure _, err)  -> echoTxt $ "Download error: " <> (fromId videoId)
    (T.ExitSuccess, stdout) -> writeChan ch (Just videoId)

-- Process
consume :: Chan (Maybe VideoId) -> VideoDirs -> [VideoId] -> IO Text
consume ch videoDirs videoIds = do
  maybeStr <- readChan ch
  case maybeStr of
    Just videoId -> do
      echoTxt $ "  processing: " <> (fromId videoId)
      echoTxt $ "  " <> (getProgress videoId videoIds)
      processVideo videoDirs videoId
      consume ch videoDirs videoIds
    Nothing -> return "Done."


forkJoin :: IO a -> IO (MVar a)
forkJoin task = do
  mv <- newEmptyMVar
  forkIO (task >>= putMVar mv)
  return mv


getProgress :: VideoId -> [VideoId] -> Text
getProgress videoId videoIds = showt percentage <> "%"
  where
    total = fromIntegral (length videoIds)
    maybeIndex = fmap (fromIntegral . (+1)) (elemIndex videoId videoIds)
    decm :: Double = (maybe 0 (/total) maybeIndex ) * 100
    percentage :: Int = round decm
