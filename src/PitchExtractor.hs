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

import GetYinPitches         (extractPitchTo)
import YouTubeDownloader     (searchYoutube, download)
import Utils.MediaConversion (convertToMp4)
import Utils.Misc            (toTxt, exec, dropDotFiles, mkdirDestructive, successData)
import Types                 (VideoId)


runPitchExtractor :: IO ()
runPitchExtractor = do
  args <- T.arguments
  currentDir <- T.pwd
  -------- File system setup --------
  let searchQuery   = args !! 0
      maxResults    = args !! 1
      outputBase    = currentDir </> "vid-output"
      outputDir     = outputBase </> Path.fromText (replace " " "_" searchQuery)
      outputWavDir  = outputDir  </> "WAV"
      sourceDir     = currentDir </> "vid-source"
      sourceMp4Dir  = currentDir </> "vid-source-mp4"
      tempDir       = currentDir </> ".temp"

  T.echo "files system setup..."
  baseAlreadyExists <- T.testdir outputBase
  unless baseAlreadyExists (T.mkdir outputBase)


  -------- Download vids --------
  T.echo "\ndownloading vids..."
  mkdirDestructive sourceDir
  videoIds <- searchYoutube searchQuery maxResults
  print videoIds
  dldVids <- mapM (download sourceDir) videoIds
  -- print $ successData dldVids


  -------- Convert source to 44.1k mp4 --------
  T.echo "\ncreating 44.1k mp4s..."
  mkdirDestructive sourceMp4Dir

  sourceDirAllFiles <- map T.filename <$> (T.fold (T.ls sourceDir) F.list)

  let sourceDirFiles   = dropDotFiles sourceDirAllFiles
      sourcePathsOrig  = map (sourceDir </>) sourceDirFiles
      sourcePathsMp4   = map (\x -> sourceMp4Dir </> x `replaceExtension` "mp4") sourceDirFiles
      sourcePathsInOut = zip sourcePathsOrig sourcePathsMp4


  mp4ConvOutputs <- mapM convertToMp4 sourcePathsInOut

  let successfulMp4Paths :: [T.FilePath]
      successfulMp4Paths = successData mp4ConvOutputs


  -------- Pitch extraction from source-mp4 --------
  T.echo "\npitch extraction..."
  mkdirDestructive tempDir
  mkdirDestructive outputDir
  T.mkdir          outputWavDir


  mapM_ (extractPitchTo outputDir outputWavDir tempDir) successfulMp4Paths


  -- -------- Cleanup --------
  -- T.rmtree tempDir
  -- T.rmtree sourceDir
  -- T.rmtree sourceMp4Dir

  T.echo $ "\n\nDone, successful videos extracted to: " <> (toTxt outputDir)
