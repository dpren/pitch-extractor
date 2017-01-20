module PitchExtractor where

import Data.List
import Data.Text as Text  (pack, unpack, replace, head, Text)
import qualified Control.Foldl as F
import qualified Turtle as T
import Prelude hiding     (FilePath, head)
import Filesystem.Path.CurrentOS as Path
import Data.Monoid        ((<>))
import TextShow
import Control.Monad      (when, unless)

import GetYinPitches      (extractPitchTo)
import YouTubeDownloader  (searchYoutube)
-- import Utils.MediaConversion  (convertToMp4)
import Utils.Misc         (toTxt, exec, dropDotFiles)


convertToMp4' :: (T.FilePath, T.FilePath) -> Text
convertToMp4' paths = "ffmpeg -loglevel error"
  <> " -i "        <> toTxt (fst paths)
  <> " -ar 44.1k " <> toTxt (snd paths)

mkdirDestructive :: T.MonadIO io => T.FilePath -> io ()
mkdirDestructive path = do
  dirExists <- T.testdir path
  when dirExists (T.rmtree path)
  T.mkdir path

wasSuccessful :: (T.ExitCode, b) -> Bool
wasSuccessful a = (fst a) == T.ExitSuccess


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
  T.echo "\n downloading vids..."
  mkdirDestructive sourceDir
  searchYoutube searchQuery maxResults sourceDir


  -------- Convert source to 44.1k mp4 --------
  T.echo "\ncreating 44.1k mp4s..."
  mkdirDestructive sourceMp4Dir

  sourceDirAllFiles <- map T.filename <$> (T.fold (T.ls sourceDir) F.list)

  let sourceDirFiles   = dropDotFiles sourceDirAllFiles
      sourcePathsOrig  = map (sourceDir </>) sourceDirFiles
      sourcePathsMp4   = map (\x -> sourceMp4Dir </> x `replaceExtension` "mp4") sourceDirFiles
      sourcePathsInOut = zip sourcePathsOrig sourcePathsMp4

      convToMp4Cmds :: [Text]
      convToMp4Cmds = map convertToMp4' sourcePathsInOut

  outputs <- mapM exec convToMp4Cmds

  let
      outputsWithPath :: [((T.ExitCode, Text), T.FilePath)]
      outputsWithPath = zip outputs sourcePathsMp4

      successfulMp4Paths :: [T.FilePath]
      successfulMp4Paths = map snd $ filter (wasSuccessful . fst) outputsWithPath


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

  T.echo $ "\n\n Done, successful videos extracted to: " <> (toTxt outputDir)
