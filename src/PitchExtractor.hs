module PitchExtractor where

import Data.List
import Data.Text as Text      (pack, unpack, replace, head, Text)
import qualified Control.Foldl as F
import qualified Turtle as T
import Prelude hiding (FilePath, head)
import Filesystem.Path.CurrentOS as Path
import Data.Monoid ((<>))
import TextShow

import Control.Monad          (when, unless)
-- import System.Environment     (getArgs)
-- import System.FilePath        ((-<.>), (</>))
-- import System.Directory
-- import System.Process         (callCommand)

import ExtractPitchTo
import GetYinPitches          (_extractPitchTo, toText')
import YouTubeDownloader      (searchYoutube)
-- import Utils.MediaConversion  (convertToMp4)
-- import Utils.Misc             (dropDotFiles)

convertToMp4' :: (T.FilePath, T.FilePath) -> Text
convertToMp4' paths = "ffmpeg -loglevel error"
  <> " -i "        <> fst paths
  <> " -ar 44.1k " <> snd paths

isDotFile :: T.FilePath -> Bool
isDotFile x = Text.head (toText' x) /= '.'

dropDotFiles' :: [T.FilePath] -> [T.FilePath]
dropDotFiles' = filter isDotFile

mkdirDestructive :: T.MonadIO io => T.FilePath -> io ()
mkdirDestructive path = do
  dirExists <- T.testdir path
  when dirExists (T.rmtree path)
  T.mkdir path


runPitchExtractor :: IO ()
runPitchExtractor = do
  args <- T.arguments --getArgs
  currentDir <- T.pwd --getCurrentDirectory
  let searchQuery   = args !! 0
      maxResults    = args !! 1
      outputBase    = currentDir </> "vid-output"
      outputDir     = outputBase </> Path.fromText (replace " " "_" searchQuery)
      outputWavDir  = outputDir  </> "WAV"
      sourceDir     = currentDir </> "vid-source"
      sourceMp4Dir  = currentDir </> "vid-source-mp4"
      tempDir       = currentDir </> ".temp"

  -------- File system setup --------
  putStrLn "files system setup..."
  baseAlreadyExists <- T.testdir outputBase
  unless baseAlreadyExists (T.mkdir outputBase)
  -- createDirectoryIfMissing baseExists outputBase

  -- outputExists    <- T.testdir outputDir
  -- sourceExists    <- T.testdir sourceDir
  -- sourceMp4Exists <- T.testdir sourceMp4Dir
  -- tempExists      <- T.testdir tempDir


  -- -------- Download vids --------
  -- putStrLn "\n downloading vids..."
   -- when sourceExists $ removeDirectoryRecursive sourceDir
   -- createDirectory sourceDir

  -- mkdirDestructive sourceDir
  -- searchYoutube searchQuery maxResults sourceDir
  -- return ()


  -------- Convert source to 44.1k mp4 --------
  putStrLn "\ncreating 44.1k mp4s..."
  -- when sourceMp4Exists (T.rmtree sourceMp4Dir)
  -- T.mkdir sourceMp4Dir

  -- mkdirDestructive sourceMp4Dir


  sourceDirFilesAll <- map T.filename <$> (T.fold (T.ls sourceDir) F.list)
  let sourceDirFiles  = dropDotFiles' sourceDirFilesAll
      sourcePathsOrig = fmap (sourceDir </>) sourceDirFiles
      sourcePathsMp4  = fmap (\x -> sourceMp4Dir </> x `replaceExtension` ".mp4") sourceDirFiles
      sourcePathsBoth = zip sourcePathsOrig sourcePathsMp4


  -- mapM_ callCommand $ fmap convertToMp4' sourcePathsBoth

  cmd <- T.shellStrict (convertToMp4' sourcePathsBoth) T.empty

  cmd <- T.shellStrict (_createWav outputPath wavFilePath) T.empty
  case cmd of
    (T.ExitFailure n, err)  -> T.echo err
    (T.ExitSuccess, stdout) -> do

  -------- Pitch extraction from source-mp4 --------
  putStrLn "\npitch extraction..."
  --removeDirectoryRecursive
  -- when tempExists (T.rmtree tempDir)
  -- when outputExists (T.rmtree outputDir)
  -- T.mkdir tempDir
  -- T.mkdir outputDir
  mkdirDestructive tempDir
  mkdirDestructive outputDir
  T.mkdir outputWavDir


  -- mapM_ (_extractPitchTo outputDir outputWavDir tempDir) sourcePathsMp4


  -- -------- Cleanup --------
  --   removeDirectoryRecursive tempDir
  --   removeDirectoryRecursive sourceDir
  --   removeDirectoryRecursive sourceMp4Dir
  --
  -- putStrLn $ "\n \n Done, successful videos extracted to: " ++ outputDir
