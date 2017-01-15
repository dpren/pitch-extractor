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
  <> " -i "        <> toText' (fst paths)
  <> " -ar 44.1k " <> toText' (snd paths)


-- let cmNums = P.zip cms nums
-- let cmds = P.zip cms nums
-- resNums <- mapM (exec . fst) cmNums

-- let exec = (flip Turtle.shellStrict Turtle.empty)
-- let ltrs = ["a", "b", "`", "d"] :: [Text]
-- let nums = ["1", "2", "3", "4"] :: [Text]
-- let cmd ab = ("echo " :: Text) <> fst ab <> snd ab
-- let cms = P.map cmd $ P.zip ltrs nums
-- res <- mapM exec cms
-- P.zip res nums






isDotFile :: T.FilePath -> Bool
isDotFile x = Text.head (toText' x) /= '.'

dropDotFiles' :: [T.FilePath] -> [T.FilePath]
dropDotFiles' = filter isDotFile

mkdirDestructive :: T.MonadIO io => T.FilePath -> io ()
mkdirDestructive path = do
  dirExists <- T.testdir path
  when dirExists (T.rmtree path)
  T.mkdir path

exec :: T.MonadIO io => Text -> io (T.ExitCode, Text)
exec = (flip T.shellStrict T.empty)

wasSuccessful :: (T.ExitCode, b) -> Bool
wasSuccessful a = (fst a) == T.ExitSuccess


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
      sourcePathsOrig = map (sourceDir </>) sourceDirFiles
      sourcePathsMp4  = map (\x -> sourceMp4Dir </> x `replaceExtension` ".mp4") sourceDirFiles
      sourcePathsInOut = zip sourcePathsOrig sourcePathsMp4

      -- convToMp4Cmds :: [(Text, T.FilePath)]
      -- commands = (map convertToMp4' sourcePathsInOut)
      convToMp4Cmds = map convertToMp4' sourcePathsInOut

  outputs <- mapM exec convToMp4Cmds

  -- outputsWithPath :: [((T.ExitCode, Text), T.FilePath)]
  let outputsWithPath = zip outputs sourcePathsMp4

  -- successfulMp4Paths :: [T.FilePath]
  let successfulMp4Paths = map snd $ filter (wasSuccessful . fst) outputsWithPath


  -------- Pitch extraction from source-mp4 --------
  putStrLn "\npitch extraction..."
  mkdirDestructive tempDir
  mkdirDestructive outputDir
  T.mkdir          outputWavDir


  mapM_ (_extractPitchTo outputDir outputWavDir tempDir) successfulMp4Paths


  -- -------- Cleanup --------
  --   removeDirectoryRecursive tempDir
  --   removeDirectoryRecursive sourceDir
  --   removeDirectoryRecursive sourceMp4Dir
  --
  -- putStrLn $ "\n \n Done, successful videos extracted to: " ++ outputDir
