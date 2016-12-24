module PitchExtractor where

import Data.List
import Data.Text as Text      (pack, unpack, replace, Text)
import Control.Monad          (when, unless)
import System.Environment     (getArgs)
import System.FilePath        ((-<.>), (</>), takeFileName)
import System.Directory
import System.Process         (callCommand)

import ExtractPitchTo
import YouTubeDownloader      (searchYoutube)
import Utils.MediaConversion  (convertToMp4)
import Utils.Misc             (dropDotFiles)


runPitchExtractor :: IO ()
runPitchExtractor = do
  args <- getArgs
  currentDir <- getCurrentDirectory
  let searchQuery   = pack (args !! 0)
      maxResults    = pack (args !! 1)
      outputBase    = currentDir </> "vid-output"
      outputDir     = outputBase </> unpack (replace " " "_" searchQuery)
      outputWavDir  = outputDir  </> "WAV"
      sourceDir     = currentDir </> "vid-source"
      sourceMp4Dir  = currentDir </> "vid-source-mp4"
      tempDir       = currentDir </> ".temp"
      offlineMode   = False

  -------- File system setup --------
  putStrLn "files system setup..."
  baseExists   <- doesDirectoryExist outputBase
  createDirectoryIfMissing baseExists outputBase

  outputExists    <- doesDirectoryExist outputDir
  sourceExists    <- doesDirectoryExist sourceDir
  sourceMp4Exists <- doesDirectoryExist sourceMp4Dir
  tempExists      <- doesDirectoryExist tempDir


  -- -------- Download vids --------
  -- putStrLn "\n downloading vids..."
  -- -- when sourceExists $ removeDirectoryRecursive sourceDir
  -- -- createDirectory sourceDir
  -- searchYoutube searchQuery maxResults sourceDir
  -- return ()


  -------- Convert source to 44.1k mp4 --------
  -- putStrLn "\ncreating 44.1k mp4s..."
  -- when sourceMp4Exists $ removeDirectoryRecursive sourceMp4Dir
  -- createDirectory sourceMp4Dir

  sourceDirFiles <- dropDotFiles <$> listDirectory sourceDir

  let sourcePathsOrig = fmap (sourceDir </>) sourceDirFiles
      sourcePathsMp4  = fmap (\x -> sourceMp4Dir </> x -<.> ".mp4") sourceDirFiles

  --     sourcePathsBoth = zip sourcePathsOrig sourcePathsMp4
  --
  -- mapM_ callCommand $ fmap convertToMp4 sourcePathsBoth


  -------- Pitch extraction from source-mp4 --------
  putStrLn "\npitch extraction..."
  when tempExists $ removeDirectoryRecursive tempDir
  createDirectory tempDir
  when outputExists $ removeDirectoryRecursive outputDir
  createDirectory outputDir
  createDirectory outputWavDir

  mapM_ (extractPitchTo outputDir outputWavDir tempDir) sourcePathsMp4


  -- -------- Cleanup --------
  -- -- unless offlineMode $ do
  --   -- removeDirectoryRecursive tempDir
  --   -- removeDirectoryRecursive sourceDir
  --   -- removeDirectoryRecursive sourceMp4Dir
  --
  -- putStrLn $ "\n \n Done, successful videos extracted to: " ++ outputDir
