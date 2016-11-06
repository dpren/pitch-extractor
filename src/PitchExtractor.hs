module PitchExtractor where

import Data.List
import Data.Text as Text      (pack, unpack, replace, Text)
import Control.Monad          (when, unless)
import System.Environment     (getArgs)
import System.FilePath        ((-<.>), (</>), takeFileName)
import System.Directory
import System.Process         (callCommand, spawnCommand)

import ExtractPitchTo         (extractPitchTo)
import YouTubeDownloader      (searchYoutube)
import CalculatePitchLocation ((<$$>))

dropDotFiles :: [FilePath] -> [FilePath]
dropDotFiles = filter $ \x -> (head x) /= '.'


runPitchExtractor :: IO ()
runPitchExtractor = do
   args <- getArgs
   currentDir <- getCurrentDirectory
   let searchQuery  = pack (args !! 0)
       maxResults   = pack (args !! 1)
       outputBase   = currentDir </> "vid-output"
       outputDir    = outputBase </> unpack (replace " " "_" searchQuery)
       outputWavDir = outputDir  </> "WAV"
       sourceDir    = currentDir </> "vid-source"
       tempDir      = currentDir </> "temp"
       offlineMode  = False

   baseExists   <- doesDirectoryExist outputBase
   createDirectoryIfMissing baseExists outputBase

   outputExists <- doesDirectoryExist outputDir
   sourceExists <- doesDirectoryExist sourceDir
   tempExists   <- doesDirectoryExist tempDir

   when outputExists $ removeDirectoryRecursive outputDir
   when tempExists   $ removeDirectoryRecursive tempDir

   createDirectory outputDir
   createDirectory outputWavDir
   createDirectory tempDir

   if offlineMode
      then unless sourceExists $ error "no source directory found"
      else do
         when sourceExists $ removeDirectoryRecursive sourceDir
         createDirectory sourceDir
         searchYoutube searchQuery maxResults sourceDir
         return ()

   sourceFiles <- (sourceDir </>) <$$> dropDotFiles <$> listDirectory sourceDir
   mapM_ putStrLn sourceFiles
   mapM_ (extractPitchTo outputDir outputWavDir tempDir) sourceFiles

   outputFiles <- (outputDir </>) <$$> dropDotFiles <$> listDirectory outputDir

   unless offlineMode $ do
      removeDirectoryRecursive tempDir
      removeDirectoryRecursive sourceDir

   putStrLn $ "\n \n Done, successful videos extracted to: " ++ outputDir
