module PitchExtractor where

import Data.List
import Control.Monad          (when)
import System.Process         (callCommand, spawnCommand)
import System.Environment
import System.FilePath        ((-<.>), (</>), takeFileName)
import System.Directory
import Data.Text as Text      (pack, unpack, replace, Text)

import YouTubeDownloader      (searchYoutube)
import CalculatePitchLocation
--(trackFileToBins, pitchStartTime, pitchDuration, pitchNoteName)


a <$$> b = (fmap.fmap) a b

dropDotFiles :: [FilePath] -> [FilePath]
dropDotFiles = filter $ \x -> (head x) /= '.'

createRaw :: FilePath -> FilePath -> String
createRaw filePath rawFilePath = concat [
     "ffmpeg -i ", filePath
   , " -f f64le -ar 44.1k -ac 1 ", rawFilePath ]

spliceFile :: FilePath -> String -> String -> FilePath -> String
spliceFile filePath startTime duration outputPath = concat [
     "ffmpeg -ss " , startTime
   , " -i "        , filePath
   , " -t "        , duration
   , " -c copy "   , outputPath ]




mainFunc :: IO ()
mainFunc = do
   args <- getArgs
   currentDir <- getCurrentDirectory
   let searchQuery = pack (args !! 0)
       maxResults  = pack (args !! 1)
       outputBase  = currentDir </> "vid-output"
       outputDir   = outputBase </> unpack (replace " " "_" searchQuery)
       sourceDir   = currentDir </> "vid-source"
       tempDir     = currentDir </> "temp"

   baseExists   <- doesDirectoryExist outputBase
   createDirectoryIfMissing baseExists outputBase

   outputExists <- doesDirectoryExist outputDir
   sourceExists <- doesDirectoryExist sourceDir
   tempExists   <- doesDirectoryExist tempDir

   when outputExists $ removeDirectoryRecursive outputDir
   when sourceExists $ removeDirectoryRecursive sourceDir
   when tempExists   $ removeDirectoryRecursive tempDir

   createDirectory outputDir
   createDirectory sourceDir
   createDirectory tempDir

   searchYoutube searchQuery maxResults sourceDir

   sourceFiles <- (sourceDir </>) <$$> dropDotFiles <$> listDirectory sourceDir
   mapM_ putStrLn sourceFiles
   mapM_ (extractPitchTo outputDir tempDir) sourceFiles

   removeDirectoryRecursive tempDir
   removeDirectoryRecursive sourceDir

   putStrLn $ "\n --- \n Done, successful videos extracted to: " ++ outputDir




extractPitchTo :: FilePath -> FilePath -> FilePath -> IO ()
extractPitchTo outputDir tempDir filePath = do
   let fileName    = takeFileName filePath
       rawFilePath = tempDir </> fileName -<.> ".raw"

   callCommand $ createRaw filePath rawFilePath

   let bins = trackFileToBins rawFilePath
   duration <- show <$> (pitchDuration <$> bins)
   startTime <- show <$$> (pitchStartTime <$> bins)
   noteName <- pitchNoteName <$> pitchSegment <$> bins

   let outputPath = outputDir </> (noteName ++ "_" ++ fileName)

   case startTime of
      Just time -> case (read duration :: Double) > 0.3 of
                     True -> callCommand $ spliceFile filePath time duration outputPath
                     False -> putStrLn "skipping: duration too short"
      Nothing -> putStrLn "longestBin not found"
