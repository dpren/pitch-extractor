module PitchExtractor where

import PitchTrack.Track    (trackFileToList)
import Data.List
import Data.Function       (on)
import Control.Applicative (liftA2)
import Control.Monad       (when)

import System.Process      (callCommand, spawnCommand)
import System.Environment
import System.FilePath     ((-<.>), (</>), takeFileName)
import System.Directory
import Data.Text as Text   (pack, unpack, replace, Text)

import YouTubeDownloader


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
   let maxResults  = pack (args !! 1)
       outputBase = currentDir </> "vid-output"
       outputDir  = outputBase </> unpack (replace " " "_" searchQuery)
       sourceDir  = currentDir </> "vid-source"
       tempDir    = currentDir </> "temp"

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
   let fileName = takeFileName filePath
       rawFilePath = tempDir </> fileName -<.> ".raw"
       outputPath = outputDir </> fileName

   callCommand $ createRaw filePath rawFilePath

   let bins = trackFileToBins rawFilePath
   duration <- show <$> (pitchDuration <$> bins)
   startTime <- show <$$> (pitchStartTime <$> bins)

   case startTime of
      Just time -> case (read duration :: Double) > 0.3 of
                     True -> callCommand $ spliceFile filePath time duration outputPath
                     False -> putStrLn "skipping: duration too short"
      Nothing -> putStrLn "longestBin not found"



pitchStartTime :: [[Int]] -> Maybe Double
pitchStartTime bins = computeTime <$> (firstSegment bins)

pitchDuration :: [[Int]] -> Double
pitchDuration bins = computeTime $ (pitchSegment bins)


firstSegment :: [[Int]] -> Maybe [Int]
firstSegment bins = concat <$> flip take bins <$> (longestBinIndex bins)

pitchSegment :: [[Int]] -> [Int]
pitchSegment bins = longestBin bins


longestBinIndex :: [[Int]] -> Maybe Int
longestBinIndex = liftA2 elemIndex longestBin dropZeros

longestBin :: [[Int]] -> [Int]
longestBin = longest . dropZeros

longest :: [[a]] -> [a]
longest = maximumBy (compare `on` length)

dropZeros :: [[Int]] -> [[Int]]
dropZeros = fmap (takeWhile (> 0))


-- | Data binning by frequency range
trackFileToBins :: FilePath -> IO [[Int]]
trackFileToBins file = groupByQuantize 6 <$> (trackRound file)
   -- where trackRound = map round <$> trackFileToList

groupByQuantize :: Int -> [Int] -> [[Int]]
groupByQuantize range = groupBy ((==) `on` quantize range)

-- | Rounds n down to the nearest multiple of range
quantize :: Int -> Int -> Int
quantize range n = n - (n `mod` range)


-- | totalNumOfSamples / samplesPerSecond = total seconds
computeTime :: [Int] -> Double
computeTime pitchList = (totalSamples pitchList) / 44100

-- | 2048 samples per pitch computation (defaultSampleNum)
totalSamples :: [Int] -> Double
totalSamples = fromIntegral . (* 2048) . length


trackRound :: FilePath -> IO [Int]
trackRound file = fmap round <$> trackFileToList file
