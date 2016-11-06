module ExtractPitchTo (extractPitchTo) where

import Data.List
import System.FilePath        ((-<.>), (</>), takeFileName)
import System.Process         (callCommand, spawnCommand)
import Numeric                (showFFloat)
import CalculatePitchLocation

formatDouble :: Int -> Double -> String
formatDouble numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""

createRaw :: FilePath -> FilePath -> String
createRaw filePath outputPath = concat [
     "ffmpeg -loglevel error "
   , " -i ", filePath
   , " -f f64le -ar 44.1k -ac 1 ", outputPath ]

createWav :: FilePath -> FilePath -> String
createWav filePath outputPath = concat [
     "ffmpeg -loglevel error "
   , " -i ", filePath
   , " ", outputPath ]

spliceFile :: FilePath -> String -> String -> FilePath -> String
spliceFile filePath startTime duration outputPath = concat [
     "ffmpeg -loglevel error "
   , " -ss "       , startTime
   , " -i "        , filePath
   , " -t "        , duration
   , " -c copy "   , outputPath ]



extractPitchTo :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
extractPitchTo outputDir outputWavDir tempDir filePath = do
   let fileName    = takeFileName filePath
       rawFilePath = tempDir </> fileName -<.> ".raw"

   callCommand $ createRaw filePath rawFilePath

   let bins = trackFileToBins rawFilePath
   duration <- show <$> (pitchDuration <$> bins)
   startTime <- show <$$> (pitchStartTime <$> bins)
   _longestBin <- show <$> (longestBin . dropOutliers <$> bins)
   noteName <- pitchNoteName <$> pitchSegment <$> bins

   let outputName = noteName ++ "__" ++ fileName
   let outputPath = outputDir </> outputName
   let wavFilePath = outputWavDir </> outputName -<.> ".wav"

   case startTime of
      Just time -> case (read duration :: Double) > 0.3 of
              True  -> do
                        callCommand $ spliceFile filePath time duration outputPath
                        callCommand $ createWav outputPath wavFilePath
                        putStrLn $ "✔ " ++ outputName
                        putStrLn $ "       time: " ++ formatDouble 2 (read time :: Double)
                        putStrLn $ "   duration: " ++ formatDouble 2 (read duration :: Double)
                        putStrLn $ " longestBin: " ++ _longestBin
              False -> do
                        putStrLn $ "× " ++ fileName
                        putStrLn   "   skipping: duration too short"
      Nothing -> putStrLn "longestBin not found"
