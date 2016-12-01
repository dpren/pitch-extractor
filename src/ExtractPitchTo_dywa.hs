module ExtractPitchTo (extractPitchTo_dywa) where

import Data.List
import System.FilePath ((-<.>), (</>), takeFileName)
import System.Process  (callCommand, spawnCommand, readProcess)

import CalculatePitchLocation
import Utils.MediaConversion
import Utils.Misc


getPitches_dywa :: FilePath -> FilePath -> IO [[Double]]
getPitches_dywa filePath tempPath = do
  let rawFilePath = tempPath -<.> ".raw"
  callCommand $ createRaw filePath rawFilePath

  bins <- trackFileToBins rawFilePath
  return bins



extractPitchTo_dywa :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
extractPitchTo_dywa outputDir outputWavDir tempDir filePath = do
  let fileName = takeFileName filePath
      tempPath = tempDir </> fileName

  bins <- getPitches_dywa filePath tempPath

  let segment     = longestPitchSeg bins
      startTime   = pitchStartTime bins
      duration    = computeTime segment
      noteName    = pitchNoteName segment
      outputName  = noteName ++ "__" ++ fileName
      outputPath  = outputDir </> outputName
      wavFilePath = outputWavDir </> outputName -<.> ".wav"

  case startTime of
      Just time -> case (duration) > 0.3 of
              True  -> do
                        let _time = show time
                        let _duration = show duration
                        let _segment = show segment
                        callCommand $ spliceFile filePath _time _duration outputPath
                        callCommand $ createWav outputPath wavFilePath
                        putStrLn $ "✔ " ++ outputName
                        putStrLn $ "     time: " ++ formatDouble 2 time
                        putStrLn $ " duration: " ++ formatDouble 2 duration
                        putStrLn $ "  segment: " ++ _segment
              False -> do
                        putStrLn $ "× " ++ fileName
                        putStrLn   "   skipping: duration too short"
      Nothing -> putStrLn "longestBin not found"
