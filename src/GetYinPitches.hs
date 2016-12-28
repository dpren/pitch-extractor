{-# LANGUAGE OverloadedStrings #-}

module GetYinPitches where

import Data.List
import qualified Turtle as T
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS as Path
import Data.Monoid ((<>))

-- import System.FilePath ((-<.>), (</>), takeFileName)
-- import System.Process  (callCommand)
import CalculatePitchLocation
-- import Utils.MediaConversion
-- import Utils.Misc
import Data.List.Split (splitOn)



-- _strToDbls :: Text -> [Double]
-- _strToDbls = map read . (splitOn ",")

parseOutput x = T.match (T.decimal `T.sepBy` ",") x !! 0

-- _formatDouble :: Int -> Double -> String
-- _formatDouble numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""

-- _createWav :: FilePath -> FilePath -> String
-- _createWav filePath outputPath = concat [
--     "ffmpeg -loglevel error "
--   , " -i ", filePath
--   , " "   , outputPath ]
--
-- _spliceFile :: FilePath -> String -> String -> FilePath -> String
-- _spliceFile filePath startTime duration outputPath = concat [
--     "ffmpeg -loglevel error "
--   , " -ss ", startTime
--   , " -i " , filePath
--   , " -t " , duration
--   , " "    , outputPath ]

_createMonoAudio :: T.FilePath -> T.FilePath -> T.Text
_createMonoAudio filePath outputPath =
    "ffmpeg -loglevel error " <> " -i " (T.format T.fp filePath) <> " -ar 44.1k -ac 1 " <> (T.format T.fp outputPath)



_getPitches_yin :: T.FilePath -> T.FilePath -> IO (Maybe [[Double]])
_getPitches_yin filePath tempPath = do
  let monoFilePath = tempPath `replaceExtension` ".wav"
  -- let yinCmd = "/usr/local/bin/python" ["yin_pitch.py", monoFilePath]

  monoAudioCmd <- T.shellStrict (_createMonoAudio filePath monoFilePath) T.empty
  case monoAudioCmd of
    (T.ExitFailure n, err) -> do
                      T.echo err
                      return Nothing
    (T.ExitSuccess, stdout) -> do
                      yin_pitches <- T.procStrict "/usr/local/bin/python" ["yin_pitch.py", (T.format T.fp monoFilePath)] T.empty
                      case yin_pitches of
                        (T.ExitFailure n, err) -> do
                                              T.echo err
                                              return Nothing
                        (T.ExitSuccess, pitches) -> do
                                              let bins = groupByEq (parseOutput pitches)
                                              return (Just bins)



-- extractPitchTo :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
_extractPitchTo :: T.FilePath -> T.FilePath -> T.FilePath -> T.FilePath -> IO ()
_extractPitchTo outputDir outputWavDir tempDir filePath = do
  let fileName = filename filePath
      tempPath = tempDir </> fileName

  bins <- _getPitches_yin filePath tempPath
  case bins of
    Nothing -> T.echo "yin pitches not found"
    Just bins -> do
        let segment     = longestPitchSeg bins
            startTime   = pitchStartTime bins
            duration    = computeTime segment
            noteName    = pitchNoteNameMIDI segment
            outputName  = noteName <> "__" <> fileName
            outputPath  = outputDir </> outputName
            wavFilePath = outputWavDir </> outputName `replaceExtension` ".wav"

        case startTime of
            Just time -> case (duration) > 0.3 of
                    True  -> do
                              let _time = show time
                              let _duration = show duration
                              let _segment = show segment
                              -- T.inshellWithErr (_spliceFile filePath _time _duration outputPath) empty
                              -- T.inshellWithErr (_createWav outputPath wavFilePath) empty
                              T.echo $ "✔ " <> outputName
                              -- T.echo $ "     time: " <> _formatDouble 2 time
                              -- T.echo $ " duration: " <> _formatDouble 2 duration
                              T.echo $ "  segment: " <> _segment
                    False -> do
                              T.echo $ "× " <> fileName
                              T.echo   " skipping: duration too short"
            Nothing -> putStrLn "longestBin not found"
