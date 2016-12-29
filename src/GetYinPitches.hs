{-# LANGUAGE OverloadedStrings #-}

module GetYinPitches (_getPitches_yin) where

import Data.List
import qualified Turtle as T
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS as Path
import Data.Monoid ((<>))
import TextShow

-- import System.FilePath ((-<.>), (</>), takeFileName)
-- import System.Process  (callCommand)
import CalculatePitchLocation
-- import Utils.MediaConversion
-- import Utils.Misc
import Numeric (showFFloat)


parseOutput x = T.match (T.decimal `T.sepBy` ",") x !! 0

_toText :: T.FilePath -> T.Text
_toText = T.format T.fp

_formatDouble :: Int -> Double -> T.Text
_formatDouble numOfDecimals floatNum = showt $ showFFloat (Just numOfDecimals) floatNum ""

_createWav :: T.FilePath -> T.FilePath -> T.Text
_createWav filePath outputPath =
  "ffmpeg -loglevel error "
  <> " -i " <> (_toText filePath)
  <> " "    <> (_toText outputPath)

_spliceFile :: T.FilePath -> T.Text -> T.Text -> T.FilePath -> T.Text
_spliceFile filePath startTime duration outputPath =
  "ffmpeg -loglevel error "
  <> " -ss " <> startTime
  <> " -i "  <> (_toText filePath)
  <> " -t "  <> duration
  <> " "     <> (_toText outputPath)

_createMonoAudio :: T.FilePath -> T.FilePath -> T.Text
_createMonoAudio filePath outputPath =
    "ffmpeg -loglevel error "
    <> " -i " <> (_toText filePath)
    <> " -ar 44.1k -ac 1 " <> (_toText outputPath)



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
            noteName    = _pitchNoteNameMIDI segment
            outputName  = noteName <> "__" <> (_toText fileName)
            outputPath  = outputDir </> (Path.fromText outputName)
            wavFilePath = outputWavDir </> (Path.fromText outputName) `replaceExtension` ".wav"

        case startTime of
          Just time -> case (duration > 0.3) of
                True  -> do
                          let _time     = showt time
                              _duration = showt duration
                              _segment  = showt segment
                          spliceCmd <- T.shellStrict (_spliceFile filePath _time _duration outputPath) T.empty
                          case spliceCmd of
                            (T.ExitFailure n, err)  -> T.echo err
                            (T.ExitSuccess, stdout) -> do
                                                        crtWavCmd <- T.shellStrict (_createWav outputPath wavFilePath) T.empty
                                                        case crtWavCmd of
                                                          (T.ExitFailure n, err)  -> T.echo err
                                                          (T.ExitSuccess, stdout) -> do
                                                                                      T.echo $ "✔ " <> outputName
                                                                                      T.echo $ "     time: " <> _formatDouble 2 time
                                                                                      T.echo $ " duration: " <> _formatDouble 2 duration
                                                                                      T.echo $ "  segment: " <> _segment
                False -> do
                          T.echo $ "× " <> (_toText fileName)
                          T.echo   " skipping: duration too short"
          Nothing -> putStrLn "longestBin not found"
