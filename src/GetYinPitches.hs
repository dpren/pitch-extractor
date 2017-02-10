module GetYinPitches
  -- (extractPitchTo)
  where

import Data.List
import qualified Turtle as T
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS as Path
import Data.Monoid           ((<>))
import TextShow              (showt)

import CalculatePitchLocation
import Utils.MediaConversion (createMonoAudio, createWav, spliceFile)
import Utils.Misc            (toTxt, exec, formatDouble, getPythonPath)

parseOutput x = T.match (T.decimal `T.sepBy` ",") x !! 0


_getPitches_yin :: T.FilePath -> T.FilePath -> IO (Either T.Text [[Double]])
_getPitches_yin filePath tempPath = do
  let monoFilePath = tempPath `replaceExtension` ".wav"
      monoAudioCmd = createMonoAudio filePath monoFilePath
      yinCmd       = ["yin_pitch.py", (toTxt monoFilePath)]

  pythonPath <- getPythonPath

  cmdOutput <- T.shellStrict monoAudioCmd T.empty
  case cmdOutput of
    (T.ExitFailure n, err) -> return (Left (err <> " (createMonoAudio)"))
    (T.ExitSuccess, stdout) -> do

      yin_pitches <- (T.procStrict pythonPath yinCmd T.empty)
      case yin_pitches of
        (T.ExitFailure n, err) -> return (Left (err <> " (yin_pitch.py)"))
        (T.ExitSuccess, pitches) -> do

          let bins = groupByEq (parseOutput pitches)
          return (Right bins)



extractPitchTo :: T.FilePath -> T.FilePath -> T.FilePath -> T.FilePath -> IO ()
extractPitchTo outputDir outputWavDir tempDir filePath = do

  bins <- _getPitches_yin filePath tempPath
  case bins of
    Left yinErr -> errMsg yinErr
    Right bins -> do
        let segment     = longestPitchSeg bins
            startTime   = pitchStartTime bins
            duration    = computeTime segment
            midiNote    = showt ((truncate $ head segment) :: Int)
            outputName  = midiNote <> "__" <> fileName
            outputPath  = outputDir </> (Path.fromText outputName)
            wavFilePath = outputWavDir </> (Path.fromText outputName) `replaceExtension` ".wav"

        case startTime of
          Nothing   -> errMsg "longestBin not found"
          Just time ->
            case (duration > 0.3) of
              False -> do
                T.echo $ "× " <> fileName
                T.echo   " skipping: duration too short"
              True  -> do
                let _time     = showt time
                    _duration = showt duration
                    _segment  = showt segment

                spliceCmd <- T.shellStrict (spliceFile filePath _time _duration outputPath) T.empty
                case spliceCmd of
                  (T.ExitFailure n, err)  -> errMsg err
                  (T.ExitSuccess, stdout) -> do

                    crtWavCmd <- T.shellStrict (createWav outputPath wavFilePath) T.empty
                    case crtWavCmd of
                      (T.ExitFailure n, err)  -> errMsg err
                      (T.ExitSuccess, stdout) -> do

                        T.echo $ "✔ " <> outputName
                        T.echo $ "     time: " <> formatDouble 2 time
                        T.echo $ " duration: " <> formatDouble 2 duration
                        T.echo $ "  segment: " <> _segment



  where
    fileNamePath = filename filePath
    tempPath = tempDir </> fileNamePath
    fileName = toTxt fileNamePath
    errMsg e = T.echo ("× " <> fileName <> " Error:\n  " <> e)
