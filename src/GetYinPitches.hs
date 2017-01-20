module GetYinPitches
  -- (extractPitchTo)
  where

import Data.List
import qualified Turtle as T
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS as Path
import Data.Monoid ((<>))
import TextShow    (showt)
import Numeric     (showFFloat)

import CalculatePitchLocation
-- import Utils.MediaConversion
import Utils.Misc (toTxt, exec)


parseOutput x = T.match (T.decimal `T.sepBy` ",") x !! 0


_formatDouble :: Int -> Double -> T.Text
_formatDouble numOfDecimals floatNum = showt $ showFFloat (Just numOfDecimals) floatNum ""

_createWav :: T.FilePath -> T.FilePath -> T.Text
_createWav filePath outputPath =
  "ffmpeg -loglevel error "
  <> " -i " <> (toTxt filePath)
  <> " "    <> (toTxt outputPath)

_spliceFile :: T.FilePath -> T.Text -> T.Text -> T.FilePath -> T.Text
_spliceFile filePath startTime duration outputPath =
  "ffmpeg -loglevel error "
  <> " -ss " <> startTime
  <> " -i "  <> (toTxt filePath)
  <> " -t "  <> duration
  <> " "     <> (toTxt outputPath)

_createMonoAudio :: T.FilePath -> T.FilePath -> T.Text
_createMonoAudio filePath outputPath =
    "ffmpeg -loglevel error "
    <> " -i " <> (toTxt filePath)
    <> " -ar 44.1k -ac 1 " <> (toTxt outputPath)

pythonPath = "/usr/local/bin/python"


_getPitches_yin :: T.FilePath -> T.FilePath -> IO (Either T.Text [[Double]])
_getPitches_yin filePath tempPath = do
  let monoFilePath = tempPath `replaceExtension` ".wav"
      monoAudioCmd = _createMonoAudio filePath monoFilePath
      yinCmd       = ["yin_pitch.py", (toTxt monoFilePath)]

  cmdOutput <- T.shellStrict monoAudioCmd T.empty
  case eith cmdOutput of
    Left err -> return (Left (err <> " (createMonoAudio)"))
    Right stdout -> do

      yin_pitches <- T.procStrict pythonPath yinCmd T.empty
      case eith yin_pitches of
        Left err -> return (Left (err <> " (yin_pitch.py)"))
        Right pitches -> do

          let bins = groupByEq (parseOutput pitches)
          return (Right bins)


eith :: (T.ExitCode, T.Text) -> (Either T.Text T.Text)
eith output = case output of
  (T.ExitFailure n, err) -> Left err
  (T.ExitSuccess, stdout) -> Right stdout


-- shellThing :: T.MonadIO io => io (T.ExitCode, T.Text) -> (T.ExitCode, T.Text) -> io (T.ExitCode, T.Text)
-- shellThing cmd result = case result of
--   (T.ExitFailure n, err)  -> T.echo ("× " <> "fName" <> " Error:\n  " <> err)
--   (T.ExitSuccess, stdout) -> cmd


-- exec (_spliceFile filePath _time _duration outputPath)
--   >>= shellThing (exec (_createWav outputPath wavFilePath))

-- shellThing (exec (_spliceFile filePath _time _duration outputPath)) -> do
--   shellThing (exec (_createWav outputPath wavFilePath)) ->

-- errorMsg fName e = T.echo ("× " <> fName <> " Error:\n  " <> e)

-- spliceCmd <- exec (_spliceFile filePath _time _duration outputPath)
-- crtWavCmd <- exec (_createWav outputPath wavFilePath)


extractPitchTo :: T.FilePath -> T.FilePath -> T.FilePath -> T.FilePath -> IO ()
extractPitchTo outputDir outputWavDir tempDir filePath = do

  bins <- _getPitches_yin filePath tempPath
  case bins of
    Left yinErr -> errMsg yinErr
    Right bins -> do
        let segment     = longestPitchSeg bins
            startTime   = pitchStartTime bins
            duration    = computeTime segment
            midiNote    = showt (head segment)
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

                spliceCmd <- T.shellStrict (_spliceFile filePath _time _duration outputPath) T.empty
                case spliceCmd of
                  (T.ExitFailure n, err)  -> errMsg err
                  (T.ExitSuccess, stdout) -> do

                    crtWavCmd <- T.shellStrict (_createWav outputPath wavFilePath) T.empty
                    case crtWavCmd of
                      (T.ExitFailure n, err)  -> errMsg err
                      (T.ExitSuccess, stdout) -> do

                        T.echo $ "✔ " <> outputName
                        T.echo $ "     time: " <> _formatDouble 2 time
                        T.echo $ " duration: " <> _formatDouble 2 duration
                        T.echo $ "  segment: " <> _segment



  where
    fileNamePath = filename filePath
    tempPath = tempDir </> fileNamePath
    fileName = toTxt fileNamePath
    errMsg e = T.echo ("× " <> fileName <> " Error:\n  " <> e)
