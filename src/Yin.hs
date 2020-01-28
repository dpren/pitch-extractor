module Yin where

import Data.List
import qualified Turtle as T
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS as Path
import Data.Monoid           ((<>))
import TextShow              (showt)
import Control.Monad         (foldM, foldM_, when)

import PitchLocation
import Util.Media           (createMonoAudio, spliceFile)
import Util.Misc            (echoTxt, toTxt, exec, formatDouble, getPythonPath, count)

parseOutput x = T.match (T.decimal `T.sepBy` ",") x !! 0


getPitches_yin :: T.FilePath -> T.FilePath -> IO (Either T.Text [[Double]])
getPitches_yin filePath tempPath = do
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

          let bins = groupBy (==) (parseOutput pitches)
          return (Right bins)



extractPitchTo :: T.FilePath -> T.FilePath -> T.FilePath -> IO ()
extractPitchTo outputDir tempDir filePath = do
  bins <- getPitches_yin filePath tempPath
  case bins of
    Left yinErr -> errMsg yinErr
    Right bins -> do
      let pitchSegments = qualifiedPitchSegments bins
      when (pitchSegments == []) (echoTxt ("× " <> fileName <> ": no pitches found"))
      foldM_
        (
          \prevNotes segment -> do
            case (pitchStartTime segment bins) of
              Nothing -> errMsg "pitch start time not found" >> return prevNotes
              Just startTime -> do
                let
                  duration      = computeTime segment
                  midiNote      = (truncate $ head segment) :: Int
                  dupeNoteIndex = count midiNote prevNotes
                  midiNoteName  = (showt midiNote) <> "__" <> (showt dupeNoteIndex) <> "__"
                  outputName    = midiNoteName <> fileName
                  outputPath    = outputDir </> (Path.fromText outputName)
                  _startTime    = showt startTime
                  _duration     = showt duration
                  _segment      = showt segment

                spliceCmd <- T.shellStrict (spliceFile filePath _startTime _duration outputPath) T.empty
                case spliceCmd of
                  (T.ExitFailure n, err)  -> errMsg err >> return prevNotes
                  (T.ExitSuccess, stdout) -> do

                    echoTxt $ "✔ " <> outputName
                    echoTxt $ "     time: " <> formatDouble 2 startTime
                    echoTxt $ " duration: " <> formatDouble 2 duration
                    echoTxt $ "  segment: " <> _segment

                    return (midiNote : prevNotes)
        )
        []
        pitchSegments

  where
    fileNamePath = filename filePath
    tempPath = tempDir </> fileNamePath
    fileName = toTxt fileNamePath
    errMsg e = echoTxt ("× " <> fileName <> " Error: " <> e)
