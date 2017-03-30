module Utils.MediaConversion where

import qualified Turtle as T
import Utils.Misc (toTxt, exec)
import Data.Monoid ((<>))
import Data.Text

convertToMp4 :: (T.FilePath, T.FilePath) -> IO (T.ExitCode, T.FilePath)
convertToMp4 (inPath, outPath) = do
  cmdOut <- mp4Cmd
  return (fst cmdOut, outPath)
  where
    mp4Cmd = exec $
      "ffmpeg -loglevel error"
      <> " -i "        <> toTxt inPath
      <> " -ar 44.1k " <> toTxt outPath

createMonoAudio :: T.FilePath -> T.FilePath -> T.Text
createMonoAudio filePath outputPath =
  "ffmpeg -loglevel error "
  <> " -i " <> (toTxt filePath)
  <> " -ar 44.1k -ac 1 " <> (toTxt outputPath)

spliceFile :: T.FilePath -> T.Text -> T.Text -> T.FilePath -> T.Text
spliceFile filePath startTime duration outputPath =
  "ffmpeg -loglevel error "
  <> " -ss " <> startTime
  <> " -i "  <> (toTxt filePath)
  <> " -t "  <> duration
  <> " "     <> (toTxt outputPath)

normalizeVids :: T.FilePath -> IO (T.ExitCode, Text)
normalizeVids dir = exec $
  "ffmpeg-normalize "
  <> " -o "    -- output to "normalize/"
  <> " -u "    -- merge with video
  <> " -f "    -- overwrite
  <> " -l -5 " -- dB peak volume
  <> (toTxt dir) <> "/*.mp4 "
