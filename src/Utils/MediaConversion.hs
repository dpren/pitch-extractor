module Utils.MediaConversion where

import qualified Turtle as T
import Utils.Misc (toTxt, exec)
import Data.Monoid ((<>))

convertToMp4 :: (T.FilePath, T.FilePath) -> T.Text
convertToMp4 paths = "ffmpeg -loglevel error"
  <> " -i "        <> toTxt (fst paths)
  <> " -ar 44.1k " <> toTxt (snd paths)

createMonoAudio :: T.FilePath -> T.FilePath -> T.Text
createMonoAudio filePath outputPath =
    "ffmpeg -loglevel error "
    <> " -i " <> (toTxt filePath)
    <> " -ar 44.1k -ac 1 " <> (toTxt outputPath)

createWav :: T.FilePath -> T.FilePath -> T.Text
createWav filePath outputPath =
  "ffmpeg -loglevel error "
  <> " -i " <> (toTxt filePath)
  <> " "    <> (toTxt outputPath)

spliceFile :: T.FilePath -> T.Text -> T.Text -> T.FilePath -> T.Text
spliceFile filePath startTime duration outputPath =
  "ffmpeg -loglevel error "
  <> " -ss " <> startTime
  <> " -i "  <> (toTxt filePath)
  <> " -t "  <> duration
  <> " "     <> (toTxt outputPath)
