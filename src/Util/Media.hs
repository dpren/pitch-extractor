module Util.Media where

import qualified Turtle as T
import qualified Control.Foldl as Fold
import Filesystem.Path.CurrentOS as Path
import Data.Monoid ((<>))
import Data.Text
import Data.List (null)
import Util.Misc (toTxt, exec)
import Control.Monad (unless)

convertToMp4Cmd inPath outPath = exec $
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

normalizeVidsIfPresent :: T.FilePath -> IO ()
normalizeVidsIfPresent outDir = do
  files <- lsMp4s outDir
  unless (Data.List.null files) (normalizeVids outDir)

normalizeVids :: T.FilePath -> IO ()
normalizeVids outDir = do
  normalizeCmdOut <- (exec . normalizeCmd) outDir
  case normalizeCmdOut of
    (T.ExitFailure n, err) -> error $ unpack ("ffmpeg-normalize failure" <> err)
    (T.ExitSuccess, _) -> do
      T.echo "ffmpeg-normalize success"
      let normalizedDir = outDir </> "normalized"
      originalFiles   <- lsMp4s outDir
      normalizedFiles <- lsMp4s normalizedDir

      mapM_ T.rm originalFiles
      mapM_
        (\normFile -> T.mv normFile (outDir </> T.filename normFile))
        normalizedFiles
      T.rmdir normalizedDir

normalizeCmd :: T.FilePath -> Text
normalizeCmd outDir =
  "ffmpeg-normalize"
  <> " -o "    -- output to "normalize/"
  <> " -u "    -- merge with video
  <> " -f "    -- overwrite
  -- <> " -l -5 " -- dB peak volume
  <> (toTxt (outDir </> "*.mp4"))

lsMp4s :: T.MonadIO io => T.FilePath -> io [T.FilePath]
lsMp4s = lsByPattern (T.ends ".mp4")

lsByPattern :: T.MonadIO io => T.Pattern a -> T.FilePath -> io [T.FilePath]
lsByPattern pattern dir = T.fold (
    T.ls dir
    T.& fmap (T.format T.fp)
    T.& T.grep pattern
    T.& fmap T.fromText
  ) Fold.list
