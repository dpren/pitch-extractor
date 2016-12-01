module Utils.MediaConversion where

convertToMp4 :: (FilePath, FilePath) -> String
convertToMp4 paths = concat [
    "ffmpeg -loglevel error"
  , " -i ", fst paths
  , " -ar 44.1k ", snd paths ]


createRaw :: FilePath -> FilePath -> String
createRaw filePath outputPath = concat [
    "ffmpeg -loglevel error "
  , " -i ", filePath
  , " -f f64le -ar 44.1k -ac 1 ", outputPath ]


createMonoAudio :: FilePath -> FilePath -> String
createMonoAudio filePath outputPath = concat [
    "ffmpeg -loglevel error "
  , " -i ", filePath
  , " -ar 44.1k -ac 1 ", outputPath ]


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
