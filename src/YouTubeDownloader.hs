module YouTubeDownloader where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.ByteString.Lazy as SL (putStrLn)
import Data.Text as Text          (pack, unpack, Text)
import Control.Monad              (when, unless)
import Data.Monoid                ((<>))

import qualified Turtle as T
import Prelude hiding             (FilePath)
import Utils.Misc                 (toTxt, exec)
import Types                      (VideoId)

api_key = "AIzaSyCCYfqHdQPxyhbNAPlUeSecvBnoQK0kQhk"

download :: T.FilePath -> VideoId -> IO (T.ExitCode, VideoId)
download path videoId = do
  cmdOut <- downloadCmd path videoId
  return (fst cmdOut, videoId)
  
  where
    downloadCmd :: T.FilePath -> VideoId -> IO (T.ExitCode, Text)
    downloadCmd path videoId = exec $
      "youtube-dl -o "
      <> "'" <> (toTxt path) <> "/%(id)s.%(ext)s" <> "'"
      <> " -f 'bestvideo[height<=480]+bestaudio/best[height<=480]' "
      <> " --min-sleep-interval 1 "
      <> " --max-sleep-interval 200 "
      <> " --no-warnings "
      <> " --abort-on-error "
      -- <> " --ignore-errors "
      <> " -- " <> videoId


searchYoutube :: Text -> Text -> IO [VideoId]
searchYoutube query maxResults = do
  let opts = defaults & param "part"            .~ ["snippet"]
                      & param "key"             .~ [api_key]
                      & param "maxResults"      .~ [maxResults]
                      & param "type"            .~ ["video"]
                      & param "duration"        .~ ["short"]
                      -- & param "videoDefinition" .~ ["standard"]
                      & param "order"           .~ ["date"]
                      & param "q"               .~ [query]

  r <- getWith opts "https://www.googleapis.com/youtube/v3/search?"

  let videoIds = r ^.. responseBody . key "items" . values . key "id" . key "videoId" . _String
      resultsCount = length videoIds

  when (resultsCount == 0) (error $ "\n No videos found for:  " ++ unpack query)

  return videoIds
