module YouTubeDownloader where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import Data.Map as Map

import Data.ByteString.Lazy as SL (putStrLn)
import Data.Text as Text (pack, unpack, Text)

import System.Process

type Resp = Response (Map String Value)

--anti-bot
api_key = mconcat ["AIzaSyCCYfqHdQPxyhb","NAPlUeSecvBnoQK0kQhk"]

download path videoId = callCommand $ mconcat [
   "youtube-dl -o "
   , "'", path, "/%(id)s.%(ext)s", "'"
   -- , " --simulate "
   , " -f 'bestvideo[height<=480]+bestaudio/best[height<=480]' "
   , " --min-sleep-interval 1 "
   , " --max-sleep-interval 20 "
   , " --no-warnings "
   , " -- ", videoId]


searchYoutube :: Text -> Text -> String -> IO ()
searchYoutube query maxResults path = do
   let opts = defaults & param "part"            .~ ["snippet"]
                       & param "key"             .~ [api_key]
                       & param "maxResults"      .~ [maxResults]  --max:50
                       & param "type"            .~ ["video"]
                       & param "duration"        .~ ["short"]
                       & param "videoDefinition" .~ ["standard"]
                       & param "order"           .~ ["date"]
                       & param "q"               .~ [query]

   r <- getWith opts "https://www.googleapis.com/youtube/v3/search?"

   let videoIds = r ^.. responseBody . key "items" . values . key "id" . key "videoId" . _String
   print $ videoIds

   mapM_ ((download path) . Text.unpack) videoIds
