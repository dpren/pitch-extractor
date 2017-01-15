module YouTubeDownloader where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import Data.Map as Map
import Data.ByteString.Lazy as SL (putStrLn)
import Data.Text as Text          (pack, unpack, Text)
import Control.Monad              (when, unless)
import Data.Monoid                ((<>))
import System.Process

import qualified Turtle as T
import Prelude hiding             (FilePath)
import Utils.Misc                 (toTxt, exec)

type Resp = Response (Map String Value)

api_key = "AIzaSyCCYfqHdQPxyhbNAPlUeSecvBnoQK0kQhk"

download :: T.MonadIO io => T.FilePath -> Text -> io (T.ExitCode, Text)
download path videoId = exec $
  "youtube-dl -o "
  <> "'" <> (toTxt path) <> "/%(id)s.%(ext)s" <> "'"
  <> " -f 'bestvideo[height<=480]+bestaudio/best[height<=480]' "
  <> " --min-sleep-interval 1 "
  <> " --max-sleep-interval 700 "
  <> " --no-warnings "
  <> " -- " <> videoId


searchYoutube :: Text -> Text -> T.FilePath -> IO ()
searchYoutube query maxResults path = do
  let opts = defaults & param "part"            .~ ["snippet"]
                      & param "key"             .~ [api_key]
                      & param "maxResults"      .~ [maxResults]
                      & param "type"            .~ ["video"]
                      & param "duration"        .~ ["short"]
                      -- & param "videoDefinition" .~ ["standard"]
                      & param "order"           .~ ["date"]
                      & param "q"               .~ ["roland keyboard|korg keyboard"]

  r <- getWith opts "https://www.googleapis.com/youtube/v3/search?"

  let videoIds = r ^.. responseBody . key "items" . values . key "id" . key "videoId" . _String

  let resultsCount = (r ^.. responseBody . key "pageInfo" . key "totalResults" . _Integer) !! 0

  print videoIds

  when (resultsCount == 0) (error $ "\n No videos found for:  " ++ (unpack query))

  mapM_ (download path) videoIds
