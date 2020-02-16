module YouTube where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.ByteString.Lazy as SL (putStrLn)
import Data.Text as Text          (pack, unpack, Text)
import TextShow                   (showt)
import Control.Monad              (when, unless)
import Data.Monoid                ((<>))
import qualified Turtle as T
import Prelude hiding             (FilePath)

import Util.Misc                  (toTxt, exec, parseInt)
import Types

api_key = ""
api_url = "https://www.googleapis.com/youtube/v3/search?"

download :: T.FilePath -> VideoId -> IO (T.ExitCode, VideoId)
download path videoId = do
  cmdOut <- downloadCmd path videoId
  return (fst cmdOut, videoId)

  where
    downloadCmd :: T.FilePath -> VideoId -> IO (T.ExitCode, Text)
    downloadCmd path videoId = exec $
      "youtube-dl -o "
      <> "'" <> (toTxt path) <> "/%(id)s.%(ext)s" <> "'"
      -- <> " -f 'bestvideo[height<=720]+bestaudio/best[height<=720]' "
      <> " -f 'bestvideo[height<=480]+bestaudio/best[height<=480]' "
      <> " --min-sleep-interval 1 "
      <> " --max-sleep-interval 150 "
      <> " --no-playlist "
      <> " --no-warnings "
      <> " --abort-on-error "
      -- <> " --ignore-errors "
      <> " --no-check-certificate"
      -- <> " --proxy "
      <> " -- " <> (fromId videoId)

-- opts duration query maxPageResults pageToken = defaults
opts query maxPageResults pageToken = defaults
  & param "part"            .~ ["snippet"]
  & param "key"             .~ [api_key]
  & param "maxResults"      .~ [maxPageResults]
  & param "type"            .~ ["video"]
  -- & param "duration"        .~ ["short"]
  -- & param "duration"        .~ ["medium"]
  -- & param "order"           .~ ["date"]
  & param "order"           .~ ["relevance"]
  & param "pageToken"       .~ [pageToken]
  & param "q"               .~ [query]

getWithOpts opts = getWith opts api_url

videoIdsFromResponse r = r ^.. responseBody
  . key "items"
  . values
  . key "id"
  . key "videoId"
  . _String

vidsLiveContFromResponse r = r ^.. responseBody
  . key "items"
  . values
  . key "snippet"
  . key "liveBroadcastContent"
  . _String

nextPageTokenFromResponse r = r ^.. responseBody
  . key "nextPageToken"
  ^? ix 0
  . _String

clampPageSize = showt . min 50

searchYoutube :: Text -> Text -> IO [VideoId]
searchYoutube query maxTotalResultsTxt = do
  maxTotalResults <- parseInt maxTotalResultsTxt
  let maxPageResults = clampPageSize maxTotalResults

  videoIds <- getPaginatedVideoIds query maxTotalResults [] "" maxPageResults

  when (null videoIds) (error $ "\n No videos found for:  " ++ unpack query)

  return $ VideoId <$> videoIds


getPaginatedVideoIds query maxTotalResults prevVideoIds pageToken maxPageResults = do
  resp <- getWithOpts (opts query maxPageResults pageToken)
  let
    remainingCount = maxTotalResults - (length prevVideoIds)
    liveCont       = vidsLiveContFromResponse resp
    vidIds         = videoIdsFromResponse resp
    vidIdsNoLive   = fst <$> filter ((/= "live") . snd) (zip vidIds liveCont)
    newVideoIds    = take remainingCount vidIdsNoLive
    totalVideoIds  = prevVideoIds ++ (take remainingCount newVideoIds)
    nextPageToken  = nextPageTokenFromResponse resp

  case nextPageToken of
    Nothing -> return totalVideoIds
    Just nextPageToken ->
      case null newVideoIds || (length totalVideoIds >= maxTotalResults) of
        True -> return totalVideoIds
        False -> getPaginatedVideoIds query maxTotalResults totalVideoIds nextPageToken maxPageResults
