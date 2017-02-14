module Types where

import Data.Text
import Data.Typeable
-- import Network.Wreq
-- import Data.Map as Map
-- import Data.Aeson
--
-- type Resp = Response (Map String Value)

newtype VideoId = VideoId Text deriving (Eq, Read, Show, Monoid)

fromId :: VideoId -> Text
fromId (VideoId vidId) = vidId
