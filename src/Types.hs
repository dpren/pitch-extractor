module Types where

import Data.Text
import Data.Typeable

newtype VideoId = VideoId Text deriving (Eq, Read, Show, Monoid)

fromId :: VideoId -> Text
fromId (VideoId vidId) = vidId
