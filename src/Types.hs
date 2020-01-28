module Types where

import Data.Text
import Data.Typeable
import qualified Turtle as T


newtype VideoId = VideoId Text deriving (Eq, Read, Show, Monoid, Semigroup)

fromId :: VideoId -> Text
fromId (VideoId vidId) = vidId


data VideoDirs = VideoDirs {
    out    :: T.FilePath
  , src    :: T.FilePath
  , srcMp4 :: T.FilePath
  , tmp    :: T.FilePath
  } deriving (Show, Eq)
