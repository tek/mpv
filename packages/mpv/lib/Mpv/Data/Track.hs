module Mpv.Data.Track where

import Data.Aeson (withText)

data TrackType =
  Audio
  |
  Sub
  |
  Video
  deriving (Eq, Show, Enum, Ord)

instance FromJSON TrackType where
  parseJSON =
    withText "TrackType" \case
      "audio" -> pure Audio
      "sub" -> pure Sub
      "video" -> pure Video
      other -> fail [exon|Unknown track type `#{toString other}`|]

trackTypeText :: TrackType -> Text
trackTypeText = \case
  Audio -> "audio"
  Sub -> "sub"
  Video -> "video"

instance ToJSON TrackType where
  toJSON =
    toJSON . trackTypeText

data Track =
  Track {
    id :: Maybe Int,
    selected :: Bool,
    language :: Maybe Text,
    _type :: TrackType
  }
  deriving (Eq, Show, Generic, Ord)

defaultJson ''Track

newtype TrackList =
  TrackList { unTrackList :: NonEmpty Track }
  deriving (Eq, Show, Generic)

defaultJson ''TrackList
