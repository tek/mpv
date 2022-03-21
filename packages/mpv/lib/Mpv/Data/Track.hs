module Mpv.Data.Track where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Exon (exon)
import Polysemy.Time.Json (json)

data TrackType =
  Audio
  |
  Sub
  |
  Video
  deriving stock (Eq, Show, Enum, Ord)

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
    lang :: Maybe Text,
    _type :: TrackType
  }
  deriving stock (Eq, Show, Generic, Ord)

json ''Track

newtype TrackList =
  TrackList { unTrackList :: NonEmpty Track }
  deriving stock (Eq, Show, Generic)

json ''TrackList
