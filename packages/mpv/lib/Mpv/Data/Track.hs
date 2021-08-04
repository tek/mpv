module Mpv.Data.Track where

data TrackType =
  Audio
  |
  Sub
  |
  Video
  deriving (Eq, Show)

defaultJson ''TrackType

data Track =
  Track {
    id :: Maybe Int,
    selected :: Bool,
    language :: Maybe Text,
    trackType :: TrackType
  }
  deriving (Eq, Show, Generic)

defaultJson ''Track
