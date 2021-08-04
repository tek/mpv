module Mpv.Data.VideoTrack where

import Mpv.Data.VideoId (VideoId)

data VideoTrack =
  VideoTrack {
    id :: Maybe VideoId,
    selected :: Bool
  }
  deriving (Eq, Show, Generic)

defaultJson ''VideoTrack
