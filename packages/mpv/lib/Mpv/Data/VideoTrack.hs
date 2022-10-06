module Mpv.Data.VideoTrack where

import Mpv.Data.VideoId (VideoId)

data VideoTrack =
  VideoTrack {
    id :: Maybe VideoId,
    selected :: Bool
  }
  deriving stock (Eq, Show, Generic)

json ''VideoTrack
