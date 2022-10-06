module Mpv.Data.VideoTracks where

import Mpv.Data.VideoId (VideoId)
import Mpv.Data.VideoTrack (VideoTrack)

data VideoTracks =
  VideoTracks {
    active :: Maybe VideoId,
    videos :: [VideoTrack]
  }
  deriving stock (Eq, Show)

json ''VideoTracks
