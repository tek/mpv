module Mpv.Data.VideoTrack where

import Polysemy.Time.Json (json)

import Mpv.Data.VideoId (VideoId)

data VideoTrack =
  VideoTrack {
    id :: Maybe VideoId,
    selected :: Bool
  }
  deriving stock (Eq, Show, Generic)

json ''VideoTrack
