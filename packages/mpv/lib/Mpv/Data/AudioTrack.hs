module Mpv.Data.AudioTrack where

import Mpv.Data.AudioId (AudioId)

data AudioTrack =
  AudioTrack {
    id :: Maybe AudioId,
    selected :: Bool,
    language :: Maybe Text
  }
  deriving (Eq, Show, Generic)

defaultJson ''AudioTrack
