module Mpv.Data.AudioTrack where

import Polysemy.Time.Json (json)

import Mpv.Data.AudioId (AudioId)

data AudioTrack =
  AudioTrack {
    id :: Maybe AudioId,
    selected :: Bool,
    language :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

json ''AudioTrack
