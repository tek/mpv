module Mpv.Data.AudioTracks where

import Polysemy.Time.Json (json)

import Mpv.Data.AudioId (AudioId)
import Mpv.Data.AudioTrack (AudioTrack)

data AudioTracks =
  AudioTracks {
    active :: Maybe AudioId,
    audios :: [AudioTrack]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

json ''AudioTracks
