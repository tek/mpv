module Mpv.Data.AudioTracks where

import Mpv.Data.AudioId (AudioId)
import Mpv.Data.AudioTrack (AudioTrack)

data AudioTracks =
  AudioTracks {
    active :: Maybe AudioId,
    audios :: [AudioTrack]
  }
  deriving (Eq, Show, Generic, Default)

defaultJson ''AudioTracks
