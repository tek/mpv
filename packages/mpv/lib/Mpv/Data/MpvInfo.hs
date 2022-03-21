module Mpv.Data.MpvInfo where

import Control.Lens (makeClassy)
import Polysemy.Time.Json (json)

import Mpv.Data.AudioDelay (AudioDelay)
import Mpv.Data.AudioTracks (AudioTracks)
import Mpv.Data.PlaybackState (PlaybackState)
import Mpv.Data.SubDelay (SubDelay)
import Mpv.Data.Subtitles (Subtitles)
import Mpv.Data.VideoDuration (VideoDuration)
import Mpv.Data.VideoExpired (VideoExpired)
import Mpv.Data.VideoProgress (VideoProgress)

data MpvInfo =
  MpvInfo {
    _playback :: PlaybackState,
    _duration :: VideoDuration,
    _progress :: VideoProgress,
    _expired :: VideoExpired,
    _subtitles :: Subtitles,
    _subDelay :: SubDelay,
    _audio :: AudioTracks,
    _audioDelay :: AudioDelay
  }
  deriving stock (Eq, Show, Generic)

json ''MpvInfo
makeClassy ''MpvInfo
