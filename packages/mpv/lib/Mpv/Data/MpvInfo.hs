module Mpv.Data.MpvInfo where

import Mpv.Data.AudioDelay (AudioDelay)
import Mpv.Data.AudioTracks (AudioTracks)
import Mpv.Data.PlaybackState (PlaybackState)
import Mpv.Data.SubDelay (SubDelay)
import Mpv.Data.Subtitles (Subtitles)
import Mpv.Data.VideoDuration (VideoDuration)
import Mpv.Data.VideoExpired (VideoExpired)
import Mpv.Data.VideoProgress (VideoProgress)
import Mpv.Data.Volume (Volume)

data MpvInfo =
  MpvInfo {
    _playback :: PlaybackState,
    _duration :: VideoDuration,
    _progress :: VideoProgress,
    _expired :: VideoExpired,
    _subtitles :: Subtitles,
    _subDelay :: SubDelay,
    _audio :: AudioTracks,
    _audioDelay :: AudioDelay,
    _volume :: Volume
  }
  deriving stock (Eq, Show, Generic)

json ''MpvInfo
