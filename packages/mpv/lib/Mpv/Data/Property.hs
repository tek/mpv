module Mpv.Data.Property where

import Data.Aeson (Value)

import Mpv.Data.AudioDelay (AudioDelay)
import Mpv.Data.PlaybackState (PlaybackState)
import Mpv.Data.SubDelay (SubDelay)
import Mpv.Data.SubFps (SubFps)
import Mpv.Data.Track (TrackList)
import Mpv.Data.VideoDuration (VideoDuration)
import Mpv.Data.VideoExpired (VideoExpired)
import Mpv.Data.VideoProgress (VideoProgress)
import Mpv.Data.Volume (Volume)

data Property :: Type -> Type where
  Custom :: Text -> Property Value
  Duration :: Property VideoDuration
  SubFps :: Property SubFps
  SubDelay :: Property SubDelay
  AudioDelay :: Property AudioDelay
  TrackList :: Property TrackList
  PercentPos :: Property VideoProgress
  TimePos :: Property VideoExpired
  Paused :: Property PlaybackState
  Volume :: Property Volume

deriving stock instance Eq (Property v)
deriving stock instance Show (Property v)

propertyName :: Property v -> Text
propertyName = \case
  Custom name -> name
  Duration -> "duration"
  SubFps -> "sub-fps"
  SubDelay -> "sub-delay"
  AudioDelay -> "audio-delay"
  TrackList -> "track-list"
  PercentPos -> "percent-pos"
  TimePos -> "time-pos"
  Paused -> "pause"
  Volume -> "volume"
