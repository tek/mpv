module Mpv.Data.Property where

import Mpv.Data.PlaybackState (PlaybackState)
import Mpv.Data.SubFps (SubFps)
import Mpv.Data.Track (TrackList)
import Mpv.Data.VideoDuration (VideoDuration)
import Mpv.Data.VideoExpired (VideoExpired)
import Mpv.Data.VideoProgress (VideoProgress)

data Property :: Type -> Type where
  Custom :: Text -> Property Value
  Duration :: Property VideoDuration
  SubFps :: Property SubFps
  TrackList :: Property TrackList
  PercentPos :: Property VideoProgress
  TimePos :: Property VideoExpired
  Paused :: Property PlaybackState

deriving instance Eq (Property v)
deriving instance Show (Property v)

propertyName :: Property v -> Text
propertyName = \case
  Custom name -> name
  Duration -> "duration"
  SubFps -> "subfps"
  TrackList -> "track-list"
  PercentPos -> "percent-pos"
  TimePos -> "time-pos"
  Paused -> "pause"
