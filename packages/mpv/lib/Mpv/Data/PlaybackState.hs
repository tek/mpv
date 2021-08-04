module Mpv.Data.PlaybackState where

data PlaybackState =
  Playing
  |
  Paused
  deriving (Eq, Show)

defaultJson ''PlaybackState
