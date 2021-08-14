module Mpv.Data.PlayerError where

data PlayerError =
  PlayerError Text
  |
  Fatal Text
  |
  NothingPlaying
  deriving (Eq, Show)

defaultJson ''PlayerError
