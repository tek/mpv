module Mpv.Data.PlaybackState where

data PlaybackState =
  Playing
  |
  Paused
  deriving (Eq, Show, Generic)

defaultJson ''PlaybackState

fromBool :: Bool -> PlaybackState
fromBool s =
  if s then Paused else Playing

toBool :: PlaybackState -> Bool
toBool = \case
  Playing -> False
  Paused -> True

toggle :: PlaybackState -> PlaybackState
toggle = \case
  Playing -> Paused
  Paused -> Playing
