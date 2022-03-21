module Mpv.Data.PlaybackState where

import Polysemy.Time.Json (json)

data PlaybackState =
  Playing
  |
  Paused
  deriving stock (Eq, Show, Generic)

json ''PlaybackState

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
