module Mpv.Data.AudioDelay where

import Polysemy.Time (NanoSeconds)

newtype AudioDelay =
  AudioDelay { unAudioDelay :: NanoSeconds }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Ord, Enum, Real, Fractional)

defaultJson ''AudioDelay
