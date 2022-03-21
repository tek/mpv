module Mpv.Data.AudioDelay where

import Polysemy.Time (NanoSeconds)
import Polysemy.Time.Json (json)

newtype AudioDelay =
  AudioDelay { unAudioDelay :: NanoSeconds }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord, Enum, Real, Fractional)

json ''AudioDelay
