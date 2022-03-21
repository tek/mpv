module Mpv.Data.VideoExpired where

import Polysemy.Time (NanoSeconds)
import Polysemy.Time.Json (json)

newtype VideoExpired =
  VideoExpired { unVideoExpired :: NanoSeconds }
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Integral, Fractional)

json ''VideoExpired
