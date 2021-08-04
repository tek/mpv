module Mpv.Data.VideoExpired where

import Polysemy.Time (NanoSeconds)

newtype VideoExpired =
  VideoExpired { unVideoExpired :: NanoSeconds }
  deriving (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Integral, Fractional)

defaultJson ''VideoExpired
