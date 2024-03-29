module Mpv.Data.VideoDuration where

import Polysemy.Time (NanoSeconds)

newtype VideoDuration =
  VideoDuration { unVideoDuration :: NanoSeconds }
  deriving stock (Eq, Show)
  deriving newtype (Num, Ord, Enum, Real, Integral, Fractional)

json ''VideoDuration
