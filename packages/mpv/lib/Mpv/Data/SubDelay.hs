module Mpv.Data.SubDelay where

import Polysemy.Time (NanoSeconds)

newtype SubDelay =
  SubDelay { unSubDelay :: NanoSeconds }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Ord, Enum, Real, Fractional)

defaultJson ''SubDelay
