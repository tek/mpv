module Mpv.Data.SubDelay where

import Polysemy.Time (NanoSeconds)

newtype SubDelay =
  SubDelay { unSubDelay :: NanoSeconds }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord, Enum, Real, Fractional)

json ''SubDelay
