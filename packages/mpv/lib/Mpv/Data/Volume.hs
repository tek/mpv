module Mpv.Data.Volume where

import Polysemy.Time.Json (json)

newtype Volume =
  Volume { unVolume :: Double }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord, Enum, Real, Fractional)

json ''Volume
