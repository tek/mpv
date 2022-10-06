module Mpv.Data.Volume where

newtype Volume =
  Volume { unVolume :: Double }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord, Enum, Real, Fractional)

json ''Volume
