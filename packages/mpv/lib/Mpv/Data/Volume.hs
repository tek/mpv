module Mpv.Data.Volume where

newtype Volume =
  Volume { unVolume :: Double }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Ord, Enum, Real, Fractional)

defaultJson ''Volume
