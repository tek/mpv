module Mpv.Data.VideoProgress where

newtype VideoProgress =
  VideoProgress { unVideoProgress :: Double }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

defaultJson ''VideoProgress
