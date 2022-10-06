module Mpv.Data.VideoProgress where

newtype VideoProgress =
  VideoProgress { unVideoProgress :: Double }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

json ''VideoProgress
