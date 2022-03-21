module Mpv.Data.VideoProgress where

import Polysemy.Time.Json (json)

newtype VideoProgress =
  VideoProgress { unVideoProgress :: Double }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord, Enum, Real, Fractional, RealFrac)

json ''VideoProgress
