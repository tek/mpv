module Mpv.Data.SubFps where

import Polysemy.Time.Json (json)

newtype SubFps =
  SubFps { unSubFps :: Double }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord, Enum, Real, Fractional)

json ''SubFps
