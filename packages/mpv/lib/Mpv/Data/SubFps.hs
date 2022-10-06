module Mpv.Data.SubFps where

newtype SubFps =
  SubFps { unSubFps :: Double }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Ord, Enum, Real, Fractional)

json ''SubFps
