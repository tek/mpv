module Mpv.Data.SubFps where

newtype SubFps =
  SubFps { unSubFps :: Double }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Ord, Enum, Real, Fractional)

defaultJson ''SubFps
