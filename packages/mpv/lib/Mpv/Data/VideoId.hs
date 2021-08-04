module Mpv.Data.VideoId where

newtype VideoId =
  VideoId { unVideoId :: Int }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

defaultJson ''VideoId
