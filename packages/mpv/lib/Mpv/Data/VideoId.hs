module Mpv.Data.VideoId where

newtype VideoId =
  VideoId { unVideoId :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

json ''VideoId
