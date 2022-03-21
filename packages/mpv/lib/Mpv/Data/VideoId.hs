module Mpv.Data.VideoId where

import Polysemy.Time.Json (json)

newtype VideoId =
  VideoId { unVideoId :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

json ''VideoId
