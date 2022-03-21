module Mpv.Data.SubtitleId where

import Polysemy.Time.Json (json)

newtype SubtitleId =
  SubtitleId { unSubtitleId :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

json ''SubtitleId
