module Mpv.Data.AudioId where

import Polysemy.Time.Json (json)

newtype AudioId =
  AudioId { unAudioId :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

json ''AudioId
