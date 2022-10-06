module Mpv.Data.AudioId where

newtype AudioId =
  AudioId { unAudioId :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

json ''AudioId
