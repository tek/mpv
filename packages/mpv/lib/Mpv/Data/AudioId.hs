module Mpv.Data.AudioId where

newtype AudioId =
  AudioId { unAudioId :: Int }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

defaultJson ''AudioId
