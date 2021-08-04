module Mpv.Data.SubtitleId where

newtype SubtitleId =
  SubtitleId { unSubtitleId :: Int }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

defaultJson ''SubtitleId
