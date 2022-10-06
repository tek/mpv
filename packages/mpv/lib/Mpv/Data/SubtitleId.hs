module Mpv.Data.SubtitleId where

newtype SubtitleId =
  SubtitleId { unSubtitleId :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

json ''SubtitleId
