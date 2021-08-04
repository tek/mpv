module Mpv.Data.RequestId where

newtype RequestId =
  RequestId { unRequestId :: Int }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

defaultJson ''RequestId
