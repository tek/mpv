module Mpv.Data.RequestId where

import Polysemy.Time.Json (json)

newtype RequestId =
  RequestId { unRequestId :: Int }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord)

json ''RequestId
