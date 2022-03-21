module Mpv.Data.Request where

import Data.Aeson (Value)
import Polysemy.Time.Json (json)

import Mpv.Data.RequestId (RequestId)

data Request =
  Request {
    request_id :: RequestId,
    command :: [Value],
    async :: Bool
  }
  deriving stock (Eq, Show, Generic)

json ''Request
