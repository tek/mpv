module Mpv.Data.Request where

import Prelude hiding (All, Compose)

import Mpv.Data.RequestId (RequestId)

data Request =
  Request {
    request_id :: RequestId,
    command :: [Value],
    async :: Bool
  }
  deriving (Eq, Show, Generic)

defaultJson ''Request
