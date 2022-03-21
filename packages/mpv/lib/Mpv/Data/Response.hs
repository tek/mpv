module Mpv.Data.Response where

import Polysemy.Time.Json (json)

import Mpv.Data.RequestId (RequestId)

newtype ResponseError =
  ResponseError { unResponseError :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

json ''ResponseError

data Response fmt =
  Response {
    requestId :: RequestId,
    payload :: Either ResponseError fmt
  }
  deriving stock (Eq, Show, Generic)
