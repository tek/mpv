module Mpv.Data.Response where

import Mpv.Data.RequestId (RequestId)

newtype ResponseError =
  ResponseError { unResponseError :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (IsString)

defaultJson ''ResponseError

data Response fmt =
  Response {
    requestId :: RequestId,
    payload :: Either ResponseError fmt
  }
  deriving (Eq, Show, Generic)
