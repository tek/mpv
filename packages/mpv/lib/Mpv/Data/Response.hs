module Mpv.Data.Response where

import Data.Aeson (Value)
import Mpv.Data.RequestId (RequestId)

newtype ResponseError =
  ResponseError { unResponseError :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (IsString)

defaultJson ''ResponseError

data Response =
  Response {
    requestId :: RequestId,
    payload :: Either ResponseError Value
  }
  deriving (Eq, Show, Generic)
