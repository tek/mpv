module Mpv.Data.MpvResources where

import Mpv.Data.RequestId (RequestId)
import Mpv.Data.Response (ResponseError)

data InMessage fmt =
  InMessage { unInMessage :: fmt }
  |
  InMessageError { error :: Text }
  deriving stock (Eq, Show, Generic)

newtype OutMessage fmt =
  OutMessage { unOutMessage :: fmt }
  deriving stock (Eq, Show, Generic)

data Requests fmt =
  Requests {
    nextId :: RequestId,
    pending :: Map RequestId (MVar (Either ResponseError fmt))
  }
