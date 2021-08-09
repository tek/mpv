module Mpv.Data.MpvResources where

import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Network.Socket (Socket)

import Mpv.Data.RequestId (RequestId)
import Mpv.Data.Response (ResponseError)

data InMessage fmt =
  InMessage { unInMessage :: fmt }
  |
  InMessageError { error :: Text }
  deriving (Eq, Show, Generic)

newtype OutMessage fmt =
  OutMessage { unOutMessage :: fmt }
  deriving (Eq, Show, Generic)

data Requests fmt =
  Requests {
    nextId :: RequestId,
    pending :: Map RequestId (MVar (Either ResponseError fmt))
  }
  deriving stock (Eq)

data MpvResources fmt =
  MpvResources {
    socket :: Socket,
    outQueue :: TBMQueue (OutMessage fmt),
    inQueue :: TBMQueue (InMessage fmt),
    requests :: TVar (Requests fmt)
  }
