module Mpv.Data.MpvResources where

import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Data.Aeson (Value)
import Network.Socket (Socket)

import Mpv.Data.RequestId (RequestId)
import Mpv.Data.Response (ResponseError)

newtype InMessage =
  InMessage { unInMessage :: ByteString }
  deriving (Eq, Show, Generic)

newtype OutMessage =
  OutMessage { unOutMessage :: ByteString }
  deriving (Eq, Show, Generic)

data Requests =
  Requests {
    nextId :: RequestId,
    pending :: Map RequestId (MVar (Either ResponseError Value))
  }
  deriving stock (Eq)

data MpvResources =
  MpvResources {
      socket :: Socket,
      outQueue :: TBMQueue OutMessage,
      inQueue :: TBMQueue InMessage,
      requests :: TVar Requests
    }
