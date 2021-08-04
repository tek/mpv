module Mpv.Data.Message where

import Data.Aeson (Value)

import Mpv.Data.MpvEvent (EventName)

data Message =
  ResponseMessage {
    request_id :: Int,
    error :: Text,
    _data :: Maybe Value
  }
  |
  ResponseEvent {
    event :: EventName
  }

deriveJSON untaggedOptions ''Message
