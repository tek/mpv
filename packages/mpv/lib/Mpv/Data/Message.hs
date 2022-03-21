module Mpv.Data.Message where

import Data.Aeson (Value)
import Data.Aeson.TH (deriveJSON)

import Mpv.Data.MpvEvent (MpvEvent)
import Mpv.Json (untaggedOptions)

data Message =
  ResponseMessage {
    request_id :: Int,
    error :: Text,
    _data :: Maybe Value
  }
  |
  ResponseEvent MpvEvent

deriveJSON untaggedOptions ''Message
