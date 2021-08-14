module Mpv.Data.Message where

import Mpv.Data.MpvEvent (MpvEvent)

data Message =
  ResponseMessage {
    request_id :: Int,
    error :: Text,
    _data :: Maybe Value
  }
  |
  ResponseEvent MpvEvent

deriveJSON untaggedOptions ''Message
