module Mpv.Data.MpvEvent where

import Data.Aeson (Value, withText)

data EventName =
  FileLoaded
  |
  EndFile
  |
  Custom Text
  deriving (Eq, Show, Generic)

eventNameText :: EventName -> Text
eventNameText = \case
  FileLoaded -> "file-loaded"
  EndFile -> "end-file"
  Custom t -> t

instance FromJSON EventName where
  parseJSON =
    withText "EventName" \case
      "file-loaded" -> pure FileLoaded
      "end-file" -> pure EndFile
      t -> pure (Custom t)

instance ToJSON EventName where
  toJSON =
    toJSON . eventNameText

data MpvEvent =
  MpvEvent {
    event :: EventName,
    payload :: Value
  }
  deriving (Eq, Show)

defaultJson ''MpvEvent
