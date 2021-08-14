module Mpv.Data.EventName where

import Data.Aeson (withText)

data EventName =
  FileLoaded
  |
  EndFile
  |
  Pause
  |
  Unknown
  |
  Other Text
  deriving (Eq, Show, Generic)

eventNameText :: EventName -> Text
eventNameText = \case
  FileLoaded -> "file-loaded"
  EndFile -> "end-file"
  Pause -> "pause"
  Unknown -> "unknown"
  Other t -> t

instance FromJSON EventName where
  parseJSON =
    withText "EventName" \case
      "file-loaded" -> pure FileLoaded
      "end-file" -> pure EndFile
      "pause" -> pure Pause
      t -> pure (Other t)

instance ToJSON EventName where
  toJSON =
    toJSON . eventNameText
