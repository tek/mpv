module Mpv.Data.EventPayload where

import Prelude hiding (Stop)
import Data.Aeson (withText)

data EndReason =
  Quit
  |
  Stop
  |
  Eof
  |
  Error
  |
  Redirect
  |
  Unknown
  deriving (Eq, Show)

endReasonFromText :: Text -> EndReason
endReasonFromText = \case
  "quit" -> Quit
  "stop" -> Stop
  "eof" -> Eof
  "error" -> Error
  "redirect" -> Redirect
  _ -> Unknown

endReasonText :: EndReason -> Text
endReasonText = \case
  Quit -> "quit"
  Stop -> "stop"
  Eof -> "eof"
  Error -> "error"
  Redirect -> "redirect"
  Unknown -> "unknown"

instance FromJSON EndReason where
  parseJSON =
    withText "EndReason" (pure . endReasonFromText)

instance ToJSON EndReason where
  toJSON =
    toJSON . endReasonText

data EndFile =
  EndFile {
    playlist_entry_id :: Int,
    reason :: EndReason
  }
  deriving (Eq, Show)

defaultJson ''EndFile

data FileLoaded =
  FileLoaded
  deriving (Eq, Show)

defaultJson ''FileLoaded

data Pause =
  Pause
  deriving (Eq, Show)

defaultJson ''Pause
