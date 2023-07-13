module Mpv.Data.EventPayload where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Prelude hiding (Stop)

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
  deriving stock (Eq, Show)

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

data FileError =
  FileError Text
  deriving stock (Eq, Show, Generic)

instance FromJSON FileError where
  parseJSON v =
    FileError . fromMaybe (show v) <$> parseJSON v

instance ToJSON FileError where
  toJSON (FileError err) = toJSON err

data EndFile =
  EndFile {
    playlist_entry_id :: Int,
    reason :: EndReason,
    file_error :: Maybe FileError
  }
  deriving stock (Eq, Show)

json ''EndFile

data FileLoaded =
  FileLoaded
  deriving stock (Eq, Show)

json ''FileLoaded

data Pause =
  Pause
  deriving stock (Eq, Show)

json ''Pause
