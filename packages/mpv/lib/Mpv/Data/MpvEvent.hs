module Mpv.Data.MpvEvent where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Some (Some (Some))
import Polysemy.Conc (ChanConsumer)

import qualified Mpv.Data.Event as Event
import Mpv.Data.Event (Event)
import qualified Mpv.Data.EventName as EventName
import Mpv.Data.EventName (EventName)

data MpvEvent =
  MpvEvent {
    name :: EventName,
    payload :: Some Event
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON MpvEvent where
  parseJSON value =
    withObject "MpvEvent" parse value
    where
      parse o = do
        name <- o .: "event"
        pl <- payload name
        pure (MpvEvent name pl)
      payload = \case
        EventName.FileLoaded ->
          pure (Some Event.FileLoaded)
        EventName.EndFile ->
          Some . Event.EndFile <$> parseJSON value
        EventName.Pause ->
          pure (Some Event.Pause)
        EventName.Other _ ->
          pure (Some (Event.Unknown value))
        EventName.Unknown ->
          pure (Some (Event.Unknown value))

instance ToJSON MpvEvent where

type MpvEventConsumer =
  ChanConsumer MpvEvent
