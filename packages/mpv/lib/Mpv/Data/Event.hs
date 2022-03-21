module Mpv.Data.Event where

import Data.Aeson (ToJSON (toJSON), Value (Null))
import Data.GADT.Compare.TH (deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Some (Some, withSome)

import qualified Mpv.Data.EventName as EventName
import Mpv.Data.EventName (EventName)
import qualified Mpv.Data.EventPayload as Payload

data Event (name :: EventName) where
  EndFile :: Payload.EndFile -> Event 'EventName.EndFile
  FileLoaded :: Event 'EventName.FileLoaded
  Pause :: Event 'EventName.Pause
  Unknown :: Value -> Event 'EventName.Unknown

deriveGEq ''Event
deriveGShow ''Event

instance ToJSON (Some Event) where
  toJSON e =
    withSome e \case
      EndFile p -> toJSON p
      FileLoaded -> Null
      Pause -> Null
      Unknown p -> p
