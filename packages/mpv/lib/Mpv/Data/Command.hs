module Mpv.Data.Command where

import Data.Aeson (FromJSON (parseJSON), Value)
import Data.Aeson.TH (deriveJSON)
import Path (Abs, File, Path)

import Mpv.Data.EventName (EventName)
import Mpv.Data.OsdLevel (OsdLevel)
import Mpv.Data.Property (Property)
import Mpv.Data.SeekFlags (SeekFlags)
import Mpv.Json (basicOptions, lowerMinusJson)

data CycleDirection =
  Up
  |
  Down
  deriving stock (Eq, Show)

lowerMinusJson ''CycleDirection

data LoadResponse =
  LoadResponse {
    playlist_entry_id :: Int
  }
  deriving stock (Eq, Show)

deriveJSON basicOptions ''LoadResponse

data LoadOption =
  Replace
  |
  Append
  |
  AppendPlay
  deriving stock (Eq, Show)

lowerMinusJson ''LoadOption

instance Default LoadOption where
  def =
    Replace

data EmptyResponse =
  EmptyResponse
  deriving stock (Eq, Show)

instance FromJSON EmptyResponse where
  parseJSON =
    const (pure EmptyResponse)

data Command :: Type -> Type where
  Manual :: Maybe EventName -> Text -> [Value] -> Command Value
  Load :: Path Abs File -> Maybe LoadOption -> Command LoadResponse
  Stop :: Command EmptyResponse
  Seek :: Double -> SeekFlags -> Command EmptyResponse
  Prop :: Property v -> Command v
  SetProp :: Show v => Property v -> v -> Command ()
  AddProp :: Show v => Property v -> Maybe v -> Command ()
  CycleProp :: Show v => Property v -> Maybe CycleDirection -> Command ()
  MultiplyProp :: Show v => Property v -> v -> Command ()
  SetOption :: Text -> Text -> Command ()
  ShowText :: (TimeUnit u, Show u) => Text -> u -> OsdLevel -> Command ()
  ShowProgress :: Command ()

deriving stock instance Show (Command a)
