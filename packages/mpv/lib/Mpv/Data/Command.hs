module Mpv.Data.Command where

import Path (Abs, File, Path)
import Prelude hiding (All, Compose, Stop)

import Mpv.Data.EventName (EventName)
import Mpv.Data.OsdLevel (OsdLevel)
import Mpv.Data.Property (Property)
import Mpv.Data.SeekFlags (SeekFlags)
import Polysemy.Time (TimeUnit)

data CycleDirection =
  Up
  |
  Down
  deriving (Eq, Show)

lowerMinusJson ''CycleDirection

data LoadResponse =
  LoadResponse {
    playlist_entry_id :: Int
  }
  deriving (Eq, Show)

deriveJSON basicOptions ''LoadResponse

data LoadOption =
  Replace
  |
  Append
  |
  AppendPlay
  deriving (Eq, Show)

lowerMinusJson ''LoadOption

instance Default LoadOption where
  def =
    Replace

data EmptyResponse =
  EmptyResponse
  deriving (Eq, Show)

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

deriving instance Show (Command a)
