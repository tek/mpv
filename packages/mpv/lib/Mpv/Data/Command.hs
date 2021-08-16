module Mpv.Data.Command where

import Data.Aeson.Types (listValue)
import Data.SOP (All, Compose, I, K (K), NP, hcmap, hcollapse, unI)
import Path (Abs, File, Path)
import Prelude hiding (All, Compose, Stop)

import Mpv.Class.CommandEvent (CommandEvent (..))
import Mpv.Data.EventName (EventName (EndFile, FileLoaded))
import Mpv.Data.Property (Property)
import Mpv.Data.SeekFlags (SeekFlags)

data CycleDirection =
  Up
  |
  Down
  deriving (Eq, Show)

lowerMinusJson ''CycleDirection 

newtype CommandArgs (as :: [Type]) =
  CommandArgs { unCommandArgs :: NP I as }

deriving instance All (Compose Eq I) as => Eq (CommandArgs as)
deriving instance All (Compose Show I) as => Show (CommandArgs as)

instance All ToJSON as => ToJSON (CommandArgs as) where
  toJSON =
    listValue id .
    hcollapse .
    hcmap (Proxy @ToJSON) (K .  toJSON .  unI) .
    unCommandArgs

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
  Manual :: (All ToJSON as, All (Compose Show I) as) => Maybe EventName -> Text -> NP I as -> Command Value
  Load :: Path Abs File -> Maybe LoadOption -> Command LoadResponse
  Stop :: Command EmptyResponse
  Seek :: Double -> SeekFlags -> Command EmptyResponse
  Prop :: Property v -> Command v
  SetProp :: Show v => Property v -> v -> Command ()
  AddProp :: Show v => Property v -> Maybe v -> Command ()
  CycleProp :: Show v => Property v -> Maybe CycleDirection -> Command ()
  MultiplyProp :: Show v => Property v -> v -> Command ()
  SetOption :: Text -> Text -> Command ()

deriving instance Show (Command a)

instance CommandEvent Command where
  commandEvent = \case
    Manual event _ _ -> event
    Load _ _ -> Just FileLoaded
    Stop -> Just EndFile
    Seek _ _ -> Nothing
    Prop _ -> Nothing
    SetProp _ _ -> Nothing
    AddProp _ _ -> Nothing
    CycleProp _ _ -> Nothing
    MultiplyProp _ _ -> Nothing
    SetOption _ _ -> Nothing
