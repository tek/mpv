module Mpv.Data.Command where

import Data.Aeson.Types (Value, listValue)
import Data.SOP (All, Compose, I, K (K), NP, hcmap, hcollapse, unI)
import Path (Abs, File, Path)
import Prelude hiding (All, Compose, Stop)

import Mpv.Class.CommandEvent (CommandEvent (..))
import Mpv.Data.MpvEvent (EventName (EndFile, FileLoaded))
import Mpv.Data.SeekFlags (SeekFlags)

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

data Command :: Type -> Type where
  Manual :: (All ToJSON as, All (Compose Show I) as, FromJSON a) => Maybe EventName -> NP I as -> Command a
  Load :: Path Abs File -> Command Value
  Stop :: Command Value
  Seek :: Double -> SeekFlags -> Command Value

deriving instance Show (Command a)

instance CommandEvent Command where
  commandEvent = \case
    Manual event _ -> event
    Load _ -> Just FileLoaded
    Stop -> Just EndFile
    Seek _ _ -> Nothing
