module Mpv.Effect.Ipc where

import Polysemy.Time (TimeUnit)

import Mpv.Data.MpvEvent (EventName)
import Mpv.Data.Property (Property)

data Ipc (fmt :: Type) (command :: Type -> Type) :: Effect where
  Sync :: command a -> Ipc fmt command m a
  Async :: command a -> Ipc fmt command m ()
  WaitEvent :: TimeUnit u => EventName -> u -> m a -> Ipc fmt command m (Maybe Value, a)
  Prop :: Property v -> Ipc fmt command m v
  SetProp :: Show v => Property v -> v -> Ipc fmt command m ()
  SetOption :: Text -> Text -> Ipc fmt command m ()

makeSem ''Ipc
