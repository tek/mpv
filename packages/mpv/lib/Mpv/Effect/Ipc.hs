module Mpv.Effect.Ipc where

import Data.Aeson (Value)
import Polysemy.Time (TimeUnit)

import Mpv.Data.MpvEvent (EventName)

data Ipc (command :: Type -> Type) :: Effect where
  Sync :: command a -> Ipc command m a
  Async :: command a -> Ipc command m ()
  WaitEvent :: TimeUnit u => EventName -> u -> m a -> Ipc command m (Maybe Value, a)

makeSem ''Ipc
