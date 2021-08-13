module Mpv.Effect.Ipc where

import Polysemy.Time (TimeUnit)

import Mpv.Data.MpvEvent (EventName)

data Ipc (fmt :: Type) (command :: Type -> Type) :: Effect where
  Sync :: command a -> Ipc fmt command m a
  WaitEvent :: TimeUnit u => EventName -> u -> m a -> Ipc fmt command m (Maybe Value, a)

makeSem ''Ipc
