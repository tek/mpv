module Mpv.Effect.Ipc where

import Data.Some (Some)
import Polysemy.Time (TimeUnit)

import Mpv.Data.Event (Event)
import Mpv.Data.EventName (EventName)

data Ipc (fmt :: Type) (command :: Type -> Type) :: Effect where
  Sync :: command a -> Ipc fmt command m a
  WaitEvent :: TimeUnit u => EventName -> u -> m a -> Ipc fmt command m (Maybe (Some Event), a)

makeSem ''Ipc
