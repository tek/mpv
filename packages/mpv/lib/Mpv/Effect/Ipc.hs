module Mpv.Effect.Ipc where

import Conc (Scoped_, scoped_)
import Data.Some (Some)

import Mpv.Data.Event (Event)
import Mpv.Data.EventName (EventName)
import Mpv.Data.MpvError (MpvError)

data Ipc (fmt :: Type) (command :: Type -> Type) :: Effect where
  Sync :: command a -> Ipc fmt command m a
  WaitEvent :: TimeUnit u => EventName -> u -> m a -> Ipc fmt command m (Maybe (Some Event), a)

makeSem ''Ipc

withIpc ::
  ∀ resource fmt command r .
  Members [Scoped_ resource (Ipc fmt command !! MpvError) !! MpvError, Stop MpvError] r =>
  InterpreterFor (Ipc fmt command !! MpvError) r
withIpc =
  restop . scoped_ . raiseUnder
