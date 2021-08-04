module Mpv.Interpreter.Mpv where

import Data.Aeson (Value)
import Polysemy.Conc (Race, interpretEventsChan)
import Polysemy.Conc.Effect.Events (Consume, EventToken)
import Polysemy.Conc.Effect.Scoped (Scoped, runScoped, scoped)
import Polysemy.Log (Log)
import Polysemy.Time (Time, TimeUnit)

import Mpv.Class.CommandEvent (CommandEvent (commandEvent))
import Mpv.Data.Command (Command)
import Mpv.Data.MpvError (MpvError)
import Mpv.Data.MpvEvent (MpvEvent)
import Mpv.Data.MpvResources (MpvResources)
import qualified Mpv.Effect.Ipc as Ipc
import Mpv.Effect.Ipc (Ipc)
import qualified Mpv.Effect.Mpv as Mpv
import Mpv.Effect.Mpv (Mpv)
import Mpv.Interpreter.Ipc (interpretIpc)
import Mpv.MpvResources (withIpc)

waitEventCmd ::
  ∀ u command b r a .
  TimeUnit u =>
  CommandEvent command =>
  Members [Ipc command, Stop MpvError] r =>
  u ->
  command b ->
  Sem r a ->
  Sem r (Maybe Value, a)
waitEventCmd wait (commandEvent -> Just event) ma =
  Ipc.waitEvent event wait ma
waitEventCmd _ _ ma =
  (Nothing,) <$> ma

interpretMpvProcess ::
  CommandEvent command =>
  Member (Ipc command !! MpvError) r =>
  InterpreterFor (Mpv command !! MpvError) r
interpretMpvProcess =
  interpretResumable \case
    Mpv.CommandSync wait cmd ->
      fmap snd $ restop $ waitEventCmd wait cmd do
        Ipc.sync cmd
    Mpv.CommandAsync cmd ->
        restop (Ipc.async cmd)

interpretMpvIpc ::
  Members [Scoped (EventToken token) (Consume MpvEvent), Resource, Async, Race, Log, Embed IO, Final IO] r =>
  Either MpvError MpvResources ->
  InterpreterFor (Mpv Command !! MpvError) r
interpretMpvIpc = \case
  Right res ->
    interpretIpc res . interpretMpvProcess . raiseUnder
  Left err ->
    interpretResumableH \ _ -> stop err

interpretMpvNative ::
  Members [Resource, Async, Race, Log, Time t d, Embed IO, Final IO] r =>
  InterpreterFor (Scoped (Either MpvError MpvResources) (Mpv Command !! MpvError)) r
interpretMpvNative =
  interpretEventsChan .
  runScoped withIpc interpretMpvIpc .
  raiseUnder2

withMpv ::
  Member (Scoped resource (Mpv command !! MpvError)) r =>
  InterpreterFor (Mpv command !! MpvError) r
withMpv =
  scoped
