module Mpv.Interpreter.Mpv where

import Polysemy.Conc (ChanConsumer, EventConsumer, interpretEventsChan)
import Polysemy.Conc.Effect.Events (Consume, EventToken)
import Polysemy.Conc.Effect.Scoped (Scoped, runScoped, scoped)
import Polysemy.Log (Log)
import Polysemy.Time (Time, TimeUnit)

import Mpv.Class.CommandEvent (CommandEvent (commandEvent))
import Mpv.Data.Command (Command)
import Mpv.Data.MpvError (MpvError)
import Mpv.Data.MpvEvent (MpvEvent)
import Mpv.Data.MpvResources (MpvResources)
import qualified Mpv.Effect.Commands as Commands
import Mpv.Effect.Commands (Commands)
import qualified Mpv.Effect.Ipc as Ipc
import Mpv.Effect.Ipc (Ipc)
import qualified Mpv.Effect.Mpv as Mpv
import Mpv.Effect.Mpv (Mpv)
import Mpv.Interpreter.Commands (interpretCommandsJson)
import Mpv.Interpreter.Ipc (interpretIpc)
import Mpv.MpvError (optionError, propError, setPropError)
import Mpv.MpvResources (withMpvResources)

waitEventCmd ::
  TimeUnit u =>
  CommandEvent command =>
  Member (Ipc fmt command) r =>
  u ->
  command b ->
  Sem r a ->
  Sem r a
waitEventCmd wait (commandEvent -> Just event) ma =
  snd <$> Ipc.waitEvent event wait ma
waitEventCmd _ _ ma =
  ma

interpretMpvIpc ::
  CommandEvent command =>
  Members [Ipc fmt command !! MpvError, Commands fmt command] r =>
  InterpreterFor (Mpv command !! MpvError) r
interpretMpvIpc =
  interpretResumable \case
    Mpv.CommandSync wait cmd ->
      restop (waitEventCmd wait cmd (Ipc.sync cmd))
    Mpv.Prop prop -> do
      cmd <- Commands.prop prop
      resumeHoist (propError prop) (Ipc.sync cmd)
    Mpv.SetProp prop value -> do
      cmd <- Commands.setProp prop value
      resumeHoist (setPropError prop) (Ipc.sync cmd)
    Mpv.SetOption key value -> do
      cmd <- Commands.setOption key value
      resumeHoist (optionError key value) (Ipc.sync cmd)

interpretMpvResources ::
  Members [EventConsumer token MpvEvent, Resource, Async, Race, Log, Embed IO, Final IO] r =>
  Either MpvError (MpvResources Value) ->
  InterpreterFor (Mpv Command !! MpvError) r
interpretMpvResources = \case
  Right res ->
    interpretCommandsJson . interpretIpc res . interpretMpvIpc . raiseUnder2
  Left err ->
    interpretResumableH \ _ -> stop err

interpretMpvNative ::
  Members [Resource, Async, Race, Log, Time t d, Embed IO, Final IO] r =>
  InterpretersFor [Scoped (Either MpvError (MpvResources Value)) (Mpv Command !! MpvError), ChanConsumer MpvEvent] r
interpretMpvNative =
  interpretEventsChan .
  runScoped withMpvResources interpretMpvResources .
  raiseUnder

withMpv ::
  Member (Scoped resource (Mpv command !! MpvError)) r =>
  InterpreterFor (Mpv command !! MpvError) r
withMpv =
  scoped

events ::
  Member (Scoped (EventToken token) (Consume MpvEvent)) r =>
  InterpreterFor (Consume MpvEvent) r
events =
  scoped

loopEvents ::
  Member (Scoped (EventToken token) (Consume MpvEvent)) r =>
  InterpreterFor (Consume MpvEvent) r
loopEvents =
  events . forever
