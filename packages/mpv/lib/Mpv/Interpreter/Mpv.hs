module Mpv.Interpreter.Mpv where

import Polysemy.Conc (ChanConsumer, EventConsumer, interpretEventsChan)
import Polysemy.Conc.Effect.Events (Consume, EventToken)
import Polysemy.Conc.Effect.Scoped (Scoped, runScoped, scoped)
import Polysemy.Log (Log)
import Polysemy.Time (Time, TimeUnit)

import qualified Mpv.Data.Command as Command
import Mpv.Data.Command (Command)
import Mpv.Data.EventName (EventName (FileLoaded, EndFile))
import Mpv.Data.MpvError (MpvError)
import Mpv.Data.MpvEvent (MpvEvent)
import Mpv.Data.MpvProcessConfig (MpvProcessConfig)
import Mpv.Data.MpvResources (MpvResources)
import qualified Mpv.Effect.Ipc as Ipc
import Mpv.Effect.Ipc (Ipc)
import qualified Mpv.Effect.Mpv as Mpv
import Mpv.Effect.Mpv (Mpv)
import Mpv.Interpreter.Commands (interpretCommandsJson)
import Mpv.Interpreter.Ipc (interpretIpc)
import Mpv.MpvError (optionError, propError, setPropError)
import Mpv.MpvResources (withMpvResources)

commandEvent :: Command a -> Maybe EventName
commandEvent = \case
  Command.Manual event _ _ -> event
  Command.Load _ _ -> Just FileLoaded
  Command.Stop -> Just EndFile
  _ -> Nothing

waitEventCmd ::
  TimeUnit u =>
  Member (Ipc fmt Command) r =>
  u ->
  Command b ->
  Sem r a ->
  Sem r a
waitEventCmd wait (commandEvent -> Just event) ma =
  snd <$> Ipc.waitEvent event wait ma
waitEventCmd _ _ ma =
  ma

interpretMpvIpc ::
  Member (Ipc fmt Command !! MpvError) r =>
  InterpreterFor (Mpv !! MpvError) r
interpretMpvIpc =
  interpretResumable \case
    Mpv.CommandSync wait cmd ->
      restop (waitEventCmd wait cmd (Ipc.sync cmd))
    Mpv.Prop prop ->
      resumeHoist (propError prop) (Ipc.sync (Command.Prop prop))
    Mpv.SetProp prop value ->
      resumeHoist (setPropError prop) (Ipc.sync (Command.SetProp prop value))
    Mpv.AddProp prop value ->
      resumeHoist (setPropError prop) (Ipc.sync (Command.AddProp prop value))
    Mpv.CycleProp prop direction ->
      resumeHoist (setPropError prop) (Ipc.sync (Command.CycleProp prop direction))
    Mpv.MultiplyProp prop value ->
      resumeHoist (setPropError prop) (Ipc.sync (Command.MultiplyProp prop value))
    Mpv.SetOption key value ->
      resumeHoist (optionError key value) (Ipc.sync (Command.SetOption key value))

interpretMpvResources ::
  Members [EventConsumer token MpvEvent, Resource, Async, Race, Log, Embed IO, Final IO] r =>
  Either MpvError (MpvResources Value) ->
  InterpreterFor (Mpv !! MpvError) r
interpretMpvResources = \case
  Right res ->
    interpretCommandsJson . interpretIpc res . interpretMpvIpc . raiseUnder2
  Left err ->
    interpretResumableH \ _ -> stop err

interpretMpvNative ::
  Members [Reader MpvProcessConfig, Resource, Async, Race, Log, Time t d, Embed IO, Final IO] r =>
  InterpretersFor [Scoped (Either MpvError (MpvResources Value)) (Mpv !! MpvError), ChanConsumer MpvEvent] r
interpretMpvNative =
  interpretEventsChan .
  runScoped withMpvResources interpretMpvResources .
  raiseUnder

withMpv ::
  Member (Scoped resource (Mpv !! MpvError)) r =>
  InterpreterFor (Mpv !! MpvError) r
withMpv =
  scoped

events ::
  Member (Scoped (EventToken token) (Consume MpvEvent)) r =>
  InterpreterFor (Consume MpvEvent) r
events =
  scoped
