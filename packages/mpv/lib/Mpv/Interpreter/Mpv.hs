module Mpv.Interpreter.Mpv where

import Conc (Consume)

import qualified Mpv.Data.Command as Command
import Mpv.Data.Command (Command)
import Mpv.Data.EventName (EventName (EndFile, FileLoaded))
import Mpv.Data.MpvError (MpvError)
import Mpv.Data.MpvEvent (MpvEvent)
import Mpv.Data.MpvProcessConfig (MpvProcessConfig)
import qualified Mpv.Effect.Ipc as Ipc
import Mpv.Effect.Ipc (Ipc, withIpc)
import qualified Mpv.Effect.Mpv as Mpv
import Mpv.Effect.Mpv (Mpv)
import Mpv.Interpreter.Ipc (interpretIpcNative)
import Mpv.MpvError (optionError, propError, setPropError)

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

type MpvScope fmt =
  '[Ipc fmt Command !! MpvError]

mpvScope ::
  ∀ fmt r a .
  Member (Scoped_ (Ipc fmt Command !! MpvError) !! MpvError) r =>
  (() -> Sem (MpvScope fmt ++ Stop MpvError : r) a) ->
  Sem (Stop MpvError : r) a
mpvScope use =
  withIpc (use ())

handleMpvIpc ::
  Members [Ipc fmt Command !! MpvError, Stop MpvError] r =>
  Mpv m a ->
  Sem r a
handleMpvIpc = \case
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

interpretMpvIpcClient ::
  Member (Ipc fmt Command !! MpvError) r =>
  InterpreterFor (Mpv !! MpvError) r
interpretMpvIpcClient =
  interpretResumable handleMpvIpc

interpretMpvIpcServer ::
  ∀ fmt r .
  Member (Scoped_ (Ipc fmt Command !! MpvError) !! MpvError) r =>
  InterpreterFor (Scoped_ (Mpv !! MpvError) !! MpvError) r
interpretMpvIpcServer =
  interpretScopedRWith @(MpvScope _) (const mpvScope) \ _ -> handleMpvIpc

interpretMpvNative ::
  Members [Reader MpvProcessConfig, Resource, Async, Race, Log, Time t d, Embed IO, Final IO] r =>
  InterpretersFor [Scoped_ (Mpv !! MpvError) !! MpvError, EventConsumer MpvEvent] r
interpretMpvNative =
  interpretIpcNative .
  interpretMpvIpcServer .
  raiseUnder

events ::
  Member (EventConsumer MpvEvent) r =>
  InterpreterFor (Consume MpvEvent) r
events =
  scoped_
