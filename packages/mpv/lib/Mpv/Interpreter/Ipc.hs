module Mpv.Interpreter.Ipc where

import qualified Data.Map.Strict as Map
import Data.Some (Some)
import Polysemy (runTSimple)
import Polysemy.AtomicState (atomicState')
import qualified Polysemy.Conc as Race
import qualified Polysemy.Conc as Events
import Polysemy.Conc (ChanConsumer, EventConsumer, Queue, interpretEventsChan, withAsync)
import qualified Polysemy.Conc.Data.Queue as Queue
import Polysemy.Conc.Effect.Scoped (Scoped, runScoped, scoped)
import Polysemy.Conc.Interpreter.Queue.TBM (interpretQueueTBMWith)
import qualified Polysemy.Log as Log
import Polysemy.Log (Log)
import Polysemy.Time (Seconds (Seconds), Time, TimeUnit)

import Mpv.Data.Command (Command)
import Mpv.Data.Event (Event)
import Mpv.Data.EventName (EventName, eventNameText)
import Mpv.Data.MpvError (MpvError (MpvError))
import Mpv.Data.MpvEvent (MpvEvent (MpvEvent))
import Mpv.Data.MpvProcessConfig (MpvProcessConfig)
import qualified Mpv.Data.MpvResources as MpvResources
import Mpv.Data.MpvResources (MpvResources (MpvResources), OutMessage (OutMessage), Requests (Requests))
import Mpv.Data.RequestId (RequestId)
import Mpv.Data.Response (ResponseError (ResponseError))
import qualified Mpv.Effect.Commands as Commands
import Mpv.Effect.Commands (Commands)
import qualified Mpv.Effect.Ipc as Ipc
import Mpv.Effect.Ipc (Ipc)
import Mpv.Interpreter.Commands (interpretCommandsJson)
import Mpv.MpvResources (withMpvResources)

createRequest ::
  Members [AtomicState (Requests fmt), Embed IO] r =>
  Sem r (RequestId, MVar (Either ResponseError fmt))
createRequest = do
  mv <- embed newEmptyMVar
  i <- atomicState' \ (Requests n p) -> (Requests (n + 1) (Map.insert n mv p), n)
  pure (i, mv)

sendRequest ::
  Members [Commands fmt command, AtomicState (Requests fmt)] r =>
  Members [Queue (OutMessage fmt) !! MpvError, Stop MpvError, Race, Embed IO] r =>
  command a ->
  Sem r (MVar (Either ResponseError fmt))
sendRequest cmd = do
  (requestId, result) <- createRequest
  msg <- Commands.encode requestId False cmd
  result <$ restop (Queue.write (OutMessage msg))

syncRequest ::
  Members [Commands fmt command, AtomicState (Requests fmt)] r =>
  Members [Queue (OutMessage fmt) !! MpvError, Stop MpvError, Race, Embed IO] r =>
  command a ->
  Sem r a
syncRequest cmd = do
  result <- sendRequest cmd
  response <- Race.timeout_ (Left "mpv request timed out") (Seconds 3) (takeMVar result)
  fmt <- stopEitherWith (MpvError . coerce) response
  stopEitherWith (MpvError . coerce) =<< Commands.decode cmd fmt

waitEvent ::
  Member (EventConsumer token MpvEvent) r =>
  EventName ->
  Sem r (Some Event)
waitEvent target =
  Events.subscribe spin
  where
    spin =
      Events.consume >>= \ (MpvEvent name payload) ->
        if (target == name) then pure payload else spin

waitEventAndRun ::
  TimeUnit u =>
  Members [EventConsumer token MpvEvent, Log, Resource, Async, Race] r =>
  EventName ->
  u ->
  Sem r a ->
  Sem r (Maybe (Some Event), a)
waitEventAndRun name interval ma =
  withAsync (waitEvent name) \ handle -> do
    res <- ma
    found <- Race.timeout_ Nothing interval do
      await handle
    when (isNothing found) do
      Log.warn [exon|waiting for mpv event #{eventNameText name} failed|]
    pure (found, res)

interpretIpcWithQueue ::
  Members [Commands fmt command, EventConsumer token MpvEvent] r =>
  Members [Queue (OutMessage fmt) !! MpvError, AtomicState (Requests fmt), Log, Resource, Async, Race, Embed IO] r =>
  InterpreterFor (Ipc fmt command !! MpvError) r
interpretIpcWithQueue =
  interpretResumableH \case
    Ipc.Sync cmd -> do
      liftT (syncRequest cmd)
    Ipc.WaitEvent name interval ma -> do
      (found, res) <- waitEventAndRun name interval (runTSimple ma)
      pure ((found,) <$> res)

interpretIpc ::
  Members [Commands fmt command, EventConsumer token MpvEvent] r =>
  Members [Log, Resource, Async, Race, Embed IO] r =>
  MpvResources fmt ->
  InterpreterFor (Ipc fmt command !! MpvError) r
interpretIpc MpvResources{requests, outQueue} =
  runAtomicStateTVar requests .
  resumable (interpretQueueTBMWith outQueue) .
  interpretIpcWithQueue .
  raiseUnder2

interpretIpcResources ::
  Members [EventConsumer token MpvEvent, Resource, Async, Race, Log, Embed IO, Final IO] r =>
  Either MpvError (MpvResources Value) ->
  InterpreterFor (Ipc Value Command !! MpvError) r
interpretIpcResources = \case
  Right res ->
    interpretCommandsJson . interpretIpc res . raiseUnder
  Left err ->
    interpretResumableH \ _ -> stop err

interpretIpcNative ::
  Members [Reader MpvProcessConfig, Resource, Async, Race, Log, Time t d, Embed IO, Final IO] r =>
  InterpretersFor [
    Scoped (Either MpvError (MpvResources Value)) (Ipc Value Command !! MpvError),
    ChanConsumer MpvEvent
  ] r
interpretIpcNative =
  interpretEventsChan .
  runScoped withMpvResources interpretIpcResources .
  raiseUnder

withIpc ::
  Member (Scoped resource (Ipc fmt command !! MpvError)) r =>
  InterpreterFor (Ipc fmt command !! MpvError) r
withIpc =
  scoped
