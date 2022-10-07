module Mpv.Interpreter.Ipc where

import Conc (
  ChanConsumer,
  ChanEvents,
  Scoped_,
  interpretEventsChan,
  interpretScopedRWithH,
  withAsync,
  )
import Data.Aeson (Value)
import qualified Data.Map.Strict as Map
import Data.Some (Some)
import Exon (exon)
import qualified Polysemy.Conc as Race
import qualified Polysemy.Conc as Events
import qualified Polysemy.Conc.Data.QueueResult as QueueResult
import qualified Polysemy.Conc.Queue as Queue
import Polysemy.Internal.Tactics (liftT)
import qualified Polysemy.Log as Log
import Polysemy.Time (Seconds (Seconds))
import Process (SystemProcess)

import Mpv.Data.Command (Command)
import Mpv.Data.Event (Event)
import Mpv.Data.EventName (EventName, eventNameText)
import qualified Mpv.Data.MpvError as MpvError
import Mpv.Data.MpvError (MpvError (MpvError))
import Mpv.Data.MpvEvent (MpvEvent (MpvEvent))
import Mpv.Data.MpvProcessConfig (MpvProcessConfig)
import Mpv.Data.MpvResources (InMessage, OutMessage (OutMessage), Requests (Requests))
import Mpv.Data.RequestId (RequestId)
import Mpv.Data.Response (ResponseError (ResponseError))
import Mpv.Data.SocketPath (SocketPath)
import qualified Mpv.Effect.Commands as Commands
import Mpv.Effect.Commands (Commands)
import qualified Mpv.Effect.Ipc as Ipc
import Mpv.Effect.Ipc (Ipc)
import qualified Mpv.Effect.MpvServer as MpvServer
import Mpv.Effect.MpvServer (MpvServer)
import Mpv.Interpreter.Commands (interpretCommandsJson)
import Mpv.Process (interpretMpvProcess, withSocketQueuesMpv)
import Mpv.Response (withResponseListener)

createRequest ::
  Members [AtomicState (Requests fmt), Embed IO] r =>
  Sem r (RequestId, MVar (Either ResponseError fmt))
createRequest = do
  mv <- embed newEmptyMVar
  i <- atomicState' \ (Requests n p) -> (Requests (n + 1) (Map.insert n mv p), n)
  pure (i, mv)

sendRequest ::
  Members [Commands fmt command, AtomicState (Requests fmt)] r =>
  Members [Queue (OutMessage fmt), Stop MpvError, Race, Embed IO] r =>
  command a ->
  Sem r (MVar (Either ResponseError fmt))
sendRequest cmd = do
  (requestId, result) <- createRequest
  msg <- Commands.encode requestId False cmd
  Queue.tryWrite (OutMessage msg) >>= \case
    QueueResult.Success () ->
      unit
    QueueResult.Closed ->
      stop (MpvError.Fatal "message queue closed")
    QueueResult.NotAvailable ->
      stop (MpvError.Fatal "message queue full")
  pure result

syncRequest ::
  Members [Commands fmt command, AtomicState (Requests fmt)] r =>
  Members [Queue (OutMessage fmt), Stop MpvError, Race, Embed IO] r =>
  command a ->
  Sem r a
syncRequest cmd = do
  result <- sendRequest cmd
  response <- Race.timeout_ (pure (Left "mpv request timed out")) (Seconds 3) (embed (takeMVar result))
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
    found <- Race.timeout_ (pure Nothing) interval do
      await handle
    when (isNothing found) do
      Log.warn [exon|waiting for mpv event #{eventNameText name} failed|]
    pure (found, res)

interpretIpcWithQueue ::
  Members [Commands fmt command, EventConsumer token MpvEvent] r =>
  Members [Queue (OutMessage fmt), AtomicState (Requests fmt), Log, Resource, Async, Race, Embed IO] r =>
  InterpreterFor (Ipc fmt command !! MpvError) r
interpretIpcWithQueue =
  interpretResumableH \case
    Ipc.Sync cmd -> do
      liftT (syncRequest cmd)
    Ipc.WaitEvent name interval ma -> do
      (found, res) <- waitEventAndRun name interval (runTSimple ma)
      pure ((found,) <$> res)

type IpcScope fmt =
  [AtomicState (Requests fmt), Queue (OutMessage fmt), Queue (InMessage fmt)]

ipcScope ::
  Show pse =>
  Member (Scoped SocketPath pres (SystemProcess !! pe) !! pse) r =>
  Members [ChanEvents MpvEvent, Commands Value Command, ChanConsumer MpvEvent] r =>
  Members [Resource, Async, Race, Log, Time t d, Embed IO, Final IO] r =>
  (() -> Sem (IpcScope Value ++ Stop MpvError : r) a) ->
  Sem (Stop MpvError : r) a
ipcScope use =
  withSocketQueuesMpv $ withResponseListener $ use ()

interpretIpc ::
  Show pse =>
  Member (Scoped SocketPath pres (SystemProcess !! pe) !! pse) r =>
  Members [Commands Value Command, ChanEvents MpvEvent, ChanConsumer MpvEvent] r =>
  Members [Resource, Async, Race, Log, Time t d, Embed IO, Final IO] r =>
  InterpreterFor (Scoped_ () (Ipc Value Command !! MpvError) !! MpvError) r
interpretIpc =
  interpretScopedRWithH @(IpcScope _) (const ipcScope) \ _ -> \case
    Ipc.Sync cmd ->
      liftT (syncRequest cmd)
    Ipc.WaitEvent name interval ma -> do
      (found, res) <- waitEventAndRun name interval (runTSimple ma)
      pure ((found,) <$> res)

interpretIpcNative ::
  Members [Reader MpvProcessConfig, Resource, Async, Race, Log, Time t d, Embed IO, Final IO] r =>
  InterpretersFor [
    Scoped_ () (Ipc Value Command !! MpvError) !! MpvError,
    ChanConsumer MpvEvent
  ] r
interpretIpcNative =
  interpretEventsChan .
  interpretMpvProcess .
  interpretCommandsJson .
  interpretIpc .
  raiseUnder3

interpretIpcClient ::
  Member (MpvServer command !! MpvError) r =>
  Members [EventConsumer token MpvEvent, Log, Resource, Async, Race] r =>
  InterpreterFor (Ipc fmt command !! MpvError) r
interpretIpcClient =
  interpretResumableH \case
    Ipc.Sync cmd ->
      liftT (restop (MpvServer.send cmd))
    Ipc.WaitEvent name interval ma -> do
      (found, res) <- waitEventAndRun name interval (runTSimple ma)
      pure ((found,) <$> res)
