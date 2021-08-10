module Mpv.Interpreter.Ipc where

import qualified Data.Map.Strict as Map
import Polysemy (runTSimple)
import Polysemy.AtomicState (atomicState')
import qualified Polysemy.Conc as Race
import qualified Polysemy.Conc as Events
import Polysemy.Conc (Queue, Race, withAsync)
import qualified Polysemy.Conc.Data.Queue as Queue
import Polysemy.Conc.Effect.Events (Consume, EventToken)
import Polysemy.Conc.Effect.Scoped (Scoped)
import Polysemy.Conc.Interpreter.Queue.TBM (interpretQueueTBMWith)
import qualified Polysemy.Log as Log
import Polysemy.Log (Log)
import Polysemy.Time (Seconds (Seconds), TimeUnit)

import qualified Mpv.Data.MpvError as MpvError
import Mpv.Data.MpvError (MpvError (MpvError))
import Mpv.Data.MpvEvent (EventName, MpvEvent (MpvEvent), eventNameText)
import qualified Mpv.Data.MpvResources as MpvResources
import Mpv.Data.MpvResources (MpvResources (MpvResources), OutMessage (OutMessage), Requests (Requests))
import Mpv.Data.Property (Property, propertyName)
import Mpv.Data.RequestId (RequestId)
import Mpv.Data.Response (ResponseError (ResponseError))
import qualified Mpv.Effect.Commands as Commands
import Mpv.Effect.Commands (Commands)
import qualified Mpv.Effect.Ipc as Ipc
import Mpv.Effect.Ipc (Ipc)

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
  Members [Scoped (EventToken token) (Consume MpvEvent), Log] r =>
  EventName ->
  Sem r Value
waitEvent target =
  Events.subscribe spin
  where
    spin =
      Events.consume >>= \ (MpvEvent name payload) ->
        if (target == name) then pure payload else spin

waitEventAndRun ::
  TimeUnit u =>
  Member (Scoped (EventToken token) (Consume MpvEvent)) r =>
  Members [Queue (OutMessage fmt) !! MpvError, AtomicState (Requests fmt), Log, Resource, Async, Race, Embed IO] r =>
  EventName ->
  u ->
  Sem r a ->
  Sem r (Maybe Value, a)
waitEventAndRun name interval ma =
  withAsync (waitEvent name) \ handle -> do
    res <- ma
    found <- Race.timeout_ Nothing interval do
      await handle
    when (isNothing found) do
      Log.warn [exon|waiting for mpv event #{eventNameText name} failed|]
    pure (found, res)

propError ::
  Property v ->
  MpvError ->
  MpvError
propError prop = \case
  MpvError err ->
    MpvError (amend err)
  MpvError.Fatal err ->
    MpvError.Fatal (amend err)
  where
    amend err =
      [exon|setting #{show prop} ('#{propertyName prop}'): #{err}|]

interpretIpcWithQueue ::
  Members [Commands fmt command, Scoped (EventToken token) (Consume MpvEvent)] r =>
  Members [Queue (OutMessage fmt) !! MpvError, AtomicState (Requests fmt), Log, Resource, Async, Race, Embed IO] r =>
  InterpreterFor (Ipc fmt command !! MpvError) r
interpretIpcWithQueue =
  interpretResumableH \case
    Ipc.Sync cmd -> do
      liftT (syncRequest cmd)
    Ipc.Async _ ->
      undefined
    Ipc.WaitEvent name interval ma -> do
      (found, res) <- waitEventAndRun name interval (runTSimple ma)
      pure ((found,) <$> res)
    Ipc.Prop prop ->
      liftT do
        cmd <- Commands.prop prop
        mapStop (propError prop) (syncRequest cmd)
    Ipc.SetProp prop value -> do
      liftT do
        cmd <- Commands.setProp prop value
        syncRequest cmd

interpretIpc ::
  Members [Commands fmt command, Scoped (EventToken token) (Consume MpvEvent)] r =>
  Members [Log, Resource, Async, Race, Embed IO] r =>
  MpvResources fmt ->
  InterpreterFor (Ipc fmt command !! MpvError) r
interpretIpc MpvResources{requests, outQueue} =
  runAtomicStateTVar requests .
  resumable (interpretQueueTBMWith outQueue) .
  interpretIpcWithQueue .
  raiseUnder2
