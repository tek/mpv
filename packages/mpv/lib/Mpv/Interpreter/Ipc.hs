module Mpv.Interpreter.Ipc where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value)
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

import Mpv.Command (mpvCommand)
import qualified Mpv.Data.Command as Command
import Mpv.Data.Command (Command)
import Mpv.Data.MpvError (MpvError (MpvError))
import Mpv.Data.MpvEvent (EventName, MpvEvent (MpvEvent), eventNameText)
import qualified Mpv.Data.MpvResources as MpvResources
import Mpv.Data.MpvResources (MpvResources (MpvResources), OutMessage (OutMessage), Requests (Requests))
import Mpv.Data.RequestId (RequestId)
import Mpv.Data.Response (ResponseError (ResponseError))
import qualified Mpv.Effect.Ipc as Ipc
import Mpv.Effect.Ipc (Ipc)

createRequest ::
  Members [AtomicState Requests, Embed IO] r =>
  Sem r (RequestId, MVar (Either ResponseError Value))
createRequest = do
  mv <- embed newEmptyMVar
  i <- atomicState' \ (Requests n p) -> (Requests (n + 1) (Map.insert n mv p), n)
  pure (i, mv)

decodeResult ::
  Command a ->
  Value ->
  Either ResponseError a
decodeResult = \case
  Command.Manual _ _ ->
    check . Aeson.fromJSON
  Command.Load _ ->
    check . Aeson.fromJSON
  Command.Stop ->
    check . Aeson.fromJSON
  Command.Seek _ _ ->
    check . Aeson.fromJSON
  where
    check = \case
      Aeson.Success a -> Right a
      Aeson.Error err -> Left (ResponseError [exon|mpv response decode: #{fromString err}|])

syncRequest ::
  Members [AtomicState Requests, Queue OutMessage !! MpvError, Stop MpvError, Race, Embed IO] r =>
  Command a ->
  Sem r a
syncRequest cmd = do
  (requestId, result) <- createRequest
  restop (Queue.write (OutMessage (mpvCommand requestId cmd)))
  response <- Race.timeout_ (Left "mpv request timed out") (Seconds 3) (takeMVar result)
  stopEitherWith (MpvError . coerce) (response >>= decodeResult cmd)

waitEvent ::
  Members [Scoped (EventToken token) (Consume MpvEvent), Log] r =>
  EventName ->
  Sem r Value
waitEvent target =
  Events.subscribe do
    spin
  where
    spin =
      Events.consume >>= \ (MpvEvent name payload) ->
        if (target == name) then pure payload else spin

waitEventAndRun ::
  TimeUnit u =>
  Member (Scoped (EventToken token) (Consume MpvEvent)) r =>
  Members [Queue OutMessage !! MpvError, AtomicState Requests, Log, Resource, Async, Race, Embed IO] r =>
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

interpretIpcWithQueue ::
  Member (Scoped (EventToken token) (Consume MpvEvent)) r =>
  Members [Queue OutMessage !! MpvError, AtomicState Requests, Log, Resource, Async, Race, Embed IO] r =>
  InterpreterFor (Ipc Command !! MpvError) r
interpretIpcWithQueue =
  interpretResumableH \case
    Ipc.Sync msg -> do
      liftT (syncRequest msg)
    Ipc.Async _ ->
      undefined
    Ipc.WaitEvent name interval ma -> do
      (found, res) <- waitEventAndRun name interval (runTSimple ma)
      pure ((found,) <$> res)

interpretIpc ::
  Members [Scoped (EventToken token) (Consume MpvEvent), Log, Resource, Async, Race, Embed IO] r =>
  MpvResources ->
  InterpreterFor (Ipc Command !! MpvError) r
interpretIpc MpvResources{requests, outQueue} =
  runAtomicStateTVar requests .
  resumable (interpretQueueTBMWith outQueue) .
  interpretIpcWithQueue .
  raiseUnder2
