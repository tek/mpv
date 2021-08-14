-- |Description: Mpv Client/Server Interpreters
module Mpv.Interpreter.MpvServer where

import Data.Some (withSome)
import Polysemy (runTSimple)
import qualified Polysemy.Conc as Conc
import Polysemy.Conc (ChanConsumer, EventConsumer, Queue, interpretQueueTBM, withAsync_)
import qualified Polysemy.Conc.Data.Queue as Queue
import qualified Polysemy.Conc.Data.QueueResult as QueueResult
import Polysemy.Conc.Effect.Scoped (Scoped)
import qualified Polysemy.Log as Log
import Polysemy.Log (Log)
import Polysemy.Time (Time)

import Mpv.Data.Command (Command)
import qualified Mpv.Data.Event as Event
import qualified Mpv.Data.EventPayload as EventPayload
import qualified Mpv.Data.EventPayload as EndReason
import Mpv.Data.MpvError (MpvError)
import Mpv.Data.MpvEvent (MpvEvent (MpvEvent))
import qualified Mpv.Effect.Ipc as Ipc
import Mpv.Effect.Ipc (Ipc)
import Mpv.Effect.Mpv (Mpv)
import qualified Mpv.Effect.MpvServer as MpvServer
import Mpv.Effect.MpvServer (MpvServer)
import Mpv.Interpreter.Commands (interpretCommandsJson)
import Mpv.Interpreter.Ipc (interpretIpcNative, waitEventAndRun, withIpc)
import Mpv.Interpreter.Mpv (interpretMpvIpc)

data Control command where
  SendCommand :: command a -> MVar (Either MpvError a) -> Control command
  Terminate :: Control command

dispatch ::
  ∀ fmt command r a .
  Members [Ipc fmt command !! MpvError, Embed IO] r =>
  command a ->
  MVar (Either MpvError a) ->
  Sem r ()
dispatch cmd result = do
  r <- resuming (pure . Left) (Right <$> Ipc.sync cmd)
  embed (putMVar result r)

serverActive ::
  Members [Queue (Control command), Scoped resource (Ipc fmt command !! MpvError), Log, Embed IO] r =>
  Sem r ()
serverActive =
  withIpc spin
  where
    spin =
      Queue.read >>= \case
        QueueResult.Success Terminate ->
          unit
        QueueResult.Success (SendCommand cmd result) -> do
          dispatch cmd result
          spin
        QueueResult.NotAvailable ->
          unit
        QueueResult.Closed ->
          unit

serverIdle ::
  Members [Queue (Control command), Scoped resource (Ipc fmt command !! MpvError), Log, Embed IO] r =>
  Sem r ()
serverIdle =
  Queue.peek >>= \case
    QueueResult.Success Terminate ->
      Queue.read *> Log.warn "mpv server: received Terminate in idle server"
    QueueResult.Success (SendCommand _ _) ->
      serverActive *> serverIdle
    QueueResult.NotAvailable -> unit
    QueueResult.Closed -> unit

serverEventListener ::
  Members [EventConsumer token MpvEvent, MpvServer command !! MpvError, Log] r =>
  Sem r ()
serverEventListener =
  Conc.subscribeLoop \case
    MpvEvent _ payload ->
      withSome payload \case
        Event.EndFile (EventPayload.EndFile _ EndReason.Stop) ->
          unit
        Event.EndFile _ ->
          resume MpvServer.terminate \ e ->
            Log.warn [exon|mpv server event listener: failed to send Terminate: #{show e}|]
        _ ->
          unit

interpretMpvServer ::
  Members [Queue (Control Command), Embed IO] r =>
  InterpreterFor (MpvServer Command !! MpvError) r
interpretMpvServer =
  interpretResumable \case
    MpvServer.Send cmd -> do
      result <- embed newEmptyMVar
      Queue.write (SendCommand cmd result)
      stopEither =<< embed (takeMVar result)
    MpvServer.Terminate ->
      Queue.write Terminate

withMpvServer ::
  Members [Time t d, Log, Resource, Race, Async, Embed IO, Final IO] r =>
  InterpretersFor [MpvServer Command !! MpvError, ChanConsumer MpvEvent] r
withMpvServer =
  interpretQueueTBM 64 .
  interpretIpcNative .
  withAsync_ serverIdle .
  interpretMpvServer .
  withAsync_ serverEventListener .
  raiseUnder .
  raise2Under

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

interpretMpvClient ::
  Members [MpvServer Command !! MpvError, EventConsumer token MpvEvent, Log, Resource, Async, Race] r =>
  InterpreterFor (Mpv Command !! MpvError) r
interpretMpvClient =
  interpretCommandsJson .
  interpretIpcClient .
  interpretMpvIpc .
  raiseUnder2
