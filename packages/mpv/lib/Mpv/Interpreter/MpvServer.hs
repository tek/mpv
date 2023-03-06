-- |Description: Mpv Client/Server Interpreters
module Mpv.Interpreter.MpvServer where

import Conc (interpretQueueTBM, withAsync_)
import Data.Some (withSome)
import Exon (exon)
import qualified Conc as Conc
import qualified Queue as QueueResult
import qualified Queue
import qualified Polysemy.Log as Log

import Mpv.Data.Command (Command)
import qualified Mpv.Data.Event as Event
import qualified Mpv.Data.EventPayload as EventPayload
import qualified Mpv.Data.EventPayload as EndReason
import Mpv.Data.MpvError (MpvError)
import Mpv.Data.MpvEvent (MpvEvent (MpvEvent))
import Mpv.Data.MpvProcessConfig (MpvProcessConfig)
import qualified Mpv.Effect.Ipc as Ipc
import Mpv.Effect.Ipc (Ipc)
import Mpv.Effect.Mpv (Mpv)
import qualified Mpv.Effect.MpvServer as MpvServer
import Mpv.Effect.MpvServer (MpvServer)
import Mpv.Interpreter.Ipc (interpretIpcClient, interpretIpcNative)
import Mpv.Interpreter.Mpv (interpretMpvIpcClient)

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
  (∀ x . Show (command x)) =>
  Members [Queue (Control command), Scoped_ (Ipc fmt command !! MpvError) !! MpvError, Log, Embed IO] r =>
  Sem r ()
serverActive =
  scoped_ spin !! \ e -> Log.error [exon|mpv server: process startup error: #{show e}|]
  where
    spin =
      Queue.read >>= \case
        QueueResult.Success Terminate ->
          Log.debug "mpv server: Terminate"
        QueueResult.Success (SendCommand cmd result) -> do
          Log.debug [exon|mpv server: #{show cmd}|]
          dispatch cmd result
          spin
        QueueResult.NotAvailable ->
          Log.debug "mpv server: NotAvailable"
        QueueResult.Closed ->
          Log.debug "mpv server: Closed"

serverIdle ::
  (∀ x . Show (command x)) =>
  Members [Queue (Control command), Scoped_ (Ipc fmt command !! MpvError) !! MpvError, Log, Embed IO] r =>
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
  Members [EventConsumer MpvEvent, MpvServer command !! MpvError, Log] r =>
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
  Members [Reader MpvProcessConfig, Time t d, Log, Resource, Race, Async, Embed IO, Final IO] r =>
  InterpretersFor [MpvServer Command !! MpvError, EventConsumer MpvEvent] r
withMpvServer =
  interpretQueueTBM 64 .
  interpretIpcNative .
  withAsync_ serverIdle .
  interpretMpvServer .
  withAsync_ serverEventListener .
  raiseUnder .
  raise2Under

interpretMpvClient ::
  Members [MpvServer Command !! MpvError, EventConsumer MpvEvent, Log, Resource, Async, Race] r =>
  InterpreterFor (Mpv !! MpvError) r
interpretMpvClient =
  interpretIpcClient .
  interpretMpvIpcClient .
  raiseUnder
