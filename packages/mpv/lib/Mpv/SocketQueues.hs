module Mpv.SocketQueues where

import Control.Concurrent.STM.TBMQueue (TBMQueue)
import qualified Data.ByteString as ByteString
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as Socket
import Polysemy.Conc (Queue, withAsync_)
import qualified Polysemy.Conc.Data.Queue as Queue
import qualified Polysemy.Conc.Data.QueueResult as QueueResult
import Polysemy.Conc.Interpreter.Queue.TBM (interpretQueueTBMWith)
import qualified Polysemy.Log as Log
import Polysemy.Log (Log)

import Mpv.Data.MpvResources (
  InMessage (InMessage, InMessageError),
  MpvResources (MpvResources),
  OutMessage (OutMessage),
  )

messageLines :: ByteString -> ([ByteString], ByteString)
messageLines =
  first (filter (not . ByteString.null) . ByteString.split 10) .
  ByteString.spanEnd (/= 10)

parseInMessage :: ByteString -> InMessage Value
parseInMessage =
  jsonDecode >>> \case
    Right v -> InMessage v
    Left err -> InMessageError err

publishAsInMessage ::
  Member (Queue (InMessage Value)) r =>
  ByteString ->
  Sem r ()
publishAsInMessage =
  Queue.write . parseInMessage

concatMessages ::
  ByteString ->
  [ByteString] ->
  ByteString ->
  (ByteString, [ByteString])
concatMessages "" complete extra =
  (extra, complete)
concatMessages buf (rest : complete) extra =
  (extra, (buf <> rest : complete))
concatMessages buf [] extra =
  (buf <> extra, [])

accumulateMessages ::
  ByteString ->
  ByteString ->
  (ByteString, [ByteString])
accumulateMessages buf =
  uncurry (concatMessages buf) . messageLines

readQueue ::
  ∀ r .
  Members [Queue (InMessage Value), Embed IO] r =>
  Socket ->
  Sem r ()
readQueue socket =
  spin ""
  where
    spin buf = do
      tryAny (accumulateMessages buf <$> Socket.recv socket 4096) >>= \case
        Right (newBuf, complete) -> do
          traverse_ publishAsInMessage complete
          spin newBuf
        Left _ ->
          unit

writeQueue ::
  ∀ r .
  Members [Queue (OutMessage Value), Log, Embed IO] r =>
  Socket ->
  Sem r ()
writeQueue socket =
  Queue.read >>= \case
    QueueResult.Success (OutMessage msg) ->
      tryAny (Socket.sendAll socket (jsonEncode msg <> "\n")) >>= \case
        Right () ->
          writeQueue socket
        Left err ->
          Log.debug [exon|mpv write socket terminated: #{err}|]
    _ ->
      unit

interpretQueues ::
  Members [Resource, Race, Embed IO] r =>
  TBMQueue (OutMessage fmt) ->
  TBMQueue (InMessage fmt) ->
  InterpretersFor [Queue (InMessage fmt), Queue (OutMessage fmt)] r
interpretQueues outQ inQ =
  interpretQueueTBMWith outQ .
  interpretQueueTBMWith inQ

withSocketQueues ::
  Members [Resource, Async, Race, Log, Embed IO] r =>
  MpvResources Value ->
  InterpretersFor [Queue (InMessage Value), Queue (OutMessage Value)] r
withSocketQueues (MpvResources socket outQ inQ _) =
  interpretQueues outQ inQ .
  withAsync_ (readQueue socket) .
  withAsync_ (writeQueue socket)
