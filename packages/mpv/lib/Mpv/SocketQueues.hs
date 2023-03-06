module Mpv.SocketQueues where

import Conc (interpretQueueTBM, withAsync_)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value)
import qualified Data.ByteString as ByteString
import Exon (exon)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString as Socket
import qualified Queue as QueueResult
import qualified Queue
import qualified Polysemy.Log as Log

import Mpv.Data.MpvResources (InMessage (InMessage, InMessageError), OutMessage (OutMessage))

messageLines :: ByteString -> ([ByteString], ByteString)
messageLines =
  first (filter (not . ByteString.null) . ByteString.split 10) .
  ByteString.spanEnd (/= 10)

parseInMessage :: ByteString -> InMessage Value
parseInMessage =
  Aeson.eitherDecodeStrict' >>> \case
    Right v -> InMessage v
    Left err -> InMessageError (toText err)

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
      tryIOError (accumulateMessages buf <$> Socket.recv socket 4096) >>= \case
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
      tryIOError (Socket.sendAll socket (toStrict (Aeson.encode msg) <> "\n")) >>= \case
        Right () ->
          writeQueue socket
        Left err ->
          Log.debug [exon|mpv write socket terminated: #{err}|]
    _ ->
      unit

withSocketQueues ::
  Members [Log, Resource, Race, Async, Embed IO] r =>
  Socket ->
  InterpretersFor [Queue (OutMessage Value), Queue (InMessage Value)] r
withSocketQueues socket =
  interpretQueueTBM @(InMessage Value) 64 .
  interpretQueueTBM @(OutMessage Value) 64 .
  withAsync_ (readQueue socket) .
  withAsync_ (writeQueue socket)
