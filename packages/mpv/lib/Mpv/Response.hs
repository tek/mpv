module Mpv.Response where

import Data.Aeson (Value (Null), fromJSON)
import qualified Data.Map.Strict as Map
import Polysemy.AtomicState (atomicState')
import qualified Polysemy.Conc as Events
import qualified Polysemy.Conc as Queue
import Polysemy.Conc (Events, Queue)
import qualified Polysemy.Log as Log
import Polysemy.Log (Log)

import Mpv.Data.Message (Message (ResponseEvent, ResponseMessage))
import Mpv.Data.MpvEvent (MpvEvent)
import Mpv.Data.MpvResources (InMessage (InMessage, InMessageError), Requests (Requests))
import Mpv.Data.RequestId (RequestId (RequestId))
import Mpv.Data.Response (Response (Response), ResponseError (ResponseError))

decodePayload :: Text -> Maybe Value -> Either ResponseError Value
decodePayload "success" value =
  Right (fromMaybe Null value)
decodePayload err _ =
  Left (ResponseError err)

decodeMessage ::
  Message ->
  Either MpvEvent (Response Value)
decodeMessage = \case
  ResponseMessage requestId err value ->
    Right (Response (RequestId requestId) (decodePayload err value))
  ResponseEvent event ->
    Left event

decodeInMessage ::
  InMessage Value ->
  Either Text (Either MpvEvent (Response Value))
decodeInMessage (InMessage msg) = do
  message <- aesonToEither (fromJSON msg)
  pure (decodeMessage message)
decodeInMessage (InMessageError err) =
  Left err

parseError ::
  Show fmt =>
  Member Log r =>
  InMessage fmt ->
  Text ->
  Sem r ()
parseError msg err =
  Log.error [exon|mpv response parse error: #{err}; #{show msg}|]

notifyResponse ::
  Members [AtomicState (Requests Value), Log, Embed IO] r =>
  RequestId ->
  Either ResponseError Value ->
  Sem r ()
notifyResponse requestId result =
  atomicState' fetch >>= \case
    Just notify ->
      embed (putMVar notify result)
    Nothing ->
      Log.debug [exon|unknown mpv request with id #{show requestId}: #{show result}|]
  where
    fetch (Requests n p) =
      (Requests n (Map.delete requestId p), Map.lookup requestId p)

processMessage ::
  Members [Events t MpvEvent, AtomicState (Requests Value), Log, Embed IO] r =>
  InMessage Value ->
  Either Text (Either MpvEvent (Response Value)) ->
  Sem r ()
processMessage msg = \case
  Right (Right (Response requestId payload)) ->
    notifyResponse requestId payload
  Right (Left event) ->
    Events.publish event
  Left err ->
    parseError msg err

responseListener ::
  Members [Events t MpvEvent, Queue (InMessage Value), AtomicState (Requests Value), Log, Embed IO] r =>
  Sem r ()
responseListener =
  Queue.loop \ msg -> processMessage msg (decodeInMessage msg)
