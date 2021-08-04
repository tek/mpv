module Mpv.Response where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value (Null), fromJSON)
import qualified Data.Map.Strict as Map
import Polysemy.AtomicState (atomicState')
import qualified Polysemy.Conc as Events
import qualified Polysemy.Conc as Queue
import Polysemy.Conc (Events, Queue)
import qualified Polysemy.Log as Log
import Polysemy.Log (Log)

import Mpv.Data.Message (Message (ResponseEvent, ResponseMessage))
import Mpv.Data.MpvEvent (MpvEvent (MpvEvent))
import Mpv.Data.MpvResources (InMessage (InMessage), Requests (Requests))
import Mpv.Data.RequestId (RequestId (RequestId))
import Mpv.Data.Response (Response (Response), ResponseError (ResponseError))

decodePayload :: Text -> Maybe Value -> Either ResponseError Value
decodePayload "success" value =
  Right (fromMaybe Null value)
decodePayload err _ =
  Left (ResponseError err)

decodeMessage ::
  Value ->
  Message ->
  Either MpvEvent Response
decodeMessage payload = \case
  ResponseMessage requestId err value ->
    Right (Response (RequestId requestId) (decodePayload err value))
  ResponseEvent name ->
    Left (MpvEvent name payload)

resultToEither :: Aeson.Result a -> Either Text a
resultToEither = \case
  Aeson.Success a -> Right a
  Aeson.Error s -> Left (toText s)

decodeInMessage ::
  InMessage ->
  Either Text (Either MpvEvent Response)
decodeInMessage (InMessage msg) = do
  value <- jsonDecode msg
  message <- resultToEither (fromJSON value)
  pure (decodeMessage value message)

parseError ::
  Member Log r =>
  InMessage ->
  Text ->
  Sem r ()
parseError (InMessage msg) err =
  Log.error [exon|mpv response parse error: #{err}; #{decodeUtf8 msg}|]

notifyResponse ::
  Members [AtomicState Requests, Log, Embed IO] r =>
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
  Members [Events t MpvEvent, AtomicState Requests, Log, Embed IO] r =>
  InMessage ->
  Either Text (Either MpvEvent Response) ->
  Sem r ()
processMessage msg = \case
  Right (Right (Response requestId payload)) ->
    notifyResponse requestId payload
  Right (Left event) ->
    Events.publish event
  Left err ->
    parseError msg err

responseListener ::
  Members [Events t MpvEvent, Queue InMessage, AtomicState Requests, Log, Embed IO] r =>
  Sem r ()
responseListener =
  Queue.loop \ msg -> processMessage msg (decodeInMessage msg)
