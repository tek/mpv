module Mpv.Interpreter.Commands where

import Data.SOP (All, I (I), NP (Nil, (:*)))
import Polysemy.Time (TimeUnit, convert)
import Polysemy.Time.Data.TimeUnit (unNanoSeconds)
import Prelude hiding (All)

import qualified Mpv.Data.Command as Command
import Mpv.Data.Command (Command, CommandArgs (CommandArgs))
import qualified Mpv.Data.PlaybackState as PlaybackState
import qualified Mpv.Data.Property as Property
import Mpv.Data.Property (Property, propertyName)
import Mpv.Data.Request (Request (Request))
import Mpv.Data.RequestId (RequestId)
import Mpv.Data.Response (ResponseError (ResponseError))
import Mpv.Data.SeekFlags (SeekFlags (SeekFlags))
import Mpv.Data.VideoDuration (unVideoDuration)
import qualified Mpv.Effect.Commands as Commands
import Mpv.Effect.Commands (Commands)
import Mpv.Seek (seekRestartArg, seekStyleArg)
import Mpv.Data.VideoExpired (unVideoExpired)
import Mpv.Data.AudioDelay (unAudioDelay)
import Mpv.Data.SubDelay (unSubDelay)
import Mpv.Data.Volume (unVolume)

percentToRatio ::
  Fractional a =>
  Double ->
  a
percentToRatio pos =
  fromRational (toRational (min 1 (max 0 (pos / 100))))

ratioToPercent ::
  Real a =>
  a ->
  Double
ratioToPercent pos =
  fromRational (toRational (min 100 (max 0 (pos * 100))))

secondsFrac ::
  TimeUnit u =>
  u ->
  Double
secondsFrac u =
  fromIntegral (unNanoSeconds (convert u)) / 1e9

encodeCommand ::
  All ToJSON as =>
  RequestId ->
  Text ->
  NP I as ->
  Bool ->
  Value
encodeCommand requestId cmd args async' =
  toJSON (Request requestId (CommandArgs (I cmd :* args)) async')

encodeProp :: Property v -> v -> Value
encodeProp = \case
  Property.Custom _ ->
    toJSON
  Property.Duration ->
    toJSON . secondsFrac . unVideoDuration
  Property.SubFps ->
    toJSON
  Property.SubDelay ->
    toJSON . secondsFrac . unSubDelay
  Property.AudioDelay ->
    toJSON . secondsFrac . unAudioDelay
  Property.TrackList ->
    toJSON
  Property.PercentPos ->
    toJSON . ratioToPercent
  Property.TimePos ->
    toJSON . secondsFrac . unVideoExpired
  Property.Paused ->
    toJSON . PlaybackState.toBool
  Property.Volume ->
    toJSON . unVolume

mpvCommand :: RequestId -> Bool -> Command a -> Value
mpvCommand requestId async' = \case
  Command.Load path ->
    encodeCommand requestId "loadfile" (I path :* Nil) async'
  Command.Stop ->
    encodeCommand requestId "quit" Nil async'
  Command.Seek pos (SeekFlags reference unit' restart) ->
    encodeCommand requestId "seek" (I (pos * 100) :* I spec :* Nil) async'
    where
      spec =
        [exon|#{seekStyleArg unit' reference}+#{seekRestartArg restart}|]
  Command.Manual _ name args ->
    encodeCommand requestId name args async'
  Command.Prop prop ->
    encodeCommand requestId "get_property" (I (propertyName prop) :* Nil) async'
  Command.SetProp prop value ->
    encodeCommand requestId "set_property" (I (propertyName prop) :* I (encodeProp prop value) :* Nil) async'
  Command.SetOption key value ->
    encodeCommand requestId "set" (I key :* I value :* Nil) async'

decodeProp ::
  Property v ->
  Value ->
  Either Text v
decodeProp = \case
  Property.Custom _ ->
    jsonDecodeValue
  Property.Duration ->
    fmap fromRational . jsonDecodeValue
  Property.SubFps ->
    jsonDecodeValue
  Property.SubDelay ->
    fmap fromRational . jsonDecodeValue
  Property.AudioDelay ->
    fmap fromRational . jsonDecodeValue
  Property.TrackList ->
    jsonDecodeValue
  Property.PercentPos ->
    fmap percentToRatio . jsonDecodeValue
  Property.TimePos ->
    fmap fromRational . jsonDecodeValue
  Property.Paused ->
    fmap PlaybackState.fromBool . jsonDecodeValue
  Property.Volume ->
    jsonDecodeValue

decodeResult ::
  Command a ->
  Value ->
  Either Text a
decodeResult = \case
  Command.Manual _ _ _ ->
    jsonDecodeValue
  Command.Load _ ->
    jsonDecodeValue
  Command.Stop ->
    jsonDecodeValue
  Command.Seek _ _ ->
    jsonDecodeValue
  Command.Prop prop ->
    decodeProp prop
  Command.SetProp _ _ ->
    const unit
  Command.SetOption _ _ ->
    const unit

decodeError :: Text -> ResponseError
decodeError err =
  ResponseError [exon|mpv response decode: #{err}|]

interpretCommandsJson :: InterpreterFor (Commands Value Command) r
interpretCommandsJson =
  interpret \case
    Commands.Encode requestId async' cmd ->
      pure (mpvCommand requestId async' cmd)
    Commands.Decode cmd value ->
      pure (mapLeft decodeError (decodeResult cmd value))
    Commands.Prop prop ->
      pure (Command.Prop prop)
    Commands.SetProp prop value ->
      pure (Command.SetProp prop value)
    Commands.SetOption key value ->
      pure (Command.SetOption key value)
