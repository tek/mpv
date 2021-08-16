module Mpv.Interpreter.Commands where

import Data.SOP (All, I (I), NP (Nil, (:*)))
import Polysemy.Time (TimeUnit, convert)
import Polysemy.Time.Data.TimeUnit (unNanoSeconds)
import Prelude hiding (All)

import Mpv.Data.AudioDelay (unAudioDelay)
import qualified Mpv.Data.Command as Command
import Mpv.Data.Command (Command, CommandArgs (CommandArgs))
import qualified Mpv.Data.PlaybackState as PlaybackState
import qualified Mpv.Data.Property as Property
import Mpv.Data.Property (Property, propertyName)
import Mpv.Data.Request (Request (Request))
import Mpv.Data.RequestId (RequestId)
import Mpv.Data.Response (ResponseError (ResponseError))
import qualified Mpv.Data.SeekFlags as SeekUnit
import Mpv.Data.SeekFlags (SeekFlags (SeekFlags), SeekUnit)
import Mpv.Data.SubDelay (unSubDelay)
import Mpv.Data.VideoDuration (unVideoDuration)
import Mpv.Data.VideoExpired (unVideoExpired)
import Mpv.Data.Volume (unVolume)
import qualified Mpv.Effect.Commands as Commands
import Mpv.Effect.Commands (Commands)
import Mpv.Seek (seekRestartArg, seekStyleArg)

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

seekValue :: Double -> SeekUnit -> Double
seekValue value = \case
  SeekUnit.Percent -> value * 100
  SeekUnit.Time -> value

secondsFrac ::
  TimeUnit u =>
  u ->
  Double
secondsFrac u =
  fromIntegral (unNanoSeconds (convert u)) / 1e9

encodeCommand ::
  All ToJSON as =>
  Text ->
  NP I as ->
  RequestId ->
  Bool ->
  Value
encodeCommand cmd args requestId async' =
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

mpvCommand :: Command a -> RequestId -> Bool -> Value
mpvCommand = \case
  Command.Load path (Just opts) ->
    encodeCommand "loadfile" (I path :* I opts :* Nil)
  Command.Load path Nothing ->
    encodeCommand "loadfile" (I path :* Nil)
  Command.Stop ->
    encodeCommand "quit" Nil
  Command.Seek pos (SeekFlags reference unit' restart) ->
    encodeCommand "seek" (I (seekValue pos unit') :* I spec :* Nil)
    where
      spec =
        [exon|#{seekStyleArg unit' reference}+#{seekRestartArg restart}|]
  Command.Manual _ name args ->
    encodeCommand name args
  Command.Prop prop ->
    encodeCommand "get_property" (I (propertyName prop) :* Nil)
  Command.SetProp prop value ->
    encodeCommand "set_property" (I (propertyName prop) :* I (encodeProp prop value) :* Nil)
  Command.AddProp prop (Just value) ->
    encodeCommand "add_property" (I (propertyName prop) :* I (encodeProp prop value) :* Nil)
  Command.AddProp prop Nothing ->
    encodeCommand "add_property" (I (propertyName prop) :* Nil)
  Command.CycleProp prop (Just direction) ->
    encodeCommand "cycle_property" (I (propertyName prop) :* I direction :* Nil)
  Command.CycleProp prop Nothing ->
    encodeCommand "cycle_property" (I (propertyName prop) :* Nil)
  Command.MultiplyProp prop value ->
    encodeCommand "multiply_property" (I (propertyName prop) :* I (encodeProp prop value) :* Nil)
  Command.SetOption key value ->
    encodeCommand "set" (I key :* I value :* Nil)

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
  Command.Load _ _ ->
    jsonDecodeValue
  Command.Stop ->
    jsonDecodeValue
  Command.Seek _ _ ->
    jsonDecodeValue
  Command.Prop prop ->
    decodeProp prop
  Command.SetProp _ _ ->
    const unit
  Command.AddProp _ _ ->
    const unit
  Command.CycleProp _ _ ->
    const unit
  Command.MultiplyProp _ _ ->
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
      pure (mpvCommand cmd requestId async')
    Commands.Decode cmd value ->
      pure (mapLeft decodeError (decodeResult cmd value))
