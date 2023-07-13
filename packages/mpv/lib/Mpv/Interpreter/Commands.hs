module Mpv.Interpreter.Commands where

import Data.Aeson (ToJSON (toJSON), Value)
import Data.SOP (All, I (I), K (K), NP (Nil, (:*)), hcmap, hcollapse, unI)
import Exon (exon)
import Time (convert, unMilliSeconds, unNanoSeconds)

import Mpv.Data.AudioDelay (unAudioDelay)
import qualified Mpv.Data.Command as Command
import Mpv.Data.Command (Command)
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
import Mpv.Json (jsonDecodeValue)
import Mpv.Seek (seekRestartArg, seekStyleArg)

percentToRatio ::
  Fractional a =>
  Double ->
  a
percentToRatio pos =
  fromRational (toRational (min 1 (max 0 (fromMaybe 0 (pos / 100)))))

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
  fromMaybe 0 (fromIntegral (unNanoSeconds (convert u)) / 1e9)

encodeCommand ::
  Text ->
  [Value] ->
  RequestId ->
  Bool ->
  Value
encodeCommand cmd args requestId async' =
  toJSON (Request requestId (toJSON cmd : args) async')

encodeCommandGen ::
  All ToJSON as =>
  Text ->
  NP I as ->
  RequestId ->
  Bool ->
  Value
encodeCommandGen cmd args =
  encodeCommand cmd argValues
  where
    argValues =
      hcollapse (hcmap (Proxy @ToJSON) (K . toJSON . unI) args)

encodeProp :: Property v -> v -> Value
encodeProp = \case
  Property.Custom _ ->
    toJSON
  Property.Duration ->
    toJSON . secondsFrac . (.unVideoDuration)
  Property.SubFps ->
    toJSON
  Property.SubDelay ->
    toJSON . secondsFrac . (.unSubDelay)
  Property.AudioDelay ->
    toJSON . secondsFrac . (.unAudioDelay)
  Property.TrackList ->
    toJSON
  Property.PercentPos ->
    toJSON . ratioToPercent
  Property.TimePos ->
    toJSON . secondsFrac . (.unVideoExpired)
  Property.Paused ->
    toJSON . PlaybackState.toBool
  Property.Volume ->
    toJSON . (.unVolume)

mpvCommand :: Command a -> RequestId -> Bool -> Value
mpvCommand = \case
  Command.Load path (Just opts) ->
    encodeCommandGen "loadfile" (I path :* I opts :* Nil)
  Command.Load path Nothing ->
    encodeCommandGen "loadfile" (I path :* Nil)
  Command.Stop ->
    encodeCommandGen "quit" Nil
  Command.Seek pos (SeekFlags reference unit' restart) ->
    encodeCommandGen "seek" (I (seekValue pos unit') :* I spec :* Nil)
    where
      spec =
        [exon|#{seekStyleArg unit' reference}+#{seekRestartArg restart}|]
  Command.Manual _ name args ->
    encodeCommand name args
  Command.Prop prop ->
    encodeCommandGen "get_property" (I (propertyName prop) :* Nil)
  Command.SetProp prop value ->
    encodeCommandGen "set_property" (I (propertyName prop) :* I (encodeProp prop value) :* Nil)
  Command.AddProp prop (Just value) ->
    encodeCommandGen "add_property" (I (propertyName prop) :* I (encodeProp prop value) :* Nil)
  Command.AddProp prop Nothing ->
    encodeCommandGen "add_property" (I (propertyName prop) :* Nil)
  Command.CycleProp prop (Just direction) ->
    encodeCommandGen "cycle_property" (I (propertyName prop) :* I direction :* Nil)
  Command.CycleProp prop Nothing ->
    encodeCommandGen "cycle_property" (I (propertyName prop) :* Nil)
  Command.MultiplyProp prop value ->
    encodeCommandGen "multiply_property" (I (propertyName prop) :* I (encodeProp prop value) :* Nil)
  Command.SetOption key value ->
    encodeCommandGen "set" (I key :* I value :* Nil)
  Command.ShowText txt duration level ->
    encodeCommandGen "show_text" (I txt :* I (unMilliSeconds (convert duration)) :* I level :* Nil)
  Command.ShowProgress ->
    encodeCommand "show_progress" []

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
  Command.ShowText _ _ _ ->
    const unit
  Command.ShowProgress ->
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
      pure (first decodeError (decodeResult cmd value))
