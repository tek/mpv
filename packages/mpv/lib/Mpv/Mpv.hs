module Mpv.Mpv where

import Mpv.Data.AudioDelay (AudioDelay)
import Mpv.Data.MpvInfo (MpvInfo (MpvInfo))
import qualified Mpv.Data.PlaybackState as PlaybackState
import Mpv.Data.PlaybackState (PlaybackState)
import qualified Mpv.Data.Property as Property
import Mpv.Data.Property (Property)
import Mpv.Data.SubDelay (SubDelay)
import Mpv.Data.Volume (Volume)
import qualified Mpv.Effect.Mpv as Mpv
import Mpv.Effect.Mpv (Mpv)
import Mpv.Track (tracks)

setDefaultOptions ::
  Member Mpv r =>
  Sem r ()
setDefaultOptions = do
  Mpv.setOption "input-default-bindings" "yes"
  Mpv.setOption "input-vo-keyboard" "yes"
  Mpv.setOption "osd-level" "1"

alterPropM ::
  Show v =>
  Member Mpv r =>
  Property v ->
  (v -> Sem r v) ->
  Sem r v
alterPropM prop f = do
  Mpv.setProp prop =<< f =<< Mpv.prop prop
  Mpv.prop prop

alterProp ::
  Show v =>
  Member Mpv r =>
  Property v ->
  (v -> v) ->
  Sem r v
alterProp prop f =
  alterPropM prop (pure . f)

togglePlaybackState ::
  Member Mpv r =>
  Sem r PlaybackState
togglePlaybackState =
  alterProp Property.Paused PlaybackState.toggle

info ::
  Member Mpv r =>
  Sem r MpvInfo
info = do
  playback <- Mpv.prop Property.Paused
  duration <- Mpv.prop Property.Duration
  progress <- Mpv.prop Property.PercentPos
  expired <- Mpv.prop Property.TimePos
  (_, audio, subs) <- tracks
  subDelay <- Mpv.prop Property.SubDelay
  audioDelay <- Mpv.prop Property.AudioDelay
  pure (MpvInfo playback duration progress expired subs subDelay audio audioDelay)

adjustVolumeBy ::
  Member Mpv r =>
  Volume ->
  Sem r Volume
adjustVolumeBy delta =
  alterProp Property.Volume (delta +)

addAudioDelay ::
  Member Mpv r =>
  AudioDelay ->
  Sem r AudioDelay
addAudioDelay delta =
  alterProp Property.AudioDelay (delta +)

addSubDelay ::
  Member Mpv r =>
  SubDelay ->
  Sem r SubDelay
addSubDelay delta =
  alterProp Property.SubDelay (delta +)
