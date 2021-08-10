module Mpv.Mpv where

import qualified Mpv.Data.PlaybackState as PlaybackState
import Mpv.Data.PlaybackState (PlaybackState)
import qualified Mpv.Data.Property as Property
import Mpv.Data.Property (Property)
import qualified Mpv.Effect.Mpv as Mpv
import Mpv.Effect.Mpv (Mpv)

alterPropM ::
  Show v =>
  Member (Mpv command) r =>
  Property v ->
  (v -> Sem r v) ->
  Sem r v
alterPropM prop f = do
  Mpv.setProp prop =<< f =<< Mpv.prop prop
  Mpv.prop prop

alterProp ::
  Show v =>
  Member (Mpv command) r =>
  Property v ->
  (v -> v) ->
  Sem r v
alterProp prop f =
  alterPropM prop (pure . f)

pause ::
  Member (Mpv command) r =>
  Sem r PlaybackState
pause =
  alterProp Property.Paused PlaybackState.toggle
