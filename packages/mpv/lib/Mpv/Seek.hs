module Mpv.Seek where

import Mpv.Data.SeekFlags (SeekReference (Absolute, Relative), SeekRestart (Exact, Keyframes), SeekUnit (Percent, Time))

seekStyleArg ::
  SeekUnit ->
  SeekReference ->
  Text
seekStyleArg Time = \case
  Absolute -> "absolute"
  Relative -> "relative"
seekStyleArg Percent = \case
  Absolute -> "absolute-percent"
  Relative -> "relative-percent"

seekRestartArg :: SeekRestart -> Text
seekRestartArg = \case
  Keyframes -> "keyframes"
  Exact -> "exact"
