module Mpv.Command where

import Data.SOP (All, I (I), NP (Nil, (:*)))
import Prelude hiding (All)

import qualified Mpv.Data.Command as Command
import Mpv.Data.Command (Command, CommandArgs (CommandArgs))
import Mpv.Data.Request (Request (Request))
import Mpv.Data.RequestId (RequestId)
import Mpv.Data.SeekFlags (
  SeekFlags (SeekFlags),
  SeekReference (Absolute, Relative),
  SeekRestart (Exact, Keyframes),
  SeekUnit (Percent, Time),
  )

encodeCommand ::
  All ToJSON as =>
  RequestId ->
  String ->
  NP I as ->
  ByteString
encodeCommand requestId cmd args =
  jsonEncode (Request requestId (CommandArgs (I cmd :* args)))

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

mpvCommand :: RequestId -> Command a -> ByteString
mpvCommand requestId = \case
  Command.Load path ->
    encodeCommand requestId "loadfile" (I path :* Nil)
  Command.Stop ->
    encodeCommand requestId "quit" Nil
  Command.Seek pos (SeekFlags reference unit' restart) ->
    encodeCommand requestId "seek" (I (pos * 100) :* I spec :* Nil)
    where
      spec =
        [exon|#{seekStyleArg unit' reference}+#{seekRestartArg restart}|]
  Command.Manual _ _ ->
    undefined
