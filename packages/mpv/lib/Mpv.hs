module Mpv (
  -- * Introduction
  -- $intro

  -- * Effects
  module Mpv.Effect.Mpv,
  module Mpv.Effect.VideoPlayer,

  -- * Interpreters
  withMpvServer,
  interpretMpvClient,
  interpretVideoPlayer,
  interpretVideoPlayerServer,

  -- * Misc
  PlayerError(PlayerError),
  MpvEvent(MpvEvent),
  MpvEventConsumer,
  Event,
  EventName,
) where

import Mpv.Data.MpvEvent (MpvEvent(MpvEvent), MpvEventConsumer)
import Mpv.Data.PlayerError (PlayerError (PlayerError))
import Mpv.Effect.Mpv (Mpv)
import Mpv.Effect.VideoPlayer (
  VideoPlayer,
  addAudioDelay,
  addSubDelay,
  adjustVolumeBy,
  audioDelay,
  audios,
  current,
  duration,
  expired,
  info,
  load,
  pause,
  progress,
  seek,
  setAudio,
  setAudioDelay,
  setOption,
  setSubDelay,
  setSubFps,
  setSubtitle,
  setVolume,
  stop,
  subDelay,
  subFps,
  subtitles,
  volume,
  )
import Mpv.Interpreter.MpvServer (interpretMpvClient, withMpvServer)
import Mpv.Interpreter.VideoPlayer (interpretVideoPlayer, interpretVideoPlayerServer)
import Mpv.Data.Event (Event)
import Mpv.Data.EventName (EventName)

-- $intro
-- This is an implementation of an [mpv](https://mpv.io) client using its
-- [JSON IPC API](https://mpv.io/manual/master/#json-ipc), built with [https://hackage.haskell.org/package/polysemy].
