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
) where

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
  info,
  load,
  pause,
  seek,
  setAudio,
  setAudioDelay,
  setSubDelay,
  setSubFps,
  setSubtitle,
  setVolume,
  stop,
  subDelay,
  subFps,
  subtitles,
  volume,
  progress,
  expired,
  setOption,
  )
import Mpv.Interpreter.MpvServer (interpretMpvClient, withMpvServer)
import Mpv.Interpreter.VideoPlayer (interpretVideoPlayer)

-- $intro
-- This is an implementation of an [mpv](https://mpv.io) client using its
-- [JSON IPC API](https://mpv.io/manual/master/#json-ipc), built with [https://hackage.haskell.org/package/polysemy].
