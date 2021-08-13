module Mpv (
  -- * Introduction
  -- $intro

  -- * Effects
  module Mpv.Effect.Mpv,

  -- * Interpreters
  withMpvServer,
  withMpvClient,
) where

import Mpv.Effect.Mpv (Mpv, command, commandSync, prop, setOption, setProp)
import Mpv.Interpreter.MpvServer (withMpvClient, withMpvServer)

-- $intro
-- This is an implementation of an [mpv](https://mpv.io) client using its
-- [JSON IPC API](https://mpv.io/manual/master/#json-ipc), built with [https://hackage.haskell.org/package/polysemy].
