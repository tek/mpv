module Mpv.Data.MpvProcess where

import Path (Abs, File, Path)
import System.Process.Typed (Process)

data MpvProcess =
  MpvProcess {
      socketPath :: Path Abs File,
      process :: Process () () ()
    }
