module Mpv.Data.SocketPath where

import Path (Abs, File, Path)

newtype SocketPath =
  SocketPath { unSocketPath :: Path Abs File }
  deriving stock (Eq, Show)
