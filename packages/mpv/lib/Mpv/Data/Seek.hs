module Mpv.Data.Seek where

import Mpv.Data.SeekFlags (SeekFlags)

data Seek =
  Seek {
    position :: Double,
    flags :: SeekFlags
  }
  deriving (Eq, Show)

defaultJson ''Seek
