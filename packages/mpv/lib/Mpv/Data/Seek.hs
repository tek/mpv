module Mpv.Data.Seek where

import Mpv.Data.SeekFlags (SeekFlags)

data Seek =
  Seek {
    position :: Double,
    flags :: SeekFlags
  }
  deriving stock (Eq, Show)

json ''Seek
