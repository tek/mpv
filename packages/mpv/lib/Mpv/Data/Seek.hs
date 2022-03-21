module Mpv.Data.Seek where

import Polysemy.Time.Json (json)

import Mpv.Data.SeekFlags (SeekFlags)

data Seek =
  Seek {
    position :: Double,
    flags :: SeekFlags
  }
  deriving stock (Eq, Show)

json ''Seek
