module Mpv.Data.SeekFlags where

data SeekReference =
  Absolute
  |
  Relative
  deriving (Eq, Show)

defaultJson ''SeekReference

instance Default SeekReference where
  def =
    Relative

data SeekUnit =
  Percent
  |
  Time
  deriving (Eq, Show)

defaultJson ''SeekUnit

instance Default SeekUnit where
  def =
    Time

data SeekRestart =
  Keyframes
  |
  Exact
  deriving (Eq, Show)

defaultJson ''SeekRestart

instance Default SeekRestart where
  def =
    Keyframes

data SeekFlags =
  SeekFlags {
    reference :: SeekReference,
    unit :: SeekUnit,
    restart :: SeekRestart
  }
  deriving (Eq, Show, Generic, Default)

defaultJson ''SeekFlags
