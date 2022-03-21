module Mpv.Data.SeekFlags where

import Polysemy.Time.Json (json)

data SeekReference =
  Absolute
  |
  Relative
  deriving stock (Eq, Show)

json ''SeekReference

instance Default SeekReference where
  def =
    Relative

data SeekUnit =
  Percent
  |
  Time
  deriving stock (Eq, Show)

json ''SeekUnit

instance Default SeekUnit where
  def =
    Time

data SeekRestart =
  Keyframes
  |
  Exact
  deriving stock (Eq, Show)

json ''SeekRestart

instance Default SeekRestart where
  def =
    Keyframes

data SeekFlags =
  SeekFlags {
    reference :: SeekReference,
    unit :: SeekUnit,
    restart :: SeekRestart
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

json ''SeekFlags
