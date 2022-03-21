module Mpv.Data.MpvProcessConfig where

import Path (Abs, File, Path)
import Polysemy.Time.Json (json)

data MpvProcessConfig =
  MpvProcessConfig {
    executable :: Maybe (Path Abs File)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Default)

json ''MpvProcessConfig
