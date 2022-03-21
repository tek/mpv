module Mpv.Data.MpvError where

import Polysemy.Time.Json (json)

data MpvError =
  MpvError { error :: Text }
  |
  Fatal { exception :: Text }
  deriving stock (Eq, Show)

json ''MpvError
