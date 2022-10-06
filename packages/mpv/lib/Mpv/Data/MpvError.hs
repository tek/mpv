module Mpv.Data.MpvError where

data MpvError =
  MpvError { error :: Text }
  |
  Fatal { exception :: Text }
  deriving stock (Eq, Show)

json ''MpvError
