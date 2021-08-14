module Mpv.Class.CommandEvent where

import Mpv.Data.EventName (EventName)

class CommandEvent command where
  commandEvent :: command a -> Maybe EventName
