module Mpv.Class.CommandEvent where

import Mpv.Data.MpvEvent (EventName)

class CommandEvent command where
  commandEvent :: command a -> Maybe EventName
