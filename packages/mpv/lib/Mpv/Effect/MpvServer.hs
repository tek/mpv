module Mpv.Effect.MpvServer where

data MpvServer command :: Effect where
  Send :: command a -> MpvServer command m a
  Terminate :: MpvServer command m ()

makeSem ''MpvServer
