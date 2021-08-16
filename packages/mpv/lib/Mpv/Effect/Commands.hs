module Mpv.Effect.Commands where

import Mpv.Data.RequestId (RequestId)
import Mpv.Data.Response (ResponseError)

data Commands (fmt :: Type) (command :: Type -> Type) :: Effect where
  Decode :: command a -> fmt -> Commands fmt command m (Either ResponseError a)
  Encode :: RequestId -> Bool -> command a -> Commands fmt command m fmt

makeSem ''Commands
