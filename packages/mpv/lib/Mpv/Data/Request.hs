module Mpv.Data.Request where

import Data.SOP (All, Compose, I)
import Prelude hiding (All, Compose)

import Mpv.Data.Command (CommandArgs)
import Mpv.Data.RequestId (RequestId)

data Request as =
  Request {
    request_id :: RequestId,
    command :: CommandArgs as,
    async :: Bool
  }
  deriving (Generic)

deriving instance All (Compose Eq I) as => Eq (Request as)
deriving instance All (Compose Show I) as => Show (Request as)
deriving instance All ToJSON as => ToJSON (Request as)
