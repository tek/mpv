module Mpv.Data.Request where

import Data.SOP (All, Compose, I)

import Mpv.Data.Command (CommandArgs)
import Mpv.Data.RequestId (RequestId)
import Prelude hiding (All, Compose)

data Request as =
  Request {
    request_id :: RequestId,
    command :: CommandArgs as
  }
  deriving (Generic)

deriving instance All (Compose Eq I) as => Eq (Request as)
deriving instance All (Compose Show I) as => Show (Request as)
deriving instance All ToJSON as => ToJSON (Request as)
