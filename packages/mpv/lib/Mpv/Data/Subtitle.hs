module Mpv.Data.Subtitle where

import Mpv.Data.SubtitleId (SubtitleId)

data Subtitle =
  Subtitle {
    id :: Maybe SubtitleId,
    selected :: Bool,
    language :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

json ''Subtitle
