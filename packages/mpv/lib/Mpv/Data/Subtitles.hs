module Mpv.Data.Subtitles where

import Mpv.Data.Subtitle (Subtitle)
import Mpv.Data.SubtitleId (SubtitleId)

data Subtitles =
  Subtitles {
    active :: Maybe SubtitleId,
    subtitles :: [Subtitle]
  }
  deriving stock (Eq, Show, Generic)

json ''Subtitles
