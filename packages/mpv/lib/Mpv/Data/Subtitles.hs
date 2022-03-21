module Mpv.Data.Subtitles where

import Polysemy.Time.Json (json)

import Mpv.Data.Subtitle (Subtitle)
import Mpv.Data.SubtitleId (SubtitleId)

data Subtitles =
  Subtitles {
    active :: Maybe SubtitleId,
    subtitles :: [Subtitle]
  }
  deriving stock (Eq, Show, Generic)

json ''Subtitles
