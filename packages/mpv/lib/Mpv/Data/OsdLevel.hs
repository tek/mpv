module Mpv.Data.OsdLevel where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withScientific)
import Exon (exon)

data OsdLevel =
  SubtitlesOnly
  |
  UserInteraction
  |
  CurrentTime
  |
  CurrentTimeAndStatus
  deriving stock (Eq, Show)

instance ToJSON OsdLevel where
  toJSON =
    toJSON @Int . \case
      SubtitlesOnly -> 0
      UserInteraction -> 1
      CurrentTime -> 2
      CurrentTimeAndStatus -> 3

instance FromJSON OsdLevel where
  parseJSON =
    withScientific "OsdLevel" \case
      0 -> pure SubtitlesOnly
      1 -> pure UserInteraction
      2 -> pure CurrentTime
      3 -> pure CurrentTimeAndStatus
      v -> fail [exon|invalid number for OsdLevel: #{show v}|]
