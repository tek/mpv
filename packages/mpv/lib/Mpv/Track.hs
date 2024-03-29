module Mpv.Track where

import Lens.Micro.Extras (view)

import Mpv.Data.AudioId (AudioId (AudioId))
import Mpv.Data.AudioTrack (AudioTrack (AudioTrack))
import Mpv.Data.AudioTracks (AudioTracks (AudioTracks))
import qualified Mpv.Data.Property as Property
import Mpv.Data.Subtitle (Subtitle (Subtitle))
import Mpv.Data.SubtitleId (SubtitleId (SubtitleId))
import Mpv.Data.Subtitles (Subtitles (Subtitles))
import qualified Mpv.Data.Track as Track
import Mpv.Data.Track (Track (Track), TrackList (TrackList))
import Mpv.Data.VideoId (VideoId (VideoId))
import Mpv.Data.VideoTrack (VideoTrack (VideoTrack))
import Mpv.Data.VideoTracks (VideoTracks (VideoTracks))
import qualified Mpv.Effect.Mpv as Mpv
import Mpv.Effect.Mpv (Mpv)

toVideo :: Track -> Maybe VideoTrack
toVideo = \case
  Track id' sel _ Track.Video -> Just (VideoTrack (VideoId <$> id') sel)
  _ -> Nothing

toAudio :: Track -> Maybe AudioTrack
toAudio = \case
  Track id' sel lang Track.Audio -> Just (AudioTrack (AudioId <$> id') sel lang)
  _ -> Nothing

toSubtitle :: Track -> Maybe Subtitle
toSubtitle = \case
  Track id' sel lang Track.Sub -> Just (Subtitle (SubtitleId <$> id') sel lang)
  _ -> Nothing

trackIdIfSelected :: Track -> Maybe Int
trackIdIfSelected = \case
  Track id' True _ _ -> id'
  _ -> Nothing

trackType ::
  Integral i =>
  (Track -> Maybe a) ->
  (Maybe i -> [a] -> b) ->
  [Track] ->
  b
trackType tpe grp ts =
  grp (fromIntegral <$> firstJust trackIdIfSelected matchingTracks) finalTracks
  where
    (matchingTracks, finalTracks) =
      unzip (mapMaybe withTrack ts)
    withTrack a =
      (a,) <$> tpe a

splitTracks ::
  [Track] ->
  (VideoTracks, AudioTracks, Subtitles)
splitTracks ts =
  (trackType toVideo VideoTracks ts, trackType toAudio AudioTracks ts, trackType toSubtitle Subtitles ts)

tracks ::
  Member Mpv r =>
  Sem r (VideoTracks, AudioTracks, Subtitles)
tracks = do
  TrackList allTracks <- Mpv.prop Property.TrackList
  pure (splitTracks (toList allTracks))

videoTracks ::
  Member Mpv r =>
  Sem r VideoTracks
videoTracks =
  view _1 <$> tracks

audioTracks ::
  Member Mpv r =>
  Sem r AudioTracks
audioTracks =
  view _2 <$> tracks

subtitles ::
  Member Mpv r =>
  Sem r Subtitles
subtitles =
  view _3 <$> tracks
