module Mpv.Effect.VideoPlayer where

import Path (Abs, File, Path)
import Polysemy.Time (NanoSeconds, Seconds (Seconds), convert)

import Mpv.Data.AudioDelay (AudioDelay)
import Mpv.Data.AudioId (AudioId)
import Mpv.Data.AudioTracks (AudioTracks)
import Mpv.Data.MpvInfo (MpvInfo)
import Mpv.Data.PlaybackState (PlaybackState)
import Mpv.Data.SeekFlags (
  SeekFlags (SeekFlags),
  SeekReference (Absolute, Relative),
  SeekRestart (Exact),
  SeekUnit (Percent, Time),
  )
import Mpv.Data.SubDelay (SubDelay)
import Mpv.Data.SubFps (SubFps)
import Mpv.Data.SubtitleId (SubtitleId)
import Mpv.Data.Subtitles (Subtitles)
import Mpv.Data.VideoDuration (VideoDuration)
import Mpv.Data.VideoProgress (VideoProgress (VideoProgress))
import Mpv.Data.Volume (Volume)
import Mpv.Data.VideoExpired (VideoExpired)

data VideoPlayer (meta :: Type) :: Effect where
  Current :: VideoPlayer meta m (Maybe meta)
  Load :: meta -> Path Abs File -> VideoPlayer meta m ()
  Pause :: VideoPlayer meta m PlaybackState
  Stop :: VideoPlayer meta m ()
  Seek :: Double -> SeekFlags -> VideoPlayer meta m ()
  Info :: VideoPlayer meta m MpvInfo
  Subtitles :: VideoPlayer meta m Subtitles
  SetSubtitle :: SubtitleId -> VideoPlayer meta m ()
  SubDelay :: VideoPlayer meta m SubDelay
  SetSubDelay :: SubDelay -> VideoPlayer meta m ()
  AddSubDelay :: SubDelay -> VideoPlayer meta m SubDelay
  SubFps :: VideoPlayer meta m SubFps
  SetSubFps :: SubFps -> VideoPlayer meta m ()
  Volume :: VideoPlayer meta m Volume
  SetVolume :: Volume -> VideoPlayer meta m ()
  AdjustVolumeBy :: Volume -> VideoPlayer meta m Volume
  Audios :: VideoPlayer meta m AudioTracks
  SetAudio :: AudioId -> VideoPlayer meta m ()
  AudioDelay :: VideoPlayer meta m AudioDelay
  SetAudioDelay :: AudioDelay -> VideoPlayer meta m ()
  AddAudioDelay :: AudioDelay -> VideoPlayer meta m AudioDelay
  Duration :: VideoPlayer meta m VideoDuration
  Progress :: VideoPlayer meta m VideoProgress
  Expired :: VideoPlayer meta m VideoExpired
  SetOption :: Text -> Text -> VideoPlayer meta m ()

makeSem ''VideoPlayer

seekRel ::
  Member (VideoPlayer meta) r =>
  NanoSeconds ->
  Sem r ()
seekRel (convert -> Seconds secs) =
  seek (fromIntegral secs) (SeekFlags Relative Time def)

seekAbs ::
  Member (VideoPlayer meta) r =>
  VideoProgress ->
  Sem r ()
seekAbs (VideoProgress pct) =
  seek pct (SeekFlags Absolute Percent Exact)
