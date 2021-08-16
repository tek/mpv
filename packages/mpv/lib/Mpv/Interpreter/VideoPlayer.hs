module Mpv.Interpreter.VideoPlayer where

import Polysemy.Conc (ChanConsumer, EventConsumer, interpretAtomic)
import Polysemy.Log (Log)
import Polysemy.Time (Time)

import Mpv.Data.AudioId (AudioId (AudioId))
import qualified Mpv.Data.Command as Command
import Mpv.Data.Command (Command)
import Mpv.Data.MpvError (MpvError)
import Mpv.Data.MpvEvent (MpvEvent)
import Mpv.Data.MpvProcessConfig (MpvProcessConfig)
import qualified Mpv.Data.Property as Property
import Mpv.Data.SubtitleId (SubtitleId (SubtitleId))
import qualified Mpv.Effect.Mpv as Mpv
import Mpv.Effect.Mpv (Mpv)
import Mpv.Effect.MpvServer (MpvServer)
import qualified Mpv.Effect.VideoPlayer as VideoPlayer
import Mpv.Effect.VideoPlayer (VideoPlayer)
import Mpv.Interpreter.MpvServer (interpretMpvClient, withMpvServer)
import Mpv.Mpv (addAudioDelay, addSubDelay, adjustVolumeBy, info, togglePlaybackState)
import Mpv.Track (audioTracks, subtitles)

interpretVideoPlayerMpvAtomic ::
  ∀ meta r .
  Members [Mpv !! MpvError, AtomicState (Maybe meta), Race] r =>
  InterpreterFor (VideoPlayer meta !! MpvError) r
interpretVideoPlayerMpvAtomic =
  interpretResumable \case
    VideoPlayer.Current ->
      atomicGet
    VideoPlayer.Load meta file -> do
      void $ restop (Mpv.command (Command.Load file Nothing))
      atomicPut (Just meta)
    VideoPlayer.Pause ->
      restop togglePlaybackState
    VideoPlayer.Stop ->
      void (restop (Mpv.command Command.Stop))
    VideoPlayer.Seek pos flags ->
      void (restop (Mpv.command (Command.Seek pos flags)))
    VideoPlayer.Info ->
      restop info
    VideoPlayer.Subtitles ->
      restop subtitles
    VideoPlayer.SetSubtitle (SubtitleId id') ->
      restop (Mpv.setOption "sub" (show id'))
    VideoPlayer.SubFps ->
      restop (Mpv.prop Property.SubFps)
    VideoPlayer.SetSubFps fps ->
      restop (Mpv.setProp Property.SubFps fps)
    VideoPlayer.SubDelay ->
      restop (Mpv.prop Property.SubDelay)
    VideoPlayer.SetSubDelay delay ->
      restop (Mpv.setProp Property.SubDelay delay)
    VideoPlayer.AddSubDelay delta ->
      restop (addSubDelay delta)
    VideoPlayer.Audios ->
      restop audioTracks
    VideoPlayer.SetAudio (AudioId id') ->
      restop (Mpv.setOption "audio" (show id'))
    VideoPlayer.AudioDelay ->
      restop (Mpv.prop Property.AudioDelay)
    VideoPlayer.SetAudioDelay delay ->
      restop (Mpv.setProp Property.AudioDelay delay)
    VideoPlayer.AddAudioDelay delta ->
      restop (addAudioDelay delta)
    VideoPlayer.Volume ->
      restop (Mpv.prop Property.Volume)
    VideoPlayer.SetVolume value ->
      restop (Mpv.setProp Property.Volume value)
    VideoPlayer.AdjustVolumeBy delta ->
      restop (adjustVolumeBy delta)
    VideoPlayer.Duration ->
      restop (Mpv.prop Property.Duration)
    VideoPlayer.Progress ->
      restop (Mpv.prop Property.PercentPos)
    VideoPlayer.Expired ->
      restop (Mpv.prop Property.TimePos)

interpretVideoPlayer ::
  ∀ meta token r .
  Members [MpvServer Command !! MpvError, EventConsumer token MpvEvent, Log, Resource, Async, Race, Embed IO] r =>
  InterpreterFor (VideoPlayer meta !! MpvError) r
interpretVideoPlayer =
  interpretAtomic Nothing . interpretMpvClient . interpretVideoPlayerMpvAtomic . raiseUnder2

interpretVideoPlayerServer ::
  Members [Reader MpvProcessConfig, Log, Resource, Async, Race, Time t d, Embed IO, Final IO] r =>
  InterpretersFor [VideoPlayer meta !! MpvError, ChanConsumer MpvEvent] r
interpretVideoPlayerServer =
  withMpvServer . interpretVideoPlayer . raiseUnder
