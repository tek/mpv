module Mpv.Interpreter.VideoPlayer where

import Polysemy.Conc (ChanConsumer, EventConsumer, interpretAtomic)
import Polysemy.Log (Log)
import Polysemy.Time (Time)

import Mpv.Data.AudioId (AudioId (AudioId))
import qualified Mpv.Data.Command as Command
import Mpv.Data.Command (Command)
import qualified Mpv.Data.MpvError as MpvError
import Mpv.Data.MpvError (MpvError (MpvError))
import Mpv.Data.MpvEvent (MpvEvent)
import Mpv.Data.MpvProcessConfig (MpvProcessConfig)
import qualified Mpv.Data.PlayerError as PlayerError
import Mpv.Data.PlayerError (PlayerError (PlayerError))
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

mpvError :: MpvError -> PlayerError
mpvError = \case
  MpvError err ->
    PlayerError err
  MpvError.Fatal err ->
    PlayerError.Fatal err

hoistError ::
  Members [Mpv !! MpvError, Stop PlayerError] r =>
  InterpreterFor Mpv r
hoistError =
  resumeHoist mpvError

hoistError_ ::
  Members [Mpv !! MpvError, Stop PlayerError] r =>
  Sem (Mpv : r) a ->
  Sem r ()
hoistError_ =
  void . hoistError

interpretVideoPlayerMpvAtomic ::
  ∀ meta r .
  Members [Mpv !! MpvError, AtomicState (Maybe meta), Race] r =>
  InterpreterFor (VideoPlayer meta !! PlayerError) r
interpretVideoPlayerMpvAtomic =
  interpretResumable \case
    VideoPlayer.Current ->
      atomicGet
    VideoPlayer.Load meta file -> do
      void $ hoistError (Mpv.command (Command.Load file Nothing))
      atomicPut (Just meta)
    VideoPlayer.Pause ->
      hoistError togglePlaybackState
    VideoPlayer.Stop ->
      hoistError_ (Mpv.command Command.Stop)
    VideoPlayer.Seek pos flags ->
      hoistError_ (Mpv.command (Command.Seek pos flags))
    VideoPlayer.Info ->
      hoistError info
    VideoPlayer.Subtitles ->
      hoistError subtitles
    VideoPlayer.SetSubtitle (SubtitleId id') ->
      hoistError (Mpv.setOption "sub" (show id'))
    VideoPlayer.SubFps ->
      hoistError (Mpv.prop Property.SubFps)
    VideoPlayer.SetSubFps fps ->
      hoistError (Mpv.setProp Property.SubFps fps)
    VideoPlayer.SubDelay ->
      hoistError (Mpv.prop Property.SubDelay)
    VideoPlayer.SetSubDelay delay ->
      hoistError (Mpv.setProp Property.SubDelay delay)
    VideoPlayer.AddSubDelay delta ->
      hoistError (addSubDelay delta)
    VideoPlayer.Audios ->
      hoistError audioTracks
    VideoPlayer.SetAudio (AudioId id') ->
      hoistError (Mpv.setOption "audio" (show id'))
    VideoPlayer.AudioDelay ->
      hoistError (Mpv.prop Property.AudioDelay)
    VideoPlayer.SetAudioDelay delay ->
      hoistError (Mpv.setProp Property.AudioDelay delay)
    VideoPlayer.AddAudioDelay delta ->
      hoistError (addAudioDelay delta)
    VideoPlayer.Volume ->
      hoistError (Mpv.prop Property.Volume)
    VideoPlayer.SetVolume value ->
      hoistError (Mpv.setProp Property.Volume value)
    VideoPlayer.AdjustVolumeBy delta ->
      hoistError (adjustVolumeBy delta)
    VideoPlayer.Duration ->
      hoistError (Mpv.prop Property.Duration)
    VideoPlayer.Progress ->
      hoistError (Mpv.prop Property.PercentPos)
    VideoPlayer.Expired ->
      hoistError (Mpv.prop Property.TimePos)

interpretVideoPlayer ::
  ∀ meta token r .
  Members [MpvServer Command !! MpvError, EventConsumer token MpvEvent, Log, Resource, Async, Race, Embed IO] r =>
  InterpreterFor (VideoPlayer meta !! PlayerError) r
interpretVideoPlayer =
  interpretAtomic Nothing . interpretMpvClient . interpretVideoPlayerMpvAtomic . raiseUnder2

interpretVideoPlayerServer ::
  Members [Reader MpvProcessConfig, Log, Resource, Async, Race, Time t d, Embed IO, Final IO] r =>
  InterpretersFor [VideoPlayer meta !! PlayerError, ChanConsumer MpvEvent] r
interpretVideoPlayerServer =
  withMpvServer . interpretVideoPlayer . raiseUnder
