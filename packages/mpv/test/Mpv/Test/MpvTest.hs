{-# language CPP #-}

module Mpv.Test.MpvTest where

import qualified Data.List.NonEmpty as NonEmpty
import Path (relfile)
import qualified Polysemy.Conc as Race
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertClose, assertEq)
import qualified Polysemy.Time as Time
import Polysemy.Time (Seconds (Seconds))

import qualified Mpv.Data.Command as Command
import qualified Mpv.Data.OsdLevel as OsdLevel
import qualified Mpv.Data.Property as Property
import qualified Mpv.Data.SeekFlags as SeekFlags
import Mpv.Data.SeekFlags (SeekFlags (SeekFlags), SeekReference (Absolute), SeekRestart (Exact))
import Mpv.Data.Track (Track (Track), TrackList (TrackList), TrackType (Audio, Sub, Video))
import qualified Mpv.Effect.Mpv as Mpv
import Mpv.Effect.Mpv (Mpv, withMpv)
import Mpv.Interpreter.Mpv (interpretMpvNative)
import qualified Mpv.Mpv as Mpv
import Mpv.Mpv (addAudioDelay, adjustVolumeBy, setDefaultOptions, togglePlaybackState)
import Mpv.Test.Run (runTest)

#if !MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import Path (File, Rel)
#endif

trackList :: NonEmpty Track
trackList =
  [
    Track (Just 1) False (Just "fre") Audio,
    Track (Just 1) True Nothing Video,
    Track (Just 1) True (Just "eng") Sub,
    Track (Just 2) False (Just "ger") Sub,
    Track (Just 2) True Nothing Audio
  ]

test_mpv :: UnitTest
test_mpv =
  (runTest . interpretMpvNative) do
    vid <- Test.fixturePath [relfile|vid.mkv|]
    Race.timeoutU (Seconds 4) do
      restop $ withMpv do
        resumeHoistError @_ @Mpv show do
          setDefaultOptions
          Mpv.command (Command.Load vid def)
          assertEq 3.6 =<< Mpv.prop Property.Duration
          assertEq 0 =<< Mpv.prop Property.SubFps
          assertEq "message vid.mkv" =<< Mpv.command (Command.Manual Nothing "expand-text" ["message ${filename}"])
          Mpv.command (Command.ShowText "message" (Seconds 2) OsdLevel.SubtitlesOnly)
          Time.sleep (Seconds 2)
          Mpv.setProp Property.SubFps 100
          Mpv.setProp Property.SubDelay 1
          assertEq 100 =<< Mpv.prop Property.SubFps
          assertEq trackList . NonEmpty.sort . coerce =<< Mpv.prop Property.TrackList
          void Mpv.info
          void togglePlaybackState
          Mpv.setProp Property.Volume 100
          assertClose 95 =<< adjustVolumeBy (-5)
          void (addAudioDelay 5)
          Mpv.command (Command.Seek 50 (SeekFlags Absolute SeekFlags.Percent Exact))
          void $ Mpv.command Command.Stop
