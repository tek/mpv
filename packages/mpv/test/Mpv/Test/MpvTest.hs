module Mpv.Test.MpvTest where

import qualified Data.List.NonEmpty as NonEmpty
import Path (File, Rel, relfile)
import qualified Polysemy.Conc as Race
import qualified Polysemy.Conc as Conc
import Polysemy.Conc (interpretRace, withAsync_)
import qualified Polysemy.Log as Log
import Polysemy.Log.Colog (interpretLogStdoutConc)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertClose, assertEq, runTestAuto)
import Polysemy.Time (Seconds (Seconds), interpretTimeGhc)

import qualified Mpv.Data.Command as Command
import qualified Mpv.Data.Property as Property
import qualified Mpv.Data.SeekFlags as SeekFlags
import Mpv.Data.SeekFlags (SeekFlags (SeekFlags), SeekReference (Absolute), SeekRestart (Exact))
import Mpv.Data.Track (Track (Track), TrackList (TrackList), TrackType (Audio, Sub, Video))
import qualified Mpv.Effect.Mpv as Mpv
import qualified Mpv.Interpreter.Mpv as Mpv
import Mpv.Interpreter.Mpv (interpretMpvNative, withMpv)
import qualified Mpv.Mpv as Mpv
import Mpv.Mpv (addAudioDelay, adjustVolumeBy, setDefaultOptions, togglePlaybackState)

trackList :: NonEmpty Track
trackList =
  [
    Track (Just 1) False Nothing Audio,
    Track (Just 1) True Nothing Sub,
    Track (Just 1) True Nothing Video,
    Track (Just 2) False Nothing Sub,
    Track (Just 2) True Nothing Audio
  ]

test_mpv :: UnitTest
test_mpv =
  (runTestAuto . asyncToIOFinal . interpretRace . interpretLogStdoutConc . interpretTimeGhc . interpretMpvNative) do
    vid <- Test.fixturePath [relfile|vid.mkv|]
    Race.timeout_ () (Seconds 4) do
      withMpv do
        resumeHoistError show do
          withAsync_ (subscribeLoop (Log.debug . show)) do
            setDefaultOptions
            Mpv.command (Command.Load vid)
            assertEq 3.6 =<< Mpv.prop Property.Duration
            assertEq 0 =<< Mpv.prop Property.SubFps
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
