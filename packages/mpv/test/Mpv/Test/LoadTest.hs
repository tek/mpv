module Mpv.Test.LoadTest where

import qualified Data.List.NonEmpty as NonEmpty
import Path (File, Rel, relfile)
import qualified Polysemy.Conc as Race
import qualified Polysemy.Conc as Conc
import Polysemy.Conc (interpretRace, withAsync_)
import qualified Polysemy.Log as Log
import Polysemy.Log.Colog (interpretLogStdoutConc)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq, runTestAuto)
import Polysemy.Time (Seconds (Seconds), interpretTimeGhc)

import qualified Mpv.Data.Command as Command
import qualified Mpv.Data.Property as Property
import qualified Mpv.Data.SeekFlags as SeekFlags
import Mpv.Data.SeekFlags (SeekFlags (SeekFlags), SeekReference (Absolute), SeekRestart (Exact))
import Mpv.Data.Track (Track (Track), TrackList (TrackList), TrackType (Audio, Sub, Video))
import qualified Mpv.Effect.Mpv as Mpv
import qualified Mpv.Interpreter.Mpv as Mpv
import Mpv.Interpreter.Mpv (interpretMpvNative, withMpv)
import Mpv.Mpv (pause, setDefaultOptions)

trackList :: NonEmpty Track
trackList =
  [
    Track (Just 1) False Nothing Audio,
    Track (Just 1) True Nothing Sub,
    Track (Just 1) True Nothing Video,
    Track (Just 2) False Nothing Sub,
    Track (Just 2) True Nothing Audio
  ]

test_loadFile :: UnitTest
test_loadFile =
  (runTestAuto . asyncToIOFinal . interpretRace . interpretLogStdoutConc . interpretTimeGhc . interpretMpvNative) do
    vid <- Test.fixturePath [relfile|vid.mkv|]
    Race.timeout_ () (Seconds 4) do
      withMpv do
        resumeHoistError show do
          withAsync_ (Mpv.loopEvents (Log.debug . show =<< Conc.consume)) do
            setDefaultOptions
            Mpv.command (Command.Load vid)
            assertEq 3.6 =<< Mpv.prop Property.Duration
            assertEq 0 =<< Mpv.prop Property.SubFps
            Mpv.setProp Property.SubFps 100
            assertEq 100 =<< Mpv.prop Property.SubFps
            assertEq trackList . NonEmpty.sort . coerce =<< Mpv.prop Property.TrackList
            void pause
            Mpv.command (Command.Seek 50 (SeekFlags Absolute SeekFlags.Percent Exact))
            void $ Mpv.command Command.Stop
