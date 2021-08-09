module Mpv.Test.LoadTest where

import Path (File, Rel, relfile)
import qualified Polysemy.Conc as Race
import Polysemy.Conc (interpretRace)
import Polysemy.Log.Colog (interpretLogStdoutConc)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq, runTestAuto)
import Polysemy.Time (Seconds (Seconds), interpretTimeGhc)

import qualified Mpv.Data.Command as Command
import qualified Mpv.Data.Property as Property
import qualified Mpv.Data.SeekFlags as SeekFlags
import Mpv.Data.SeekFlags (SeekFlags (SeekFlags), SeekReference (Absolute), SeekRestart (Exact))
import qualified Mpv.Effect.Mpv as Mpv
import Mpv.Interpreter.Mpv (interpretMpvNative, withMpv)

test_loadFile :: UnitTest
test_loadFile =
  (runTestAuto . asyncToIOFinal . interpretRace . interpretLogStdoutConc . interpretTimeGhc) do
    vid <- Test.fixturePath [relfile|vid.mkv|]
    interpretMpvNative do
      Race.timeout_ () (Seconds 4) do
        withMpv do
          resumeHoistError show do
            Mpv.command (Command.Load vid)
            assertEq 3.6 =<< Mpv.prop Property.Duration
            assertEq 0 =<< Mpv.prop Property.SubFps
            Mpv.setProp Property.SubFps 100
            assertEq 100 =<< Mpv.prop Property.SubFps
            Mpv.command (Command.Seek 50 (SeekFlags Absolute SeekFlags.Percent Exact))
            void $ Mpv.command Command.Stop
