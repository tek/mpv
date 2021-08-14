module Mpv.Test.LoadTwiceTest where

import Path (File, Rel, relfile)
import qualified Polysemy.Conc as Race
import Polysemy.Conc (interpretRace)
import Polysemy.Log.Colog (interpretLogStdoutConc)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, runTestAuto, assertEq)
import Polysemy.Time (Seconds (Seconds), interpretTimeGhc)

import qualified Mpv.Data.Command as Command
import qualified Mpv.Data.Property as Property
import qualified Mpv.Effect.Mpv as Mpv
import Mpv.Interpreter.Mpv (interpretMpvNative)
import Mpv.Interpreter.MpvServer (withMpvServer, withMpvClient)

test_loadTwice :: UnitTest
test_loadTwice =
  (runTestAuto . asyncToIOFinal . interpretRace . interpretLogStdoutConc . interpretTimeGhc . interpretMpvNative) do
    vid <- Test.fixturePath [relfile|vid.mkv|]
    Race.timeout_ () (Seconds 4) do
      void $ withMpvServer do
        void $ withMpvClient do
          resumeHoistError show do
            void $ Mpv.command (Command.Load vid)
        withMpvClient do
          resumeHoistError show do
            Mpv.command (Command.Load vid)
            assertEq 3.6 =<< Mpv.prop Property.Duration
            Mpv.command Command.Stop
