module Mpv.Test.ServerTest where

import Path (File, Rel, relfile)
import qualified Polysemy.Conc as Race
import Polysemy.Conc (interpretRace)
import Polysemy.Log.Colog (interpretLogStdoutConc)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq, assertRight, runTestAuto)
import Polysemy.Time (Seconds (Seconds), interpretTimeGhc)

import qualified Mpv.Data.Command as Command
import Mpv.Data.Command (LoadResponse (LoadResponse))
import qualified Mpv.Data.Property as Property
import qualified Mpv.Effect.Mpv as Mpv
import Mpv.Interpreter.MpvServer (interpretMpvClient, withMpvServer)

test_server :: UnitTest
test_server =
  (runTestAuto . asyncToIOFinal . interpretRace . interpretLogStdoutConc . interpretTimeGhc) do
    vid <- Test.fixturePath [relfile|vid.mkv|]
    duration <- Race.timeout () (Seconds 4) do
      (withMpvServer . interpretMpvClient . resumeHoistError show) do
        assertEq (LoadResponse 1) =<< Mpv.command (Command.Load vid)
        assertEq (LoadResponse 2) =<< Mpv.command (Command.Load vid)
        Mpv.command Command.Stop
        assertEq (LoadResponse 1) =<< Mpv.command (Command.Load vid)
        duration <- Mpv.prop Property.Duration
        duration <$ Mpv.command Command.Stop
    assertRight 3.6 duration
