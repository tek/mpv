module Mpv.Test.AsyncTest where

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
import Mpv.Interpreter.Mpv (interpretMpvNative)
import Mpv.Interpreter.MpvServer (withMpvClient, withMpvServer)

test_async :: UnitTest
test_async =
  (runTestAuto . asyncToIOFinal . interpretRace . interpretLogStdoutConc . interpretTimeGhc . interpretMpvNative) do
    vid <- Test.fixturePath [relfile|vid.mkv|]
    duration <- Race.timeout () (Seconds 4) do
      withMpvServer do
        assertEq (LoadResponse 1) =<< withMpvClient do
          resumeHoistError show do
            r <- Mpv.command (Command.Load vid)
            r <$ Mpv.command Command.Stop
        withMpvClient do
          resumeHoistError show do
            Mpv.command (Command.Load vid)
            duration <- Mpv.prop Property.Duration
            duration <$ Mpv.command Command.Stop
    assertRight 3.6 duration
