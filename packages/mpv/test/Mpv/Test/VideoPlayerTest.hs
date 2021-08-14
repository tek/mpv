module Mpv.Test.VideoPlayerTest where

import Path (File, Rel, relfile)
import qualified Polysemy.Conc as Race
import Polysemy.Conc (interpretRace)
import Polysemy.Log.Colog (interpretLogStdoutConc)
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertRight, runTestAuto)
import Polysemy.Time (Seconds (Seconds), interpretTimeGhc)

import qualified Mpv.Effect.VideoPlayer as VideoPlayer
import Mpv.Interpreter.MpvServer (withMpvServer)
import Mpv.Interpreter.VideoPlayer (interpretVideoPlayer)

test_videoPlayer :: UnitTest
test_videoPlayer =
  (runTestAuto . asyncToIOFinal . interpretRace . interpretLogStdoutConc . interpretTimeGhc) do
    vid <- Test.fixturePath [relfile|vid.mkv|]
    duration <- Race.timeout () (Seconds 4) do
      withMpvServer do
        interpretVideoPlayer do
          resumeHoistError show do
            _ <- VideoPlayer.load () vid
            d <- VideoPlayer.duration
            d <$ VideoPlayer.stop
    assertRight 3.6 duration
