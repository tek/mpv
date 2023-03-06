{-# language CPP #-}

module Mpv.Test.VideoPlayerTest where

import Path (relfile)
import qualified Conc as Race
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq, assertJust)
import Polysemy.Time (Seconds (Seconds))

import qualified Mpv.Effect.VideoPlayer as VideoPlayer
import Mpv.Effect.VideoPlayer (VideoPlayer)
import Mpv.Interpreter.VideoPlayer (interpretVideoPlayerServer)
import Mpv.Test.Run (runTest)

#if !MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import Path (File, Rel)
#endif

test_videoPlayer :: UnitTest
test_videoPlayer =
  runTest do
    vid <- Test.fixturePath [relfile|vid.mkv|]
    duration <- Race.timeoutMaybe (Seconds 4) do
      interpretVideoPlayerServer do
        resumeHoistError @_ @(VideoPlayer _) show do
          _ <- VideoPlayer.load () vid
          d <- VideoPlayer.duration
          VideoPlayer.pause
          VideoPlayer.seekAbs 0.5
          assertEq 0.5 =<< VideoPlayer.progress
          d <$ VideoPlayer.stop
    assertJust 3.6 duration
