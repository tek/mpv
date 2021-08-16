module Main where

import Mpv.Test.MessageSplitTest (test_messageSplit)
import Mpv.Test.MpvTest (test_mpv)
import Mpv.Test.ServerTest (test_server)
import Mpv.Test.VideoPlayerTest (test_videoPlayer)
import Polysemy.Test (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (wrapTest)
import Test.Tasty.Providers (testPassed)

ignoreIfNoX :: TestTree -> TestTree
ignoreIfNoX =
  wrapTest \ test ->
    maybe skip (const test) =<< lookupEnv "XDG_RUNTIME_DIR"
  where
    skip =
      pure (testPassed "mpv test cannot be run without $XDG_RUNTIME_DIR")

mpvTests :: TestTree
mpvTests =
  ignoreIfNoX $
  testGroup "mpv process" [
    unitTest "basic functionality" test_mpv,
    unitTest "client/server architecture" test_server,
    unitTest "video player effect" test_videoPlayer
  ]

tests :: TestTree
tests =
  testGroup "all" [
    mpvTests,
    unitTest "split socket messages on newlines" test_messageSplit
  ]

main :: IO ()
main =
  defaultMain tests
