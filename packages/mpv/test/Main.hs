module Main where

import Mpv.Test.MpvTest (test_mpv)
import Mpv.Test.ServerTest (test_server)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Polysemy.Test (unitTest)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "basic functionality" test_mpv,
    unitTest "client/server architecture" test_server
  ]

main :: IO ()
main =
  defaultMain tests
