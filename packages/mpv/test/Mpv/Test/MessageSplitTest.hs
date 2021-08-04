module Mpv.Test.MessageSplitTest where

import Polysemy.Test (UnitTest, runTestAuto, (===))

import Mpv.SocketQueues (messageLines)

test_messageSplit :: UnitTest
test_messageSplit =
  runTestAuto do
    ([], "") === messageLines ""
    ([], "one") === messageLines "one"
    ([], "one") === messageLines "\none"
    (["one"], "") === messageLines "one\n"
    (["one", "two"], "") === messageLines "one\ntwo\n"
    (["one"], "two") === messageLines "one\n\ntwo"
    (["one", "two"], "three") === messageLines "one\ntwo\nthree"
