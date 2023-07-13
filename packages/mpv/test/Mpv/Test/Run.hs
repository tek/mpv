module Mpv.Test.Run where

import Log (Severity (Debug, Error))
import Polysemy.Test (UnitTest)
import qualified Zeugma

import Mpv.Data.MpvError (MpvError)
import Mpv.Data.MpvProcessConfig (MpvProcessConfig)

type TestEffects =
  [
    Reader MpvProcessConfig,
    Stop MpvError
  ] ++ Zeugma.TestStack

runTestLevel ::
  HasCallStack =>
  Severity ->
  Sem TestEffects () ->
  UnitTest
runTestLevel level =
  Zeugma.runTestLevel level .
  stopToErrorWith show .
  runReader def

runTestDebug ::
  HasCallStack =>
  Sem TestEffects () ->
  UnitTest
runTestDebug =
  runTestLevel Debug

runTest ::
  HasCallStack =>
  Sem TestEffects () ->
  UnitTest
runTest =
  runTestLevel Error
