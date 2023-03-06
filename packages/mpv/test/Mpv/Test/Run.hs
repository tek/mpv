module Mpv.Test.Run where

import Hedgehog.Internal.Property (Failure)
import Log (Severity (Debug, Error), interpretLogStdoutLevelConc)
import Conc (interpretRace)
import Polysemy.Test (Hedgehog, Test, TestError, UnitTest, runTestAuto)
import Polysemy.Time (GhcTime, interpretTimeGhc)

import Mpv.Data.MpvError (MpvError)
import Mpv.Data.MpvProcessConfig (MpvProcessConfig)

type TestEffects =
  [
    Reader MpvProcessConfig,
    Stop MpvError,
    GhcTime,
    Log,
    Race,
    Async,
    Test,
    Fail,
    Error TestError,
    Hedgehog IO,
    Error Failure,
    Embed IO,
    Resource,
    Final IO
  ]

runTestLevel ::
  Severity ->
  Sem TestEffects () ->
  UnitTest
runTestLevel level =
  runTestAuto .
  asyncToIOFinal .
  interpretRace .
  interpretLogStdoutLevelConc (Just level) .
  interpretTimeGhc .
  stopToErrorWith show .
  runReader def

runTestDebug ::
  Sem TestEffects () ->
  UnitTest
runTestDebug =
  runTestLevel Debug

runTest ::
  Sem TestEffects () ->
  UnitTest
runTest =
  runTestLevel Error
