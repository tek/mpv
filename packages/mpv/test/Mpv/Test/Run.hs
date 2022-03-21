module Mpv.Test.Run where

import Polysemy.Conc (interpretRace)
import Polysemy.Log (interpretLogStdoutConc)
import Polysemy.Test (Hedgehog, Test, TestError, UnitTest, runTestAuto)
import Polysemy.Time (GhcTime, interpretTimeGhc)

import Mpv.Data.MpvProcessConfig (MpvProcessConfig)

type TestEffects =
  [
    Reader MpvProcessConfig,
    GhcTime,
    Log,
    Race,
    Async,
    Test,
    Fail,
    Error TestError,
    Hedgehog IO,
    Embed IO,
    Resource,
    Final IO
  ]

runTest ::
  Sem TestEffects () ->
  UnitTest
runTest =
  runTestAuto .
  asyncToIOFinal .
  interpretRace .
  interpretLogStdoutConc .
  interpretTimeGhc .
  runReader def
