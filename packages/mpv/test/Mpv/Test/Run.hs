module Mpv.Test.Run where

import Hedgehog.Internal.Property (Failure)
import Polysemy.Conc (interpretRace)
import Polysemy.Log (interpretLogStdoutConc)
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

runTest ::
  Sem TestEffects () ->
  UnitTest
runTest =
  runTestAuto .
  asyncToIOFinal .
  interpretRace .
  interpretLogStdoutConc .
  interpretTimeGhc .
  stopToErrorWith show .
  runReader def
