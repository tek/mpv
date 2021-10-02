module Mpv.Test.ServerTest where

import Path (Abs, File, Path, Rel, relfile)
import qualified Polysemy.Conc as Race
import qualified Polysemy.Test as Test
import Polysemy.Test (Hedgehog, UnitTest, assertEq, assertJust)
import Polysemy.Time (Seconds (Seconds))

import qualified Mpv.Data.Command as Command
import Mpv.Data.Command (LoadResponse (LoadResponse))
import qualified Mpv.Data.Property as Property
import Mpv.Data.VideoDuration (VideoDuration)
import qualified Mpv.Effect.Mpv as Mpv
import Mpv.Effect.Mpv (Mpv)
import Mpv.Interpreter.MpvServer (interpretMpvClient, withMpvServer)
import Mpv.Test.Run (runTest)

main ::
  Member (Hedgehog IO) r =>
  Path Abs File ->
  Sem (Mpv : r) VideoDuration
main vid = do
  assertEq (LoadResponse 1) =<< Mpv.command (Command.Load vid Nothing)
  assertEq (LoadResponse 2) =<< Mpv.command (Command.Load vid Nothing)
  Mpv.command Command.Stop
  assertEq (LoadResponse 1) =<< Mpv.command (Command.Load vid Nothing)
  duration <- Mpv.prop Property.Duration
  duration <$ Mpv.command Command.Stop

test_server :: UnitTest
test_server =
  runTest do
    vid <- Test.fixturePath [relfile|vid.mkv|]
    duration <- Race.timeoutMaybe (Seconds 4) do
      withMpvServer $ interpretMpvClient $ resumeHoistError show $ main vid
    assertJust 3.6 duration
