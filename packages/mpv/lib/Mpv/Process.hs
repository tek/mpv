{-# language TransformListComp #-}

module Mpv.Process where

import Path (Abs, File, Path, Rel, parent, relfile, toFilePath, (</>))
import Path.IO (createTempDir, getTempDir, removeDirRecur)
import Polysemy.Error (fromExceptionSem)
import Polysemy.Resource (bracket)
import System.Process.Typed (Process, ProcessConfig, proc, startProcess, stopProcess)

import qualified Mpv.Data.MpvError as MpvError
import Mpv.Data.MpvError (MpvError)
import Mpv.Data.MpvProcess (MpvProcess (MpvProcess))

tempSocket ::
  Member (Embed IO) r =>
  Sem r (Path Abs File)
tempSocket = do
  systemTmp <- getTempDir
  dir <- createTempDir systemTmp "mpv-hs-"
  pure (dir </> [relfile|ipc|])

mpvProc ::
  Member (Embed IO) r =>
  Sem r (Path Abs File, ProcessConfig () () ())
mpvProc = do
  sock <- tempSocket
  let
    args = [
      [exon|--input-ipc-server=#{toFilePath sock}|],
      "--idle=once",
      "--no-terminal"
      ]
  pure (sock, proc "mpv" args)

startMpvProcess ::
  Members [Error SomeException, Embed IO, Final IO] r =>
  Sem r (Path Abs File, Process () () ())
startMpvProcess =
  fromExceptionSem @SomeException do
    (sock, conf) <- mpvProc
    prc <- embed (startProcess conf)
    pure (sock, prc)

spawn ::
  Members [Embed IO, Final IO] r =>
  Sem r (Either MpvError MpvProcess)
spawn = do
  runError @SomeException startMpvProcess >>= \case
    Right (sock, prc) ->
      pure (Right (MpvProcess sock prc))
    Left err ->
      pure (Left (MpvError.Fatal (show err)))

kill ::
  Members [Embed IO, Final IO] r =>
  Either MpvError MpvProcess ->
  Sem r ()
kill = \case
  Right (MpvProcess sock prc) -> do
    tryAny_ (stopProcess prc)
    tryAny_ (removeDirRecur (parent sock))
  Left _ ->
    unit

withMpvProcess ::
  Members [Resource, Embed IO, Final IO] r =>
  (Either MpvError MpvProcess -> Sem r a) ->
  Sem r a
withMpvProcess =
  bracket spawn kill
