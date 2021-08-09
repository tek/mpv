{-# language TransformListComp #-}

module Mpv.Process where

import Path (Abs, File, Path, Rel, parent, relfile, toFilePath, (</>))
import Path.IO (createTempDir, getTempDir, removeDirRecur)
import Polysemy.Resource (bracket)
import System.Process.Typed (Process, ProcessConfig, proc, startProcess, stopProcess)

import qualified Mpv.Data.MpvError as MpvError
import Mpv.Data.MpvError (MpvError)

tempSocket ::
  Member (Embed IO) r =>
  Sem r (Path Abs File)
tempSocket = do
  systemTmp <- getTempDir
  dir <- createTempDir systemTmp "mpv-hs-"
  pure (dir </> [relfile|ipc|])

withTempSocketPath ::
  Members [Resource, Embed IO] r =>
  (Path Abs File -> Sem r a) ->
  Sem r a
withTempSocketPath =
  bracket tempSocket (tryAny_ . removeDirRecur . parent)

mpvProc ::
  Path Abs File ->
  ProcessConfig () () ()
mpvProc socket =
  proc "mpv" [
    [exon|--input-ipc-server=#{toFilePath socket}|],
    "--idle=once",
    "--no-terminal"
  ]

startMpvProcess ::
  Members [Embed IO, Final IO] r =>
  Path Abs File ->
  Sem r (Either MpvError (Process () () ()))
startMpvProcess socket =
  mapLeft MpvError.Fatal <$> tryAny (startProcess (mpvProc socket))

kill ::
  Members [Embed IO, Final IO] r =>
  Either MpvError (Process () () ()) ->
  Sem r ()
kill =
  traverse_ (tryAny_ . stopProcess)

withMpvProcess ::
  Members [Resource, Embed IO, Final IO] r =>
  Path Abs File ->
  (Either MpvError (Process () () ()) -> Sem r a) ->
  Sem r a
withMpvProcess socket =
  bracket (startMpvProcess socket) kill

withMpvProcessAndSocket ::
  Members [Resource, Embed IO, Final IO] r =>
  (Either MpvError (Path Abs File) -> Sem r a) ->
  Sem r a
withMpvProcessAndSocket run = do
  withTempSocketPath \ socket ->
    withMpvProcess socket \ prc ->
      run (socket <$ prc)
