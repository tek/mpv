module Mpv.Process where

import Exon (exon)
import Path (Abs, File, Path, parent, relfile, toFilePath, (</>))
import qualified Path.IO as Path
import Path.IO (createTempDir, executable, getPermissions, getTempDir, removeDirRecur)
import System.Process.Typed (Process, ProcessConfig, proc, startProcess, stopProcess)

import qualified Mpv.Data.MpvError as MpvError
import Mpv.Data.MpvError (MpvError (MpvError))
import Mpv.Data.MpvProcessConfig (MpvProcessConfig (MpvProcessConfig))

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
  bracket tempSocket (tryAny . removeDirRecur . parent)

mpvProc ::
  Path Abs File ->
  Path Abs File ->
  ProcessConfig () () ()
mpvProc mpv socket =
  proc (toFilePath mpv) [
    [exon|--input-ipc-server=#{toFilePath socket}|],
    "--idle=once",
    "--no-terminal"
  ]

findExecutable ::
  Members [Error MpvError, Embed IO] r =>
  Maybe (Path Abs File) ->
  Sem r (Path Abs File)
findExecutable = \case
  Just path -> do
    tryAny (getPermissions path) >>= \case
      Right (executable -> True) ->
        pure path
      Right _ ->
        throw notExecutable
      Left _ ->
        throw notExist
    where
      notExist =
        MpvError [exon|specified mpv path is not a readable file: #{show path}|]
      notExecutable =
        MpvError [exon|specified mpv path is not executable: #{show path}|]
  Nothing ->
    tryAny (Path.findExecutable [relfile|mpv|]) >>= \case
      Right (Just path) ->
        pure path
      _ ->
        throw (MpvError "could not find mpv executable in $PATH.")

startMpvProcess ::
  Members [Reader MpvProcessConfig, Embed IO, Final IO] r =>
  Path Abs File ->
  Sem r (Either MpvError (Process () () ()))
startMpvProcess socket =
  runError do
    MpvProcessConfig path <- ask
    validatedPath <- findExecutable path
    fromExceptionVia @SomeException (MpvError.Fatal . show) (startProcess (mpvProc validatedPath socket))

kill ::
  Members [Embed IO, Final IO] r =>
  Either MpvError (Process () () ()) ->
  Sem r ()
kill =
  traverse_ (tryAny . stopProcess)

withMpvProcess ::
  Members [Reader MpvProcessConfig, Resource, Embed IO, Final IO] r =>
  Path Abs File ->
  (Either MpvError (Process () () ()) -> Sem r a) ->
  Sem r a
withMpvProcess socket =
  bracket (startMpvProcess socket) kill
