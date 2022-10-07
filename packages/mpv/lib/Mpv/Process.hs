{-# language CPP #-}

module Mpv.Process where

import Conc (interpretQueueTBM, withAsync_)
import Data.Aeson (Value)
import Exon (exon)
import Network.Socket (Socket)
import Path (Abs, File, Path, parent, relfile, toFilePath, (</>))
import qualified Path.IO as Path
import Path.IO (createTempDir, executable, getPermissions, getTempDir, removeDirRecur)
import System.Process.Typed (Process, ProcessConfig, proc, startProcess, stopProcess)

import qualified Mpv.Data.MpvError as MpvError
import Mpv.Data.MpvError (MpvError (MpvError))
import Mpv.Data.MpvProcessConfig (MpvProcessConfig (MpvProcessConfig))
import Mpv.Data.MpvResources (InMessage, OutMessage)
import Mpv.Socket (withSocket)
import Mpv.SocketQueues (readQueue, writeQueue)

#if !MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
import Path (Rel)
#endif

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
  bracket tempSocket (tryIOError . removeDirRecur . parent)

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
  Members [Stop MpvError, Embed IO] r =>
  Maybe (Path Abs File) ->
  Sem r (Path Abs File)
findExecutable = \case
  Just path -> do
    tryIOError (getPermissions path) >>= \case
      Right (executable -> True) ->
        pure path
      Right _ ->
        stop notExecutable
      Left _ ->
        stop notExist
    where
      notExist =
        MpvError [exon|specified mpv path is not a readable file: #{show path}|]
      notExecutable =
        MpvError [exon|specified mpv path is not executable: #{show path}|]
  Nothing ->
    tryIOError (Path.findExecutable [relfile|mpv|]) >>= \case
      Right (Just path) ->
        pure path
      _ ->
        stop (MpvError "could not find mpv executable in $PATH.")

startMpvProcess ::
  Members [Reader MpvProcessConfig, Stop MpvError, Embed IO, Final IO] r =>
  Path Abs File ->
  Sem r (Process () () ())
startMpvProcess socket = do
  MpvProcessConfig path <- ask
  validatedPath <- findExecutable path
  stopTryIOError MpvError.Fatal (startProcess (mpvProc validatedPath socket))

kill ::
  Members [Embed IO, Final IO] r =>
  Process () () () ->
  Sem r ()
kill =
  void . tryIOError . stopProcess

withMpvProcess' ::
  Members [Reader MpvProcessConfig, Stop MpvError, Resource, Embed IO, Final IO] r =>
  Path Abs File ->
  (Process () () () -> Sem r a) ->
  Sem r a
withMpvProcess' socket =
  bracket (startMpvProcess socket) kill

withMpvProcess ::
  Members [Reader MpvProcessConfig, Stop MpvError, Resource, Embed IO, Final IO] r =>
  Path Abs File ->
  Sem r a ->
  Sem r a
withMpvProcess socket =
  bracket (startMpvProcess socket) kill . const

withSocketMpv ::
  Members [Reader MpvProcessConfig, Stop MpvError, Time t d, Resource, Race, Embed IO, Final IO] r =>
  (Socket -> Sem r a) ->
  Sem r a
withSocketMpv ma =
  withTempSocketPath \ socketPath ->
    withMpvProcess socketPath (withSocket socketPath ma)

withSocketQueuesMpv ::
  ∀ t d r a .
  Members [Reader MpvProcessConfig, Stop MpvError, Time t d, Log, Resource, Race, Async, Embed IO, Final IO] r =>
  Sem (Queue (OutMessage Value) : Queue (InMessage Value) : r) a ->
  Sem r a
withSocketQueuesMpv ma =
  interpretQueueTBM @(InMessage Value) 64 $ interpretQueueTBM @(OutMessage Value) 64 $ withSocketMpv \ socket ->
    withAsync_ (readQueue socket) $ withAsync_ (writeQueue socket) ma
