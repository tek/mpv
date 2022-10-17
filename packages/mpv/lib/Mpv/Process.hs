{-# language CPP #-}

module Mpv.Process where

import Data.Aeson (Value)
import Exon (exon)
import Network.Socket (Socket)
import Path (Abs, File, Path, parent, relfile, toFilePath, (</>))
import Path.IO (createTempDir, getTempDir, removeDirRecur)
import Process (
  SysProcConf,
  SystemProcess,
  SystemProcessError,
  SystemProcessScopeError,
  interpretSystemProcessNative,
  resolveExecutable,
  withSystemProcess,
  )
import System.Process.Typed (ProcessConfig, proc)

import qualified Mpv.Data.MpvError as MpvError
import Mpv.Data.MpvError (MpvError)
import Mpv.Data.MpvProcessConfig (MpvProcessConfig (MpvProcessConfig))
import Mpv.Data.MpvResources (InMessage, OutMessage)
import Mpv.Data.SocketPath (SocketPath (SocketPath))
import Mpv.Socket (withSocket)
import Mpv.SocketQueues (withSocketQueues)

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
  (SocketPath -> Sem r a) ->
  Sem r a
withTempSocketPath use =
  bracket tempSocket (tryIOError . removeDirRecur . parent) \ path -> use (SocketPath path)

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

withMpvProcess ::
  Show pse =>
  Members [Scoped SocketPath (SystemProcess !! pe) !! pse, Stop MpvError] r =>
  SocketPath ->
  InterpreterFor (SystemProcess !! pe) r
withMpvProcess socket =
  resumeHoist (MpvError.Fatal . show) .
  withSystemProcess socket .
  raiseUnder

withSocketMpv ::
  ∀ pe pse t d r a .
  Show pse =>
  Member (Scoped SocketPath (SystemProcess !! pe) !! pse) r =>
  Members [Stop MpvError, Time t d, Resource, Race, Embed IO] r =>
  (Socket -> Sem r a) ->
  Sem r a
withSocketMpv ma =
  withTempSocketPath \ socketPath ->
    withMpvProcess socketPath (raise (withSocket socketPath ma))

withSocketQueuesMpv ::
  ∀ pe pse t d r a .
  Show pse =>
  Member (Scoped SocketPath (SystemProcess !! pe) !! pse) r =>
  Members [Stop MpvError, Time t d, Log, Resource, Race, Async, Embed IO, Final IO] r =>
  Sem (Queue (OutMessage Value) : Queue (InMessage Value) : r) a ->
  Sem r a
withSocketQueuesMpv ma =
  withSocketMpv \ socket ->
    withSocketQueues socket ma

processConfig ::
  Members [Reader MpvProcessConfig, Embed IO] r =>
  SocketPath ->
  Sem r (Either Text SysProcConf)
processConfig (SocketPath socket) = do
  MpvProcessConfig path <- ask
  fmap (flip mpvProc socket) <$> resolveExecutable [relfile|mpv|] path

interpretMpvProcess ::
  Members [Reader MpvProcessConfig, Resource, Embed IO] r =>
  InterpreterFor (Scoped SocketPath (SystemProcess !! SystemProcessError) !! SystemProcessScopeError) r
interpretMpvProcess =
  interpretSystemProcessNative processConfig
