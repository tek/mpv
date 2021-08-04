module Mpv.MpvResources where

import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Network.Socket (Socket)
import Polysemy (insertAt)
import Polysemy.Conc (Events, Race, withAsync_)
import Polysemy.Conc.Interpreter.Queue.TBM (withTBMQueue)
import Polysemy.Log (Log)
import Polysemy.Time (Time)

import Mpv.Data.MpvError (MpvError)
import Mpv.Data.MpvEvent (MpvEvent)
import qualified Mpv.Data.MpvProcess as MpvProcess
import Mpv.Data.MpvProcess (MpvProcess (MpvProcess))
import Mpv.Data.MpvResources (InMessage, MpvResources (MpvResources), OutMessage, Requests (Requests))
import Mpv.Process (withMpvProcess)
import Mpv.Response (responseListener)
import Mpv.Socket (withSocket)
import Mpv.SocketQueues (withSocketQueues)

withMpvSocket ::
  Members [Resource, Time t d, Race, Embed IO, Final IO] r =>
  (Either MpvError Socket -> Sem r a) ->
  Sem r a
withMpvSocket action =
  withMpvProcess \case
    Right MpvProcess { socketPath } ->
      withSocket socketPath action
    Left err ->
      action (Left err)

withIpcIO ::
  Members [Events t MpvEvent, Resource, Race, Async, Log, Embed IO, Final IO] r =>
  (MpvResources -> Sem r a) ->
  Socket ->
  TBMQueue OutMessage ->
  TBMQueue InMessage ->
  TVar Requests ->
  Sem r a
withIpcIO action socket outQ inQ requests =
  withSocketQueues res do
    runAtomicStateTVar requests do
      withAsync_ responseListener do
        insertAt @0 (action res)
  where
    res =
      MpvResources socket outQ inQ requests

withSTMResources ::
  Members [Resource, Embed IO] r =>
  (TBMQueue OutMessage -> TBMQueue InMessage -> TVar Requests -> Sem r a) ->
  Sem r a
withSTMResources action = do
  reqs <- embed (newTVarIO (Requests 0 mempty))
  withTBMQueue 64 \ outQ -> withTBMQueue 64 \ inQ -> action outQ inQ reqs

withIpc ::
  Members [Events token MpvEvent, Resource, Race, Async, Log, Time t d, Embed IO, Final IO] r =>
  (Either MpvError MpvResources -> Sem r a) ->
  Sem r a
withIpc run =
  withMpvSocket \case
    Right socket ->
      withSTMResources (withIpcIO (run . Right) socket)
    Left err ->
      run (Left err)
