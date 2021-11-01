module Mpv.MpvResources where

import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Network.Socket (Socket)
import Polysemy (insertAt)
import Polysemy.Conc (Events, withAsync_)
import Polysemy.Conc.Interpreter.Queue.TBM (withTBMQueue)
import Polysemy.Log (Log)
import Polysemy.Time (Time)

import Mpv.Data.MpvError (MpvError)
import Mpv.Data.MpvEvent (MpvEvent)
import Mpv.Data.MpvProcessConfig (MpvProcessConfig)
import Mpv.Data.MpvResources (InMessage, MpvResources (MpvResources), OutMessage, Requests (Requests))
import Mpv.Process (withMpvProcess, withTempSocketPath)
import Mpv.Response (responseListener)
import Mpv.Socket (withSocket)
import Mpv.SocketQueues (withSocketQueues)

withMpvSocket ::
  Members [Reader MpvProcessConfig, Resource, Time t d, Race, Embed IO, Final IO] r =>
  (Either MpvError Socket -> Sem r a) ->
  Sem r a
withMpvSocket action =
  withTempSocketPath \ socketPath ->
    withMpvProcess socketPath \case
      Right _ ->
        withSocket socketPath action
      Left err ->
        action (Left err)

withIpcIO ::
  Members [Events t MpvEvent, Resource, Race, Async, Log, Embed IO, Final IO] r =>
  (MpvResources Value -> Sem r a) ->
  Socket ->
  TBMQueue (OutMessage Value) ->
  TBMQueue (InMessage Value) ->
  TVar (Requests Value) ->
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
  (TBMQueue (OutMessage fmt) -> TBMQueue (InMessage fmt) -> TVar (Requests Value) -> Sem r a) ->
  Sem r a
withSTMResources action = do
  reqs <- embed (newTVarIO (Requests 0 mempty))
  withTBMQueue 64 \ outQ -> withTBMQueue 64 \ inQ -> action outQ inQ reqs

withMpvResources ::
  Members [Reader MpvProcessConfig, Events token MpvEvent] r =>
  Members [Resource, Race, Async, Log, Time t d, Embed IO, Final IO] r =>
  (Either MpvError (MpvResources Value) -> Sem r a) ->
  Sem r a
withMpvResources run =
  withMpvSocket \case
    Right socket ->
      withSTMResources (withIpcIO (run . Right) socket)
    Left err ->
      run (Left err)
