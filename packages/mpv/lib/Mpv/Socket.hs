module Mpv.Socket where

import Conc (retryingWithError)
import qualified Network.Socket as Socket
import Network.Socket (SockAddr (SockAddrUnix), Socket)
import Path (toFilePath)
import Polysemy.Time (MilliSeconds (MilliSeconds), Seconds (Seconds))

import qualified Mpv.Data.MpvError as MpvError
import Mpv.Data.MpvError (MpvError)
import Mpv.Data.SocketPath (SocketPath (SocketPath))

unixSocket ::
  Members [Stop MpvError, Embed IO] r =>
  Sem r Socket
unixSocket =
  stopTryIOError MpvError.Fatal (Socket.socket Socket.AF_UNIX Socket.Stream 0)

connectSocket ::
  Member (Embed IO) r =>
  SocketPath ->
  Socket ->
  Sem r (Either Text ())
connectSocket (SocketPath path) socket =
  tryIOError (Socket.connect socket (SockAddrUnix (toFilePath path)))

acquireSocket ::
  Members [Stop MpvError, Race, Time t d, Embed IO] r =>
  SocketPath ->
  Sem r Socket
acquireSocket path = do
  socket <- unixSocket
  retryingWithError (Seconds 5) (MilliSeconds 100) (connectSocket path socket) >>= \case
    Just (Right ()) -> pure socket
    Just (Left err) -> stop (MpvError.Fatal err)
    Nothing -> stop (MpvError.Fatal "socket connect timed out")

withSocket ::
  Members [Stop MpvError, Resource, Race, Time t d, Embed IO] r =>
  SocketPath ->
  (Socket -> Sem r a) ->
  Sem r a
withSocket path =
  bracket (acquireSocket path) (tryIOError_ . Socket.close)
