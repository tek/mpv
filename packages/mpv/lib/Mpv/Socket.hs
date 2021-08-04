module Mpv.Socket where

import qualified Network.Socket as Socket
import Network.Socket (SockAddr (SockAddrUnix), Socket)
import Path (Abs, File, Path, toFilePath)
import Polysemy.Conc (Race, retryingWithError)
import Polysemy.Resource (bracket)
import Polysemy.Time (MilliSeconds (MilliSeconds), Seconds (Seconds), Time)

import qualified Mpv.Data.MpvError as MpvError
import Mpv.Data.MpvError (MpvError)

unixSocket ::
  Members [Error MpvError, Embed IO] r =>
  Sem r Socket
unixSocket =
  errorException MpvError.Fatal (Socket.socket Socket.AF_UNIX Socket.Stream 0)

connectSocket ::
  Member (Embed IO) r =>
  Path Abs File ->
  Socket ->
  Sem r (Either Text ())
connectSocket path socket =
  tryAny (Socket.connect socket (SockAddrUnix (toFilePath path)))

acquireSocket ::
  Members [Error MpvError, Race, Time t d, Embed IO] r =>
  Path Abs File ->
  Sem r Socket
acquireSocket path = do
  socket <- unixSocket
  retryingWithError (Seconds 5) (MilliSeconds 100) (connectSocket path socket) >>= \case
    Just (Right ()) -> pure socket
    Just (Left err) -> throw (MpvError.Fatal err)
    Nothing -> throw (MpvError.Fatal "socket connect timed out")

releaseSocket ::
  Member (Embed IO) r =>
  Either MpvError Socket ->
  Sem r ()
releaseSocket = \case
  Right sock ->
    void (tryAny (Socket.close sock))
  Left _ ->
    unit

withSocket ::
  Members [Resource, Race, Time t d, Embed IO] r =>
  Path Abs File ->
  (Either MpvError Socket -> Sem r a) ->
  Sem r a
withSocket path =
  bracket (runError (acquireSocket path)) releaseSocket
