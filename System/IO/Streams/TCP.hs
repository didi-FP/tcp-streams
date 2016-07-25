{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions for interfacing @io-streams@
-- with raw tcp. the default receive buffer size is 4096. sending is unbuffered,
-- anything write into 'OutputStream' will be immediately send to underlying socket.
--
-- Reading 'InputStream' will block until GHC IO manager find data is ready,
-- for example 'System.IO.Streams.ByteString.readExactly 1024' will block until 1024 bytes are available.
--
-- When socket is closed, the 'InputStream' will be closed too(further reading will return 'Nothing'),
-- no exception will be thrown. You still should handle 'IOError' when you write to  'OutputStream' for safety,
-- but no exception doesn't essentially mean a successful write, especially under bad network
-- environment(broken wire for example).
--
-- `TCP_NODELAY` are enabled by default. you can use 'N.setSocketOption' to adjust.

module System.IO.Streams.TCP (
    -- * tcp client
    connectSocket
  , connect
  , connectWithBufferSize
  , withConnection
  , socketToStreamsWithBufferSize
    -- * tcp server
  , bindAndListen
  , accept
  , acceptWithBufferSize
  ) where

import           Control.Concurrent.MVar   (withMVar)
import qualified Control.Exception         as E
import           Control.Monad             (unless, void)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import           Network.Socket            (HostName, PortNumber, Socket (..))
import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as NB
import           System.IO.Streams         (InputStream, OutputStream)
import qualified System.IO.Streams         as Stream

bUFSIZ :: Int
bUFSIZ = 4096

-- | resolve a 'HostName'/'PortNumber' combination.
--
-- This function throws an 'IOError' when resolve fail.
--
resolveAddrInfo :: HostName -> PortNumber -> IO (N.Family, N.SocketType, N.ProtocolNumber, N.SockAddr)
resolveAddrInfo host port = do
    -- Partial function here OK, network will throw an exception rather than
    -- return the empty list here.
    (addrInfo:_) <- N.getAddrInfo (Just hints) (Just host) (Just $ show port)
    let family     = N.addrFamily addrInfo
    let socketType = N.addrSocketType addrInfo
    let protocol   = N.addrProtocol addrInfo
    let address    = N.addrAddress addrInfo
    return (family, socketType, protocol, address)
  where
    hints = N.defaultHints {
            N.addrFlags      = [N.AI_ADDRCONFIG, N.AI_NUMERICSERV]
        ,   N.addrSocketType = N.Stream
        }
{-# INLINABLE resolveAddrInfo #-}

-- | Convenience function for initiating an raw TCP connection to the given
-- @('HostName', 'PortNumber')@ combination.
--
-- Note that sending an end-of-file to the returned 'OutputStream' will not
-- close the underlying Socket connection.
--
connectSocket :: HostName             -- ^ hostname to connect to
              -> PortNumber           -- ^ port number to connect to
              -> IO Socket
connectSocket host port = do
    (family, socketType, protocol, address) <- resolveAddrInfo host port
    E.bracketOnError (N.socket family socketType protocol)
                     N.close
                     (\sock -> do N.connect sock address
                                  -- NoDelay causes an error for AF_UNIX.
                                  E.catch
                                    (N.setSocketOption sock N.NoDelay 1)
                                    (\ (E.SomeException _) -> return ())
                                  return sock
                     )


-- | connect to remote tcp server.
--
-- You may need to use 'E.bracket' pattern to enusre 'N.Socket' 's safety.
--
connect :: HostName             -- ^ hostname to connect to
        -> PortNumber           -- ^ port number to connect to
        -> IO (InputStream ByteString, OutputStream ByteString, Socket)
connect host port = connectWithBufferSize host port bUFSIZ

-- | connect to remote tcp server with adjustable receive buffer size.
--
connectWithBufferSize :: HostName             -- ^ hostname to connect to
                      -> PortNumber           -- ^ port number to connect to
                      -> Int                  -- ^ tcp read buffer size
                      -> IO (InputStream ByteString, OutputStream ByteString, Socket)
connectWithBufferSize host port bufsiz = do
    sock <- connectSocket host port
    (is, os) <- socketToStreamsWithBufferSize bufsiz sock
    return (is, os, sock)



-- | Convenience function for initiating an TCP connection to the given
-- @('HostName', 'PortNumber')@ combination. The socket will be
-- closed and deleted after the user handler runs.
--
withConnection :: HostName             -- ^ hostname to connect to
               -> PortNumber           -- ^ port number to connect to
               -> ( InputStream ByteString
                    -> OutputStream ByteString -> Socket -> IO a) -- ^ Action to run with the new connection
               -> IO a
withConnection host port action =
    E.bracket (connect host port) cleanup go

  where
    go (is, os, sock) = action is os sock

    cleanup (_, os, sock) = E.mask_ $
        eatException $! Stream.write Nothing os >> N.close sock

    eatException m = void m `E.catch` (\(_::E.SomeException) -> return ())

-- | bind and listen on port with a limit on connection count.
--
bindAndListen :: PortNumber -> Int -> IO Socket
bindAndListen port maxc = do
    E.bracketOnError (N.socket N.AF_INET N.Stream 0)
                     N.close
                     (\sock -> do
                                  -- NoDelay causes an error for AF_UNIX.
                                  E.catch
                                    (do
                                        N.setSocketOption sock N.ReuseAddr 1
                                        N.setSocketOption sock N.NoDelay 1)
                                    (\ (E.SomeException _) -> return ())
                                  N.bind sock (N.SockAddrInet port N.iNADDR_ANY)   -- listen on TCP port 4242.
                                  N.listen sock maxc
                                  return sock
                     )

-- | accept a new connection from remote client, return a 'InputStream' / 'OutputStream' pair,
-- a new underlying 'Socket', and remote 'N.SockAddr',you should call 'bindAndListen' first.
--
-- This function will block if there's no connection comming.
--
accept :: Socket -> IO (InputStream ByteString, OutputStream ByteString, N.Socket, N.SockAddr)
accept sock = acceptWithBufferSize sock bUFSIZ


-- | accept a connection with adjustable receive buffer size.
--
acceptWithBufferSize :: Socket -> Int -> IO (InputStream ByteString, OutputStream ByteString, N.Socket, N.SockAddr)
acceptWithBufferSize sock bufsiz = do
    (sock', sockAddr) <- N.accept sock
    (is, os) <- socketToStreamsWithBufferSize bufsiz sock'
    return (is, os, sock', sockAddr)


-- | convert a 'Socket' into a streams pair, catch 'IOException's on receiving and close 'InputStream'.
-- You still should handle 'IOError' when you write to  'OutputStream' for safety,
-- but no exception doesn't essentially mean a successful write, especially under bad network
-- environment(broken wire for example).
--
-- During receiving, the status 'Control.Concurrent.MVar.MVar' is locked, so that a cleanup thread
-- can't affect receiving until finish.
--
socketToStreamsWithBufferSize
    :: Int                      -- ^ how large the receive buffer should be
    -> Socket                   -- ^ network socket
    -> IO (InputStream ByteString, OutputStream ByteString)
socketToStreamsWithBufferSize bufsiz sock@(MkSocket _ _ _ _ statusMVar) = do
    is <- Stream.makeInputStream input
    os <- Stream.makeOutputStream output
    return (is, os)
  where
    input = withMVar statusMVar $ \ status ->
        case status of
            N.Connected -> ( do
                s <- NB.recv sock bufsiz
                return $! if B.null s then Nothing else Just s
                ) `E.catch` (\(_::E.IOException) -> return Nothing)
            _ -> return Nothing

    output Nothing  = return ()
    output (Just s) = unless (B.null s) (NB.sendAll sock s)
{-# INLINABLE socketToStreamsWithBufferSize #-}
