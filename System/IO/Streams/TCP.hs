{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions for interfacing @io-streams@
-- with raw tcp. the default receive buffer size is 4096. sending is unbuffered,
-- anything write into 'OutputStream' will be immediately send to underlying socket.
--
-- You should handle 'IOError' when you read/write these streams for safty.
-- Note, `TCP_NODELAY` are enabled by default. you can use 'N.setSocketOption' to adjust.

module System.IO.Streams.TCP (
    -- * tcp client
    connectSocket
  , connect
  , connectWithBufferSize
  , withConnection
    -- * tcp server
  , bindAndListen
  , accept
  ) where

import qualified Control.Exception         as E
import           Control.Monad             (void)
import           Data.ByteString.Char8     (ByteString)
import           Network.Socket            (HostName, PortNumber, Socket)
import qualified Network.Socket            as N
import           System.IO.Streams         (InputStream, OutputStream)
import qualified System.IO.Streams         as Stream
import           System.IO.Streams.Network (socketToStreamsWithBufferSize)


bUFSIZ :: Int
bUFSIZ = 4096

-- | resolve a 'HostName'/'PortNumber' combination.
--
-- This function throws an IO exception when resolve fail.
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
-- close the underlying Socket connection; to do that, call:
--
-- @
-- SSL.'SSL.shutdown' ssl SSL.'SSL.Unidirectional'
-- maybe (return ()) 'N.close' $ SSL.'SSL.sslSocket' ssl
-- @
--
-- on the returned 'SSL' object.
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
connect :: HostName             -- ^ hostname to connect to
        -> PortNumber           -- ^ port number to connect to
        -> IO (InputStream ByteString, OutputStream ByteString, Socket)
connect host port = do
    sock <- connectSocket host port
    (is, os) <- socketToStreamsWithBufferSize bUFSIZ sock
    return (is, os, sock)


-- | connect to remote tcp server with a receive buffer size.
connectWithBufferSize :: HostName             -- ^ hostname to connect to
                      -> PortNumber           -- ^ port number to connect to
                      -> Int                  -- ^ tcp read buffer size
                      -> IO (InputStream ByteString, OutputStream ByteString, Socket)
connectWithBufferSize host port bufsiz = do
    sock <- connectSocket host port
    (is, os) <- socketToStreamsWithBufferSize bufsiz sock
    return (is, os, sock)



-- | Convenience function for initiating an SSL connection to the given
-- @('HostName', 'PortNumber')@ combination. The socket and SSL connection are
-- closed and deleted after the user handler runs.
--
-- /Since: 1.2.0.0./
withConnection
    :: HostName             -- ^ hostname to connect to
    -> PortNumber           -- ^ port number to connect to
    -> (InputStream ByteString -> OutputStream ByteString -> Socket -> IO a)
          -- ^ Action to run with the new connection
    -> IO a
withConnection host port action =
    E.bracket (connect host port) cleanup go

  where
    go (is, os, sock) = action is os sock

    cleanup (_, os, sock) = E.mask_ $ do
        eatException $! Stream.write Nothing os
        eatException $! N.close sock

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

-- | accept a new connection from remote client, return a 'InputStream'/'OutputStream' pair,
-- a new underlying 'Socket', and remote 'N.SockAddr',you should call 'bindAndListen' first.
--
-- This function will block current thread if there's no connection comming.
accept :: Socket -> IO (InputStream ByteString, OutputStream ByteString, N.Socket, N.SockAddr)
accept sock = do
    (sock', sockAddr) <- N.accept sock
    (is, os) <- socketToStreamsWithBufferSize bUFSIZ sock'
    return (is, os, sock', sockAddr)
