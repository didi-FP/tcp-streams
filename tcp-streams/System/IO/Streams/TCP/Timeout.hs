{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}

-- | @System.IO.Streams.TCP@ module with explicit timeout configuration.
--
-- Timeout limit could be set when @connect@ (for client) or @recv@ (for server).
-- When timeout occurs, a @TimeoutException@ will be thrown.
--
-- This module is intended to be imported @qualified@, e.g.:
--
-- @
-- import qualified "System.IO.Streams.TCP.Timeout" as TCPTimeout
-- @
--
module System.IO.Streams.TCP.Timeout
  ( -- * tcp client
    connect
  , connectWithBufferSize
  , withConnection
    -- * tcp server
  , accept
  , acceptWithBufferSize
    -- * helpers
  , socketToStreamsWithBufferSize
  , N.close
    -- * Timeout exception.
  , TimeoutException (..)
  ) where

import           Control.Concurrent.MVar   (withMVar)
import qualified Control.Exception         as E
import           Control.Monad             (unless, void)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as B
import           Data.Typeable
import           Network.Socket            (HostName, PortNumber, Socket (..))
import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as NB
import           System.Timeout            (timeout)
import           System.IO.Streams         (InputStream, OutputStream)
import qualified System.IO.Streams         as Stream
import qualified System.IO.Streams.TCP     as TCP

bUFSIZ :: Int
bUFSIZ = 4096

-- | Timeout exception, when timeout occurs, a @TimeoutException@ will be thrown.
data TimeoutException = TimeoutException deriving (Eq, Show, Typeable)

instance E.Exception TimeoutException

-- | Connect to remote tcp server.
--
-- You may need to use 'E.bracket' pattern to enusre 'N.Socket' 's safety.
--
-- When read from TCP server but there's no result within @n@ microseconds(@1/10^6@ seconds),
-- an @TimeoutException@ will be thrown.
connect :: HostName             -- ^ hostname to connect to
        -> PortNumber           -- ^ port number to connect to
        -> Int                  -- ^ time limit.
        -> IO (InputStream ByteString, OutputStream ByteString, Socket)
connect host port n = connectWithBufferSize host port bUFSIZ n

-- | Connect to remote tcp server with adjustable receive buffer size.
--
connectWithBufferSize :: HostName             -- ^ hostname to connect to
                      -> PortNumber           -- ^ port number to connect to
                      -> Int                  -- ^ tcp read buffer size
                      -> Int                  -- ^ time limit.
                      -> IO (InputStream ByteString, OutputStream ByteString, Socket)
connectWithBufferSize host port bufsiz n = do
    sock <- TCP.connectSocket host port
    (is, os) <- socketToStreamsWithBufferSize bufsiz sock n
    return (is, os, sock)

-- | Convenience function for initiating an TCP connection to the given
-- @('HostName', 'PortNumber')@ combination. The socket will be
-- closed and deleted after the user handler runs.
--
withConnection :: HostName             -- ^ hostname to connect to
               -> PortNumber           -- ^ port number to connect to
               -> Int                  -- ^ time limit.
               -> ( InputStream ByteString
                    -> OutputStream ByteString -> Socket -> IO a) -- ^ Action to run with the new connection
               -> IO a
withConnection host port n action =
    E.bracket (connect host port n) cleanup go

  where
    go (is, os, sock) = action is os sock

    cleanup (_, os, sock) = E.mask_ $
        eatException $! Stream.write Nothing os >> N.close sock

    eatException m = void m `E.catch` (\(_::E.SomeException) -> return ())

-- | Accept a new connection from remote client, return a 'InputStream' / 'OutputStream' pair,
-- a new underlying 'Socket', and remote 'N.SockAddr',you should call 'bindAndListen' first.
--
-- This function will block if there's no connection comming.
--
accept :: Socket
       -> Int           -- ^ time limit
       -> IO (InputStream ByteString, OutputStream ByteString, N.Socket, N.SockAddr)
accept sock n = acceptWithBufferSize sock bUFSIZ n

-- | accept a connection with adjustable receive buffer size.
--
acceptWithBufferSize :: Socket
                     -> Int
                     -> Int         -- ^ time limit
                     -> IO (InputStream ByteString, OutputStream ByteString, N.Socket, N.SockAddr)
acceptWithBufferSize sock bufsiz n = do
    (sock', sockAddr) <- N.accept sock
    (is, os) <- socketToStreamsWithBufferSize bufsiz sock' n
    return (is, os, sock', sockAddr)

-- | Convert a 'Socket' into a streams pair, catch 'IOException's on receiving and close 'InputStream'.
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
    -> Int                      -- ^ time limit.
    -> IO (InputStream ByteString, OutputStream ByteString)
socketToStreamsWithBufferSize bufsiz sock@(MkSocket _ _ _ _ statusMVar) n = do
    is <- Stream.makeInputStream input
    os <- Stream.makeOutputStream output
    return (is, os)
  where
    input = withMVar statusMVar $ \ status ->
        case status of
            N.Connected -> ( do
                s <- timeout n (NB.recv sock bufsiz) >>= \case
                                                Just v  -> return v
                                                Nothing -> E.throw TimeoutException
                return $! if B.null s then Nothing else Just s
                ) `E.catch` (\(_::E.IOException) -> return Nothing)
            _ -> return Nothing

    output Nothing  = return ()
    output (Just s) = unless (B.null s) (NB.sendAll sock s)
