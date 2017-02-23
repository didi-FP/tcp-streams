{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions for interfacing unix-socket.
--
-- This module is intended to be imported @qualified@, e.g.:
--
-- @
-- import           "Data.Connection"
-- import qualified "System.IO.Streams.UnixSocket" as UnixSocket
-- @
--
-- @since 0.5.0.0
--
module System.IO.Streams.UnixSocket
  ( -- * client
    connect
  , connectSocket
    -- * server
  , bindAndListen
  , accept
  ) where

import qualified Control.Exception         as E
import           Control.Monad             (void)
import           Data.ByteString           (ByteString)
import qualified Network.Socket            as N
import           System.IO.Streams         (InputStream, OutputStream)
import qualified System.IO.Streams         as Stream
import           Data.Connection
import qualified System.IO.Streams.TCP     as TCP


-- | Convenience function for initiating an raw TCP connection to the given
-- unix-socket file.
--
-- Note that sending an end-of-file to the returned 'OutputStream' will not
-- close the underlying Socket connection.
--
connectSocket :: String             -- ^ unix-socket to connect to
              -> IO (N.Socket, N.SockAddr)
connectSocket usock =
    E.bracketOnError (N.socket N.AF_UNIX N.Stream N.defaultProtocol)
                     N.close
                     (\sock -> do let addr = N.SockAddrUnix usock
                                  N.connect sock addr
                                  -- default output buffer of unix socket is too small
                                  N.setSocketOption sock N.SendBuffer (TCP.defaultChunkSize * 16)
                                  return (sock, addr)
                     )

-- | Connect to unix-socket.
--
connect :: String             -- ^ unix-socket to connect to
        -> IO Connection
connect usock = connectSocket usock >>= TCP.socketToConnection TCP.defaultChunkSize

-- | Bind and listen on a unix-socket with a limit on connection count.
--
bindAndListen :: Int -> String -> IO N.Socket
bindAndListen maxc usock =
    E.bracketOnError (N.socket N.AF_UNIX N.Stream 0)
                     N.close
                     (\sock -> do N.bind sock (N.SockAddrUnix usock)
                                  N.listen sock maxc
                                  return sock
                     )

-- | Accept a connection with 'defaultChunkSize'
--
accept :: N.Socket -> IO Connection
accept sock = TCP.acceptWith sock $ \ sp@(sock, addr) -> do
    N.setSocketOption sock N.SendBuffer (TCP.defaultChunkSize * 16)
    TCP.socketToConnection TCP.defaultChunkSize sp
