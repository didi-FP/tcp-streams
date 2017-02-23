{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions for interfacing @io-streams@
-- with unix-socket file. the default receive buffer size is 4096. sending is unbuffered,
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
-- This module is intended to be imported @qualified@, e.g.:
--
-- @
-- import qualified "System.IO.Streams.UnixSocket" as UnixSocket
-- @
--
-- @since 0.5.0.0
--
module System.IO.Streams.UnixSocket
  ( -- * connection type
    Connection(..)
    -- * tcp client
  , connect
  , connectSocket
  , TCP.socketToConnection
  , TCP.defaultChunkSize
    -- * tcp server
  , bindAndListen
  , TCP.accept
  ) where

import qualified Control.Exception         as E
import           Control.Monad             (void)
import           Data.Connection
import           Data.ByteString           (ByteString)
import qualified Network.Socket            as N
import           System.IO.Streams         (InputStream, OutputStream)
import qualified System.IO.Streams         as Stream
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
