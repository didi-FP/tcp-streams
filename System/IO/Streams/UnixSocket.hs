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
  ( -- * tcp client
    connectSocket
  , connect
  , connectWithBufferSize
  , withConnection
    -- * tcp server
  , bindAndListen
  , TCP.accept
  , TCP.acceptWithBufferSize
    -- * helpers
  , TCP.socketToStreamsWithBufferSize
  , N.close
  ) where

import qualified Control.Exception         as E
import           Control.Monad             (void)
import           Data.ByteString           (ByteString)
import           Network.Socket            (Socket (..))
import qualified Network.Socket            as N
import           System.IO.Streams         (InputStream, OutputStream)
import qualified System.IO.Streams         as Stream
import qualified System.IO.Streams.TCP     as TCP

bUFSIZ :: Int
bUFSIZ = 4096

-- | Convenience function for initiating an raw TCP connection to the given
-- unix-socket file.
--
-- Note that sending an end-of-file to the returned 'OutputStream' will not
-- close the underlying Socket connection.
--
connectSocket :: String             -- ^ unix-socket to connect to
              -> IO Socket
connectSocket usock =
    E.bracketOnError (N.socket N.AF_UNIX N.Stream N.defaultProtocol)
                     N.close
                     (\sock -> do N.connect sock (N.SockAddrUnix usock)
                                  return sock
                     )

-- | Connect to unix-socket.
--
-- You may need to use 'E.bracket' pattern to enusre safety.
--
connect :: String             -- ^ unix-socket to connect to
        -> IO (InputStream ByteString, OutputStream ByteString, Socket)
connect usock = connectWithBufferSize usock bUFSIZ

-- | Connect to unix-socket with adjustable receive buffer size.
--
connectWithBufferSize :: String             -- ^ unix-socket to connect to
                      -> Int                -- ^ tcp read buffer size
                      -> IO (InputStream ByteString, OutputStream ByteString, Socket)
connectWithBufferSize usock bufsiz = do
    sock <- connectSocket usock
    (is, os) <- TCP.socketToStreamsWithBufferSize bufsiz sock
    return (is, os, sock)

-- | Convenience function for initiating an TCP connection to the given
-- unix-socket. The socket will be closed and deleted after the user handler runs.
--
withConnection :: String             -- ^ unix-socket to connect to
               -> ( InputStream ByteString
                    -> OutputStream ByteString -> Socket -> IO a) -- ^ Action to run with the new connection
               -> IO a
withConnection usock action =
    E.bracket (connect usock) cleanup go
  where
    go (is, os, sock) = action is os sock

    cleanup (_, os, sock) = E.mask_ $
        eatException $! Stream.write Nothing os >> N.close sock

    eatException m = void m `E.catch` (\(_::E.SomeException) -> return ())

-- | Bind and listen on a unix-socket with a limit on connection count.
--
-- After successful 'bind', a unix-socket file will be created,
-- you may need to manually remove it.
--
bindAndListen :: String -> Int -> IO Socket
bindAndListen usock maxc =
    E.bracketOnError (N.socket N.AF_UNIX N.Stream 0)
                     N.close
                     (\sock -> do N.bind sock (N.SockAddrUnix usock)
                                  N.listen sock maxc
                                  return sock
                     )
