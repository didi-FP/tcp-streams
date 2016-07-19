{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions for interfacing @io-streams@
-- with @tls@. the default receive buffer size is decided by @tls@.
-- sending is unbuffered, anything write into 'OutputStream' will be
-- immediately send to underlying socket.
--
-- You should handle 'IOError' when you read/write these streams for safty.
module System.IO.Streams.TLS (
    -- * tls client
    connect
  , withConnection
    -- * tls server
  , accept
    -- * helpers
  , tlsToStreams
  , closeTLS
  ) where

import qualified Control.Exception     as E
import           Control.Monad         (void)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import           Data.ByteString.Lazy  (fromStrict)
import           Network.Socket        (HostName, PortNumber, Socket)
import qualified Network.Socket        as N
import           Network.TLS           (ClientParams, Context, ServerParams)
import qualified Network.TLS           as TLS
import           System.IO.Streams     (InputStream, OutputStream)
import qualified System.IO.Streams     as Stream
import qualified System.IO.Streams.TCP as TCP


-- | Given an existing TLS 'Context' connection, produces an 'InputStream' \/
-- 'OutputStream' pair.
--
tlsToStreams :: Context             -- ^ TLS connection object
             -> IO (InputStream ByteString, OutputStream ByteString)
tlsToStreams ctx = do
    is <- Stream.makeInputStream input
    os <- Stream.makeOutputStream output
    return (is, os)
  where
    input = do
        s <- TLS.recvData ctx
        return $! if B.null s then Nothing else Just s

    output Nothing  = return ()
    output (Just s) = TLS.sendData ctx (fromStrict s)


-- | Convenience function for initiating an TLS connection to the given
-- @('HostName', 'PortNumber')@ combination.
--
-- Note that sending an end-of-file to the returned 'OutputStream' will not
-- close the underlying TLS connection; to do that, call 'closeTLS'
--
-- this operation will throw 'TLS.TLSException' on failure.
connect :: ClientParams         -- ^ SSL context. See the @HsOpenSSL@
                                   -- documentation for information on creating
                                   -- this.
           -> HostName             -- ^ hostname to connect to
           -> PortNumber           -- ^ port number to connect to
           -> IO (InputStream ByteString, OutputStream ByteString, Context)
connect prms host port = do
    sock <- TCP.connectSocket host port
    E.bracketOnError (TLS.contextNew sock prms) closeTLS $ \ ctx -> do
        TLS.handshake ctx
        (is, os) <- tlsToStreams ctx
        return (is, os, ctx)


-- | Convenience function for initiating an TLS connection to the given
-- @('HostName', 'PortNumber')@ combination. The socket and SSL connection are
-- closed and deleted after the user handler runs.
--
withConnection ::
     ClientParams         -- ^ SSL context. See the @HsOpenSSL@
                          -- documentation for information on creating
                          -- this.
  -> HostName             -- ^ hostname to connect to
  -> PortNumber           -- ^ port number to connect to
  -> (InputStream ByteString -> OutputStream ByteString -> Context -> IO a)
          -- ^ Action to run with the new connection
  -> IO a
withConnection prms host port action =
    E.bracket (connect prms host port) cleanup go

  where
    go (is, os, ctx) = action is os ctx

    cleanup (_, os, ctx) = E.mask_ $
        eatException $! Stream.write Nothing os >> closeTLS ctx

    eatException m = void m `E.catch` (\(_::E.SomeException) -> return ())


-- | accept a new connection from remote client, return a 'InputStream'/'OutputStream' pair
-- and remote 'N.SockAddr', you should call 'TCP.bindAndListen' first.
accept :: ServerParams -> Socket -> IO (InputStream ByteString, OutputStream ByteString, Context, N.SockAddr)
accept prms sock = do
    (sock', sockAddr) <- N.accept sock
    E.bracketOnError (TLS.contextNew sock' prms) closeTLS $ \ ctx -> do
        TLS.handshake ctx
        (is, os) <- tlsToStreams ctx
        return (is, os, ctx, sockAddr)


-- | close a TLS 'Context'(and its underlying socket).
--
closeTLS :: Context -> IO ()
closeTLS ctx = TLS.bye ctx >> TLS.contextClose ctx
