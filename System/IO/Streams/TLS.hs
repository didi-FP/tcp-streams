{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions for interfacing @io-streams@
-- with @tls@. the default receive buffer size is decided by @tls@.
-- sending is unbuffered, anything write into 'OutputStream' will be
-- immediately send to underlying socket.
--
-- The same exceptions rule which applied to TCP apply here, with addtional
-- 'TLS.TLSException' to be watched out.
--
-- This module is intended to be imported @qualified@, e.g.:
--
-- @
-- import qualified "Data.TLSSetting"       as TLS
-- import qualified "System.IO.Streams.TLS" as TLS
-- @
--
module System.IO.Streams.TLS
  ( -- * client
    connect
  , withConnection
    -- * server
  , accept
    -- * helpers
  , tlsToStreams
  , close
  ) where

import qualified Control.Exception     as E
import           Control.Monad         (void)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
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
    input = ( do
        s <- TLS.recvData ctx
        return $! if B.null s then Nothing else Just s
        ) `E.catch` (\(_::E.SomeException) -> return Nothing)

    output Nothing  = return ()
    output (Just s) = TLS.sendData ctx (fromStrict s)
{-# INLINABLE tlsToStreams #-}

-- | Close a TLS 'Context' and its underlying socket.
--
close :: Context -> IO ()
close ctx = (TLS.bye ctx >> TLS.contextClose ctx) -- sometimes socket was closed before 'TLS.bye'
    `E.catch` (\(_::E.SomeException) -> return ())   -- so we catch the 'Broken pipe' error here

-- | Convenience function for initiating an TLS connection to the given
-- @('HostName', 'PortNumber')@ combination.
--
-- Note that sending an end-of-file to the returned 'OutputStream' will not
-- close the underlying TLS connection; to do that, call 'close'
--
-- this operation will throw 'TLS.TLSException' on failure.
--
connect :: ClientParams         -- ^ check "Data.TLSSetting".
        -> Maybe String         -- ^ Optional certificate subject name, if set to 'Nothing'
                                -- then we will try to verify 'HostName' as subject name.
        -> HostName             -- ^ hostname to connect to
        -> PortNumber           -- ^ port number to connect to
        -> IO (InputStream ByteString, OutputStream ByteString, Context)
connect prms subname host port = do
    let subname' = maybe host id subname
        prms' = prms { TLS.clientServerIdentification = (subname', BC.pack (show port)) }
    sock <- TCP.connectSocket host port
    E.bracketOnError (TLS.contextNew sock prms') close $ \ ctx -> do
        TLS.handshake ctx
        (is, os) <- tlsToStreams ctx
        return (is, os, ctx)


-- | Convenience function for initiating an TLS connection to the given
-- @('HostName', 'PortNumber')@ combination. The socket and TLS connection are
-- closed and deleted after the user handler runs.
--
withConnection :: ClientParams
               -> Maybe HostName
               -> HostName
               -> PortNumber
               -> (InputStream ByteString -> OutputStream ByteString -> Context -> IO a)
                       -- ^ Action to run with the new connection
               -> IO a
withConnection prms subname host port action =
    E.bracket (connect prms subname host port) cleanup go

  where
    go (is, os, ctx) = action is os ctx

    cleanup (_, os, ctx) = E.mask_ $
        eatException $! Stream.write Nothing os >> close ctx

    eatException m = void m `E.catch` (\(_::E.SomeException) -> return ())


-- | Accept a new connection from remote client, return a 'InputStream' / 'OutputStream'
-- pair and remote 'N.SockAddr', you should call 'TCP.bindAndListen' first.
--
-- this operation will throw 'TLS.TLSException' on failure.
--
accept :: ServerParams              -- ^ check "Data.TLSSetting".
       -> Socket                    -- ^ the listening 'Socket'.
       -> IO (InputStream ByteString, OutputStream ByteString, Context, N.SockAddr)
accept prms sock = do
    (sock', sockAddr) <- N.accept sock
    E.bracketOnError (TLS.contextNew sock' prms) close $ \ ctx -> do
        TLS.handshake ctx
        (is, os) <- tlsToStreams ctx
        return (is, os, ctx, sockAddr)
