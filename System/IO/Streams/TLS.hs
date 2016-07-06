{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions for interfacing @io-streams@
-- with @HsOpenSSL@. It is intended to be imported @qualified@, e.g.:
--
-- @
-- import qualified "OpenSSL" as SSL
-- import qualified "OpenSSL.Session" as SSL
-- import qualified "System.IO.Streams.SSL" as SSLStreams
--
-- \ example :: IO ('InputStream' 'ByteString', 'OutputStream' 'ByteString')
-- example = SSL.'SSL.withOpenSSL' $ do
--     ctx <- SSL.'SSL.context'
--     SSL.'SSL.contextSetDefaultCiphers' ctx
--
-- \     \-\- Note: the location of the system certificates is system-dependent,
--     \-\- on Linux systems this is usually \"\/etc\/ssl\/certs\". This
--     \-\- step is optional if you choose to disable certificate verification
--     \-\- (not recommended!).
--     SSL.'SSL.contextSetCADirectory' ctx \"\/etc\/ssl\/certs\"
--     SSL.'SSL.contextSetVerificationMode' ctx $
--         SSL.'SSL.VerifyPeer' True True Nothing
--     SSLStreams.'connect' ctx "foo.com" 4444
-- @
--

module System.IO.Streams.TLS
  ( connectTLS
  , withTLSConnection
  , tlsToStreams
  ) where

import qualified Control.Exception     as E
import           Control.Monad         (void)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import           Data.ByteString.Lazy  (fromStrict)
import           Network.Socket        (HostName, PortNumber, Socket)
import qualified Network.Socket        as N
import           Network.TLS           (ClientParams, Context)
import qualified Network.TLS           as TLS
import           System.IO.Streams     (InputStream, OutputStream)
import qualified System.IO.Streams     as Streams
import qualified System.IO.Streams.Raw as Raw


------------------------------------------------------------------------------

bUFSIZ :: Int
bUFSIZ = 32752

------------------------------------------------------------------------------
-- | Given an existing HsOpenSSL 'SSL' connection, produces an 'InputStream' \/
-- 'OutputStream' pair.
tlsToStreams :: Context             -- ^ TLS connection object
             -> IO (InputStream ByteString, OutputStream ByteString)
tlsToStreams tls = do
    is <- Streams.makeInputStream input
    os <- Streams.makeOutputStream output
    return (is, os)
  where
    input = do
        s <- TLS.recvData tls
        return $! if B.null s then Nothing else Just s

    output Nothing  = return ()
    output (Just s) = TLS.sendData tls (fromStrict s)


------------------------------------------------------------------------------
-- | Convenience function for initiating an SSL connection to the given
-- @('HostName', 'PortNumber')@ combination.
--
-- Note that sending an end-of-file to the returned 'OutputStream' will not
-- close the underlying SSL connection; to do that, call:
--
-- @
-- SSL.'SSL.shutdown' ssl SSL.'SSL.Unidirectional'
-- maybe (return ()) 'N.close' $ SSL.'SSL.sslSocket' ssl
-- @
--
-- on the returned 'SSL' object.
connectTLS
    :: ClientParams         -- ^ SSL context. See the @HsOpenSSL@
                                -- documentation for information on creating
                                -- this.
    -> HostName             -- ^ hostname to connect to
    -> PortNumber           -- ^ port number to connect to
    -> IO (InputStream ByteString, OutputStream ByteString, Context, Socket)
connectTLS prms host port = do
    sock <- Raw.connectSocket host port
    tls <- TLS.contextNew sock prms
    (is, os) <- tlsToStreams tls
    return (is, os, tls, sock)

------------------------------------------------------------------------------
-- | Convenience function for initiating an SSL connection to the given
-- @('HostName', 'PortNumber')@ combination. The socket and SSL connection are
-- closed and deleted after the user handler runs.
--
-- /Since: 1.2.0.0./
withTLSConnection ::
     ClientParams         -- ^ SSL context. See the @HsOpenSSL@
                          -- documentation for information on creating
                          -- this.
  -> HostName             -- ^ hostname to connect to
  -> PortNumber           -- ^ port number to connect to
  -> (InputStream ByteString -> OutputStream ByteString -> Context -> IO a)
          -- ^ Action to run with the new connection
  -> IO a
withTLSConnection ctx host port action =
    E.bracket (connectTLS ctx host port) cleanup go

  where
    go (is, os, tls, _) = action is os tls

    cleanup (_, os, tls, sock) = E.mask_ $ do
        eatException $! Streams.write Nothing os
        eatException $! TLS.bye tls
        eatException $! N.close sock

    eatException m = void m `E.catch` (\(_::E.SomeException) -> return ())



