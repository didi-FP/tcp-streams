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

module System.IO.Streams.SSL
  ( connectSSL
  , withSSLConnection
  , sslToStreams
  ) where

import qualified Control.Exception     as E
import           Control.Monad         (void)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import           Network.Socket        (HostName, PortNumber, Socket)
import qualified Network.Socket        as N
import           OpenSSL.Session       (SSL, SSLContext)
import qualified OpenSSL.Session       as SSL
import           System.IO.Streams     (InputStream, OutputStream)
import qualified System.IO.Streams     as Streams
import qualified System.IO.Streams.Raw as Raw


------------------------------------------------------------------------------

bUFSIZ :: Int
bUFSIZ = 32752

------------------------------------------------------------------------------
-- | Given an existing HsOpenSSL 'SSL' connection, produces an 'InputStream' \/
-- 'OutputStream' pair.
sslToStreams
    :: SSL             -- ^ SSL connection object
     -> IO (InputStream ByteString, OutputStream ByteString)
sslToStreams ssl = do
    is <- Streams.makeInputStream input
    os <- Streams.makeOutputStream output
    return (is, os)

  where
    input = do
        s <- SSL.read ssl bUFSIZ
        return $! if S.null s then Nothing else Just s

    output Nothing  = return ()
    output (Just s) = SSL.write ssl s


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
connectSSL
    :: SSLContext           -- ^ SSL context. See the @HsOpenSSL@
                            -- documentation for information on creating
                            -- this.
    -> HostName             -- ^ hostname to connect to
    -> PortNumber           -- ^ port number to connect to
    -> IO (InputStream ByteString, OutputStream ByteString, SSL, Socket)
connectSSL ctx host port = do
    sock <- Raw.connectSocket host port
    ssl <- SSL.connection ctx sock
    SSL.connect ssl
    (is, os) <- sslToStreams ssl
    return (is, os, ssl, sock)

------------------------------------------------------------------------------
-- | Convenience function for initiating an SSL connection to the given
-- @('HostName', 'PortNumber')@ combination. The socket and SSL connection are
-- closed and deleted after the user handler runs.
--
-- /Since: 1.2.0.0./
withSSLConnection ::
     SSLContext           -- ^ SSL context. See the @HsOpenSSL@
                          -- documentation for information on creating
                          -- this.
  -> HostName             -- ^ hostname to connect to
  -> PortNumber           -- ^ port number to connect to
  -> (InputStream ByteString -> OutputStream ByteString -> SSL -> IO a)
          -- ^ Action to run with the new connection
  -> IO a
withSSLConnection ctx host port action =
    E.bracket (connectSSL ctx host port) cleanup go

  where
    go (is, os, ssl, _) = action is os ssl

    cleanup (_, os, ssl, sock) = E.mask_ $ do
        eatException $! Streams.write Nothing os
        eatException $! SSL.shutdown ssl SSL.Unidirectional
        eatException $! N.close sock

    eatException m = void m `E.catch` (\(_::E.SomeException) -> return ())


