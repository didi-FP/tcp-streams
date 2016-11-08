{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions for interfacing @io-streams@
-- with @HsOpenSSL@. @ssl/SSL@ here stand for @HsOpenSSL@ library, not the
-- deprecated SSL 2.0/3.0 protocol. the receive buffer size is 32752.
-- sending is unbuffered, anything write into 'OutputStream' will be immediately
-- send to underlying socket.
--
-- The same exceptions rule which applied to TCP apply here, with addtional
-- 'SSL.SomeSSLException` to be watched out.
--
-- This module is intended to be imported @qualified@, e.g.:
--
-- @
-- import qualified "Data.SSLSetting"           as SSL
-- import qualified "System.IO.Streams.OpenSSL" as SSL
-- @
--
module System.IO.Streams.OpenSSL
  ( -- * client
    connect
  , connectWithVerifier
  , withConnection
    -- * server
  , accept
    -- * helpers
  , sslToStreams
  , close
    -- * re-export helpers
  , module Data.OpenSSLSetting
  ) where

import qualified Control.Exception     as E
import           Control.Monad         (unless, void)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import           Data.OpenSSLSetting
import           Network.Socket        (HostName, PortNumber, Socket)
import qualified Network.Socket        as N
import           OpenSSL               (withOpenSSL)
import           OpenSSL.Session       (SSL, SSLContext)
import qualified OpenSSL.Session       as SSL
import qualified OpenSSL.X509          as X509
import           System.IO.Streams     (InputStream, OutputStream)
import qualified System.IO.Streams     as Streams
import qualified System.IO.Streams.TCP as TCP

bUFSIZ :: Int
bUFSIZ = 32752

-- | Given an existing HsOpenSSL 'SSL' connection, produces an 'InputStream' \/
-- 'OutputStream' pair.
--
sslToStreams :: SSL             -- ^ SSL connection object
             -> IO (InputStream ByteString, OutputStream ByteString)
sslToStreams ssl = do
    is <- Streams.makeInputStream input
    os <- Streams.makeOutputStream output
    return (is, os)

  where
    input = ( do
        s <- SSL.read ssl bUFSIZ
        return $! if S.null s then Nothing else Just s
        ) `E.catch` (\(_::E.SomeException) -> return Nothing)

    output Nothing  = return ()
    output (Just s) = SSL.write ssl s
{-# INLINABLE sslToStreams #-}

close :: SSL.SSL -> IO ()
close ssl = withOpenSSL $ do
    SSL.shutdown ssl SSL.Unidirectional
    maybe (return ()) N.close (SSL.sslSocket ssl)

-- | Convenience function for initiating an SSL connection to the given
-- @('HostName', 'PortNumber')@ combination.
--
-- This function will try to verify server's identity using a very simple algorithm,
-- which may not suit your need:
--
-- @
--   matchDomain :: String -> String -> Bool
--   matchDomain n1 n2 =
--       let n1' = reverse (splitDot n1)
--           n2' = reverse (splitDot n2)
--           cmp src target = src == "*" || target == "*" || src == target
--       in and (zipWith cmp n1' n2')
-- @
--
-- If the certificate or hostname is not verified, a 'SSL.ProtocolError' will be thrown.
--
connect :: SSLContext           -- ^ SSL context. See the @HsOpenSSL@
                                -- documentation for information on creating
                                -- this.
        -> Maybe String         -- ^ Optional certificate subject name, if set to 'Nothing'
                                -- then we will try to verify 'HostName' as subject name.
        -> HostName             -- ^ hostname to connect to
        -> PortNumber           -- ^ port number to connect to
        -> IO (InputStream ByteString, OutputStream ByteString, SSL)
connect ctx vhost host port = withOpenSSL $ do
    connectWithVerifier ctx verify host port
  where
    verify trusted cnname = trusted
                          && maybe False (matchDomain verifyHost) cnname
    verifyHost = maybe host id vhost
    matchDomain :: String -> String -> Bool
    matchDomain n1 n2 =
        let n1' = reverse (splitDot n1)
            n2' = reverse (splitDot n2)
            cmp src target = src == "*" || target == "*" || src == target
        in and (zipWith cmp n1' n2')
    splitDot :: String -> [String]
    splitDot "" = [""]
    splitDot x  =
        let (y, z) = break (== '.') x in
        y : (if z == "" then [] else splitDot $ drop 1 z)

-- | Connecting with a custom verification callback.
--
-- @since 0.6.0.0@
--
connectWithVerifier :: SSLContext       -- ^ SSL context. See the @HsOpenSSL@
                                        -- documentation for information on creating
                                        -- this.
                    -> (Bool -> Maybe String -> Bool) -- ^ A verify callback, the first param is
                                                -- the result of certificate verification, the
                                                -- second param is the certificate's subject name.
                    -> HostName             -- ^ hostname to connect to
                    -> PortNumber           -- ^ port number to connect to
                    -> IO (InputStream ByteString, OutputStream ByteString, SSL)
connectWithVerifier ctx f host port = withOpenSSL $ do
    sock <- TCP.connectSocket host port
    E.bracketOnError (SSL.connection ctx sock) close $ \ ssl -> do
        SSL.connect ssl
        trusted <- SSL.getVerifyResult ssl
        cert <- SSL.getPeerCertificate ssl
        subnames <- maybe (return []) (`X509.getSubjectName` False) cert
        let cnname = lookup "CN" subnames
            verified = f trusted cnname
        unless verified (E.throwIO $ SSL.ProtocolError "fail to verify certificate")
        (is, os) <- sslToStreams ssl
        return (is, os, ssl)


-- | Convenience function for initiating an SSL connection to the given
-- @('HostName', 'PortNumber')@ combination. The socket and SSL connection are
-- closed and deleted after the user handler runs.
--
withConnection :: SSLContext


               -> Maybe String
               -> HostName
               -> PortNumber
               -> (InputStream ByteString -> OutputStream ByteString -> SSL -> IO a)
                       -- ^ Action to run with the new connection
               -> IO a
withConnection ctx subname host port action =
    E.bracket (connect ctx subname host port) cleanup go

  where
    go (is, os, ssl) = action is os ssl

    cleanup (_, os, ssl) = E.mask_ $
        eatException $! Streams.write Nothing os >> close ssl

    eatException m = void m `E.catch` (\(_::E.SomeException) -> return ())


-- | Accept a new connection from remote client, return a 'InputStream' / 'OutputStream'
-- pair and remote 'N.SockAddr', you should call 'TCP.bindAndListen' first.
--
-- this operation will throw 'SSL.SomeSSLException' on failure.
--
accept :: SSL.SSLContext            -- ^ check "Data.OpenSSLSetting".
       -> Socket                    -- ^ the listening 'Socket'.
       -> IO (InputStream ByteString, OutputStream ByteString, SSL.SSL, N.SockAddr)
accept ctx sock = withOpenSSL $ do
    (sock', sockAddr) <- N.accept sock
    E.bracketOnError (SSL.connection ctx sock') close $ \ ssl -> do
        SSL.accept ssl
        trusted <- SSL.getVerifyResult ssl
        unless trusted (E.throwIO $ SSL.ProtocolError "fail to verify certificate")
        (is, os) <- sslToStreams ssl
        return (is, os, ssl, sockAddr)

