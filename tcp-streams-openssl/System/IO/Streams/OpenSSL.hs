{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions for interfacing @HsOpenSSL@.
-- @ssl/SSL@ here stand for @HsOpenSSL@ library, not the deprecated SSL 2.0/3.0 protocol.
--
-- This module is intended to be imported @qualified@, e.g.:
--
-- @
-- import           "Data.Connection"
-- import qualified "System.IO.Streams.OpenSSL" as SSL
-- @
--
module System.IO.Streams.OpenSSL
  ( TLSConnection
    -- * client
  , connect
  , connectWithVerifier
  , sslToConnection
    -- * server
  , accept
    -- * re-export
  , module Data.OpenSSLSetting
  ) where

import qualified Control.Exception     as E
import           Control.Monad         (unless)
import           Data.Connection
import qualified Data.ByteString       as S
import           Data.OpenSSLSetting
import qualified Network.Socket        as N
import           OpenSSL               (withOpenSSL)
import qualified OpenSSL.Session       as SSL
import qualified OpenSSL.X509          as X509
import qualified System.IO.Streams     as Streams
import qualified System.IO.Streams.TCP as TCP

-- | Type alias for tls connection.
--
-- Normally you shouldn't use 'SSL.SSL' in 'connExtraInfo' directly.
--
type TLSConnection = Connection (SSL.SSL , N.SockAddr)

-- | Given an existing HsOpenSSL 'SSL' connection, produces an 'InputStream' \/
-- 'OutputStream' pair.
--
sslToConnection :: (SSL.SSL, N.SockAddr)             -- ^ SSL connection object
                -> IO TLSConnection
sslToConnection (ssl, addr) = do
    is <- Streams.makeInputStream input
    return (Connection is (SSL.lazyWrite ssl) (closeSSL ssl) (ssl, addr))
  where
    input = ( do
        s <- SSL.read ssl TCP.defaultChunkSize
        return $! if S.null s then Nothing else Just s
        ) `E.catch` (\(_::E.SomeException) -> return Nothing)

closeSSL :: SSL.SSL -> IO ()
closeSSL ssl = withOpenSSL $ do
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
connect :: SSL.SSLContext           -- ^ SSL context, see the @HsOpenSSL@
                                -- documentation for more information
        -> Maybe String         -- ^ Optional certificate subject name, if set to 'Nothing'
                                -- then we will try to verify 'HostName' as subject name
        -> N.HostName           -- ^ hostname to connect to
        -> N.PortNumber         -- ^ port number to connect to
        -> IO TLSConnection
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
connectWithVerifier :: SSL.SSLContext       -- ^ SSL context. See the @HsOpenSSL@
                                        -- documentation for information on creating
                                        -- this.
                    -> (Bool -> Maybe String -> Bool) -- ^ A verify callback, the first param is
                                              -- the result of certificate verification, the
                                              -- second param is the certificate's subject name
                    -> N.HostName             -- ^ hostname to connect to
                    -> N.PortNumber           -- ^ port number to connect to
                    -> IO TLSConnection
connectWithVerifier ctx f host port = withOpenSSL $ do
    (sock, addr) <- TCP.connectSocket host port
    E.bracketOnError (SSL.connection ctx sock) closeSSL $ \ ssl -> do
        SSL.connect ssl
        trusted <- SSL.getVerifyResult ssl
        cert <- SSL.getPeerCertificate ssl
        subnames <- maybe (return []) (`X509.getSubjectName` False) cert
        let cnname = lookup "CN" subnames
            verified = f trusted cnname
        unless verified (E.throwIO $ SSL.ProtocolError "fail to verify certificate")
        sslToConnection (ssl, addr)

-- | Accept a new connection from remote client, return a 'InputStream' / 'OutputStream'
-- pair and remote 'N.SockAddr', you should call 'TCP.bindAndListen' first.
--
-- this operation will throw 'SSL.SomeSSLException' on failure.
--
accept :: SSL.SSLContext            -- ^ check "Data.OpenSSLSetting"
       -> N.Socket                  -- ^ the listening 'Socket'
       -> IO TLSConnection
accept ctx sock = withOpenSSL $ do
    (sock', addr) <- N.accept sock
    E.bracketOnError (SSL.connection ctx sock') closeSSL $ \ ssl -> do
        SSL.accept ssl
        trusted <- SSL.getVerifyResult ssl
        unless trusted (E.throwIO $ SSL.ProtocolError "fail to verify certificate")
        sslToConnection (ssl, addr)
