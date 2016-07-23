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
-- Be sure to use 'withOpenSSL' wrap your operation before using any functions here.
-- otherwise a segmentation fault will happen.
--
module System.IO.Streams.OpenSSL
  ( -- * client
    connect
  , withConnection
    -- * server
  , accept
    -- * helpers
  , withOpenSSL
  , sslToStreams
  , closeSSL
  ) where

import qualified Control.Exception     as E
import           Control.Monad         (unless, void)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
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
    input = do
        s <- SSL.read ssl bUFSIZ
        return $! if S.null s then Nothing else Just s
        `E.onException` return Nothing

    output Nothing  = return ()
    output (Just s) = SSL.write ssl s

closeSSL :: SSL.SSL -> IO ()
closeSSL ssl = do
    SSL.shutdown ssl SSL.Unidirectional
    maybe (return ()) N.close (SSL.sslSocket ssl)

-- | Convenience function for initiating an SSL connection to the given
-- @('HostName', 'PortNumber')@ combination.
--
-- this function will try to verify server's identity,
-- a 'SSL.ProtocolError' will be thrown if fail.
--
connect :: SSLContext           -- ^ SSL context. See the @HsOpenSSL@
                                -- documentation for information on creating
                                -- this.
        -> Maybe String         -- ^ Optional certificate subject name, if set to 'Nothing'
                                -- then we will try to verify 'HostName' as subject name.
        -> HostName             -- ^ hostname to connect to
        -> PortNumber           -- ^ port number to connect to
        -> IO (InputStream ByteString, OutputStream ByteString, SSL)
connect ctx subname host port = do
    sock <- TCP.connectSocket host port
    E.bracketOnError (SSL.connection ctx sock) closeSSL $ \ ssl -> do
        SSL.connect ssl
        trusted <- SSL.getVerifyResult ssl
        cert <- SSL.getPeerCertificate ssl
        subnames <- maybe (return []) (`X509.getSubjectName` False) cert
        let cnname = lookup "CN" subnames
            verified = case subname of
                Just subname' -> maybe False (== subname') cnname
                Nothing       -> maybe False (matchDomain host) cnname
        unless (trusted && verified) (E.throwIO $ SSL.ProtocolError "fail to verify certificate")
        (is, os) <- sslToStreams ssl
        return (is, os, ssl)

  where
    matchDomain :: String -> String -> Bool
    matchDomain n1 n2 =
        let n1' = reverse (splitDot n1)
            n2' = reverse (splitDot n2)
            cmp src target = src == "*" || src == target
        in and (zipWith cmp n1' n2')
    splitDot :: String -> [String]
    splitDot "" = [""]
    splitDot x  =
        let (y, z) = break (== '.') x in
        y : (if z == "" then [] else splitDot $ drop 1 z)

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
        eatException $! Streams.write Nothing os >> closeSSL ssl

    eatException m = void m `E.catch` (\(_::E.SomeException) -> return ())


-- | accept a new connection from remote client, return a 'InputStream' / 'OutputStream'
-- pair and remote 'N.SockAddr', you should call 'TCP.bindAndListen' first.
--
-- this operation will throw 'SSL.SomeSSLException' on failure.
--
accept :: SSL.SSLContext            -- ^ check "Data.OpenSSLSetting".
       -> Socket                    -- ^ the listening 'Socket'.
       -> IO (InputStream ByteString, OutputStream ByteString, SSL.SSL, N.SockAddr)
accept ctx sock = do
    (sock', sockAddr) <- N.accept sock
    E.bracketOnError (SSL.connection ctx sock') closeSSL $ \ ssl -> do
        SSL.accept ssl
        trusted <- SSL.getVerifyResult ssl
        unless trusted (E.throwIO $ SSL.ProtocolError "fail to verify certificate")
        (is, os) <- sslToStreams ssl
        return (is, os, ssl, sockAddr)

