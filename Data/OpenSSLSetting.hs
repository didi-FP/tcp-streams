-- | Helpers for setting up a tls connection with @HsOpenSSL@ package
--
-- Note, functions in this module will throw error if can't load certificates or CA store.
--
module Data.OpenSSLSetting
    ( -- * choose a CAStore
      TrustedCAStore(..)
      -- * make TLS settings
    , makeClientSSLContext
    , makeClientSSLContext'
    , makeServerSSLContext
    , makeServerSSLContext'
    ) where

import qualified OpenSSL.X509.SystemStore as X509
import qualified OpenSSL.Session          as SSL
import           Paths_tcp_streams          (getDataFileName)
import           Data.TLSSetting            (TrustedCAStore(..))


makeCAStore :: TrustedCAStore -> SSL.SSLContext -> IO ()
makeCAStore SystemCAStore  ctx  = X509.contextLoadSystemCerts ctx
makeCAStore MozillaCAStore ctx  = do
    fp <- getDataFileName "mozillaCAStore.pem"
    SSL.contextSetCAFile ctx fp
makeCAStore (CustomCAStore fp) ctx = SSL.contextSetCAFile ctx fp

-- | make a simple 'SSL.SSLContext' that will validate server and use tls connection
-- without providing client's own certificate. suitable for connecting server which don't
-- validate clients.
--
makeClientSSLContext :: TrustedCAStore          -- ^ trusted certificates.
                     -> IO SSL.SSLContext
makeClientSSLContext tca = do
    let caStore = makeCAStore tca
    ctx <- SSL.context
    caStore ctx
    SSL.contextSetDefaultCiphers ctx
    SSL.contextSetVerificationMode ctx (SSL.VerifyPeer True True Nothing)
    caStore ctx
    return ctx

-- | make a simple 'SSL.SSLContext' that will validate server and use tls connection
-- while providing client's own certificate. suitable for connecting server which
-- validate clients.
--
-- The certificates must be in PEM format and must be sorted starting with the subject's certificate
-- (actual client or server certificate), followed by intermediate CA certificates if applicable,
-- and ending at the highest level (root) CA.
--
makeClientSSLContext' :: FilePath       -- ^ public certificate (X.509 format).
                      -> [FilePath]       -- ^ chain certificate (X.509 format).
                      -> FilePath       -- ^ private key associated.
                      -> TrustedCAStore -- ^ server will use these certificates to validate clients.
                      -> IO SSL.SSLContext
makeClientSSLContext' pub certs priv tca = do
    let caStore = makeCAStore tca
    ctx <- SSL.context
    caStore ctx
    SSL.contextSetDefaultCiphers ctx
    SSL.contextSetCertificateFile ctx pub
    SSL.contextSetPrivateKeyFile ctx priv
    mapM_ (SSL.contextSetCertificateChainFile ctx) certs
    SSL.contextSetVerificationMode ctx (SSL.VerifyPeer True True Nothing)
    return ctx

-- | make a simple 'SSL.SSLContext' without validating client's certificate.
--
makeServerSSLContext :: FilePath       -- ^ public certificate (X.509 format).
                     -> [FilePath]     -- ^ chain certificate (X.509 format).
                     -> FilePath       -- ^ private key associated.
                     -> IO SSL.SSLContext
makeServerSSLContext pub certs priv = do
    ctx <- SSL.context
    SSL.contextSetDefaultCiphers ctx
    SSL.contextSetCertificateFile ctx pub
    SSL.contextSetPrivateKeyFile ctx priv
    mapM_ (SSL.contextSetCertificateChainFile ctx) certs
    return ctx

-- | make a tls 'TLS.ServerParams' that also validating client's certificate.
--
-- This's an alias to 'makeClientSSLContext'.
--
makeServerSSLContext' :: FilePath       -- ^ public certificate (X.509 format).
                      -> [FilePath]     -- ^ chain certificates (X.509 format).
                      -> FilePath       -- ^ private key associated.
                      -> TrustedCAStore -- ^ server will use these certificates to validate clients.
                      -> IO SSL.SSLContext
makeServerSSLContext' = makeClientSSLContext'
