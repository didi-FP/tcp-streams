-- | Helpers for setting up a tls connection with @tls@ package
--
module Data.TLSSetting
    (   -- * choose a CAStore
        TrustedCAStore(..)
    ,   makeCAStore
        -- * make TLS settings
    ,   makeClientParams
    ,   makeClientParams'
    ,   makeServerParams
    ,   makeServerParams'
    ) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import           Data.Default.Class         (def)
import qualified Data.PEM                   as TLS_X509
import qualified Data.X509                  as TLS_X509
import qualified Data.X509.CertificateStore as TLS_X509
import           Network.Socket             (HostName)
import qualified Network.TLS                as TLS
import qualified Network.TLS.Extra          as TLS
import           Paths_tcp_streams          (getDataFileName)
import qualified System.X509                as TLS_X509

-- | The whole point of TLS is that client have already trusted
-- some certificates 'TrustedCAStore',
-- Then client can use such certificates for validating,
-- if the certificates chain sent by server was issued by one of CA in 'TrustedCAStore',
-- The server will be trusted.
-- if server want to validate client's identity, it'll use the same scheme.
--
data TrustedCAStore
    = SystemCAStore                   -- ^ provided by your operating system
    | MozillaCAStore                  -- ^ provided by <https://curl.haxx.se/docs/caextract.html Mozilla>
    | CustomCAStore FilePath          -- ^ provided by your self, a CA file can contain multiple certificates
                                      --   to form a certificate chain

  deriving (Show, Eq)

mozillaCAStore :: IO FilePath
mozillaCAStore = getDataFileName "mozillaCAStore20160420.pem"

makeCAStore :: TrustedCAStore -> IO TLS_X509.CertificateStore
makeCAStore SystemCAStore = TLS_X509.getSystemCertificateStore
makeCAStore MozillaCAStore = makeCAStore . CustomCAStore =<< mozillaCAStore
makeCAStore (CustomCAStore fp) = do
    bs <- B.readFile fp
    let Right pems = TLS_X509.pemParseBS bs
    case mapM (TLS_X509.decodeSignedCertificate . TLS_X509.pemContent) pems of
        Right cas -> return (TLS_X509.makeCertificateStore cas)


-- | make a simple tls 'TLS.ClientParams' that will validate server and use tls connection
-- without providing client's own certificate. suitable for connecting server which don't
-- validate clients.
--
-- Note, tls's default validating method require server has v3 certificate.
-- You can use openssl's V3 extension to issue such a certificate.
--
makeClientParams :: (HostName, ByteString)   -- ^ hostname which should match with certificate, with
                                                -- identitifer to distinguish different certificate on the same hostname
                                                -- (for example, service port)
                    -> TrustedCAStore           -- ^ trusted certificates
                    -> IO TLS.ClientParams
makeClientParams (host, cid) tca = do
    caStore <- makeCAStore tca
    return (TLS.defaultParamsClient host cid)
        {   TLS.clientSupported = def { TLS.supportedCiphers = TLS.ciphersuite_all }
        ,   TLS.clientShared    = def
            {   TLS.sharedCAStore         = caStore
            ,   TLS.sharedValidationCache = def
            }
        }

-- | make a simple tls 'TLS.ClientParams' that will validate server and use tls connection
-- while providing client's own certificate as well. suitable for connecting server which
-- validate clients.
--
-- Also only accept v3 certificate.
--
makeClientParams' :: FilePath   -- ^ public certificate (X.509 format)
                     -> [FilePath] -- ^ chain certificates (X.509 format)
                                   --   the root of your certificate chain should be
                                   --   already trusted by server, or tls will fail.
                     -> FilePath   -- ^ private key associated
                     -> (HostName, ByteString)  -- ^ same as 'makeTLSClientParams'
                     -> TrustedCAStore -- ^ trusted certificates
                     -> IO TLS.ClientParams
makeClientParams' pub certs priv host_cid tca = do
    p <- makeClientParams host_cid tca
    c <- TLS.credentialLoadX509Chain pub certs priv
    case c of
        Right c' ->
            return p
                {   TLS.clientShared = (TLS.clientShared p)
                    {
                        TLS.sharedCredentials = TLS.Credentials [c']
                    }
                }
        Left err -> error err

-- | make a simple tls 'TLS.ServerParams' without validating client's certificate.
--
makeServerParams :: FilePath   -- ^ public certificate (X.509 format)
                    -> [FilePath] -- ^ chain certificates (X.509 format)
                                  --   the root of your certificate chain should be
                                  --   already trusted by client, or tls will fail.
                    -> FilePath   -- ^ private key associated
                    -> IO TLS.ServerParams
makeServerParams pub certs priv = do
    c <- TLS.credentialLoadX509Chain pub certs priv
    case c of
        Right c'@(TLS_X509.CertificateChain c'', _) ->
            return def
                {   TLS.serverCACertificates =  c''
                ,   TLS.serverShared = def
                    {
                        TLS.sharedCredentials = TLS.Credentials [c']
                    }
                ,   TLS.serverSupported = def { TLS.supportedCiphers = TLS.ciphersuite_strong }
                }
        Left err -> error err

-- | make a tls 'TLS.ServerParams' that also validating client's certificate.
--
makeServerParams' :: FilePath   -- ^ public certificate (X.509 format)
                     -> [FilePath] -- ^ chain certificates (X.509 format)
                     -> FilePath   -- ^ private key associated
                     -> TrustedCAStore  -- ^ server use these trusted certificates to validate client
                     -> IO TLS.ServerParams
makeServerParams' pub certs priv tca = do
    caStore <- makeCAStore tca
    p <- makeServerParams pub certs priv
    return p
        {   TLS.serverWantClientCert = True
        ,   TLS.serverShared = (TLS.serverShared p)
            {   TLS.sharedCAStore = caStore
            }
        }
