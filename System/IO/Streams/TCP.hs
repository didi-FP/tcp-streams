{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions for interfacing @io-streams@
-- with @tls@. It is intended to be imported @qualified@, e.g.:
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

module System.IO.Streams.TCP
  ( connect
  , withConnection
  , sslToStreams
  ) where

import qualified Control.Exception     as E
import           Control.Monad         (void)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as S
import           Network.Socket        (HostName, PortNumber)
import qualified Network.Socket        as N
import           System.IO.Streams     (InputStream, OutputStream)
import qualified System.IO.Streams     as Streams
import           System.IO.Streams.Raw
import           System.IO.Streams.SSL
import           System.IO.Streams.TLS

