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

module System.IO.Streams.Raw
  ( connectSocket
  , connect
  , connectWithBufferSize
  , withConnection
  ) where

import qualified Control.Exception         as E
import           Control.Monad             (void)
import           Data.ByteString.Char8     (ByteString)
import           Network.Socket            (HostName, PortNumber, Socket)
import qualified Network.Socket            as N
import           System.IO.Streams         (InputStream, OutputStream)
import qualified System.IO.Streams         as Streams
import           System.IO.Streams.Network (socketToStreamsWithBufferSize)


------------------------------------------------------------------------------

bUFSIZ :: Int
bUFSIZ = 32752

------------------------------------------------------------------------------
-- | Convenience function for initiating an raw TCP connection to the given
-- @('HostName', 'PortNumber')@ combination.
--
-- Note that sending an end-of-file to the returned 'OutputStream' will not
-- close the underlying Socket connection; to do that, call:
--
-- @
-- SSL.'SSL.shutdown' ssl SSL.'SSL.Unidirectional'
-- maybe (return ()) 'N.close' $ SSL.'SSL.sslSocket' ssl
-- @
--
-- on the returned 'SSL' object.
connectSocket :: HostName             -- ^ hostname to connect to
              -> PortNumber           -- ^ port number to connect to
              -> IO Socket
connectSocket host port = do
    -- Partial function here OK, network will throw an exception rather than
    -- return the empty list here.
    (addrInfo:_) <- N.getAddrInfo (Just hints) (Just host) (Just $ show port)

    let family     = N.addrFamily addrInfo
    let socketType = N.addrSocketType addrInfo
    let protocol   = N.addrProtocol addrInfo
    let address    = N.addrAddress addrInfo

    E.bracketOnError (N.socket family socketType protocol)
                     N.close
                     (\sock -> do N.connect sock address
                                  -- NoDelay causes an error for AF_UNIX.
                                  E.catch
                                    (N.setSocketOption sock N.NoDelay 1)
                                    (\ (E.SomeException _) -> return ())
                                  return sock
                     )
  where
    hints = N.defaultHints {
              N.addrFlags      = [N.AI_ADDRCONFIG, N.AI_NUMERICSERV]
            , N.addrSocketType = N.Stream
            }

------------------------------------------------------------------------------

connect :: HostName             -- ^ hostname to connect to
        -> PortNumber           -- ^ port number to connect to
        -> IO (InputStream ByteString, OutputStream ByteString, Socket)
connect host port = do
    sock <- connectSocket host port
    (is, os) <- socketToStreamsWithBufferSize bUFSIZ sock
    return (is, os, sock)

------------------------------------------------------------------------------

connectWithBufferSize :: HostName             -- ^ hostname to connect to
                      -> PortNumber           -- ^ port number to connect to
                      -> Int                  -- ^ tcp read buffer size
                      -> IO (InputStream ByteString, OutputStream ByteString, Socket)
connectWithBufferSize host port bufsiz = do
    sock <- connectSocket host port
    (is, os) <- socketToStreamsWithBufferSize bufsiz sock
    return (is, os, sock)

------------------------------------------------------------------------------
-- | Convenience function for initiating an SSL connection to the given
-- @('HostName', 'PortNumber')@ combination. The socket and SSL connection are
-- closed and deleted after the user handler runs.
--
-- /Since: 1.2.0.0./
withConnection
    :: HostName             -- ^ hostname to connect to
    -> PortNumber           -- ^ port number to connect to
    -> (InputStream ByteString -> OutputStream ByteString -> Socket -> IO a)
          -- ^ Action to run with the new connection
    -> IO a
withConnection host port action =
    E.bracket (connect host port) cleanup go

  where
    go (is, os, sock) = action is os sock

    cleanup (_, os, sock) = E.mask_ $ do
        eatException $! Streams.write Nothing os
        eatException $! N.close sock

    eatException m = void m `E.catch` (\(_::E.SomeException) -> return ())



