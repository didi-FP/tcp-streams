{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions for interfacing raw tcp.
--
-- Reading 'InputStream' will block until GHC IO manager find data is ready,
-- for example 'System.IO.Streams.ByteString.readExactly 1024' will block until 1024 bytes are available.
--
-- This module is intended to be imported @qualified@, e.g.:
--
-- @
-- import qualified "System.IO.Streams.TCP" as TCP
-- @
--
module System.IO.Streams.TCP
  ( -- * connection type
    Connection(..)
    -- * tcp client
  , connect
  , connectSocket
  , socketToConnection
  , defaultChunkSize
    -- * tcp server
  , bindAndListen
  , accept
  , acceptWith
  ) where

import qualified Control.Exception         as E
import           Control.Monad             (unless, void)
import           Data.Connection
import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy.Internal as L
import qualified Network.Socket            as N
import qualified Network.Socket.ByteString as NB
import qualified Network.Socket.ByteString.Lazy as NL
import qualified System.IO.Streams         as S
import           Foreign.Storable   (sizeOf)

-- | The chunk size used for I\/O, less the memory management overhead.
--
-- Currently set to 32k.
--
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
  where
    k = 1024
    chunkOverhead = 2 * sizeOf (undefined :: Int)


-- | Convenience function for initiating an raw TCP connection to the given
-- @('HostName', 'PortNumber')@ combination.
--
-- It use 'N.getAddrInfo' to resolve host/service name
-- with 'N.AI_ADDRCONFIG', 'N.AI_NUMERICSERV' hint set, so it should be able to
-- resolve both numeric IPv4/IPv6 hostname and domain name.
--
-- `TCP_NODELAY` are enabled by default. you can use 'N.setSocketOption' to adjust.
--
connectSocket :: N.HostName             -- ^ hostname to connect to
              -> N.PortNumber           -- ^ port number to connect to
              -> IO (N.Socket, N.SockAddr)
connectSocket host port = do
    (family, socketType, protocol, address) <- resolveAddrInfo host port
    E.bracketOnError (N.socket family socketType protocol)
                     N.close
                     (\sock -> do N.connect sock address
                                  -- NoDelay causes an error for AF_UNIX.
                                  E.catch
                                    (N.setSocketOption sock N.NoDelay 1)
                                    (\ (E.SomeException _) -> return ())
                                  return (sock, address)
                     )
  where
    resolveAddrInfo host port = do
        -- Partial function here OK, network will throw an exception rather than
        -- return the empty list here.
        (addrInfo:_) <- N.getAddrInfo (Just hints) (Just host) (Just $ show port)
        let family     = N.addrFamily addrInfo
        let socketType = N.addrSocketType addrInfo
        let protocol   = N.addrProtocol addrInfo
        let address    = N.addrAddress addrInfo
        return (family, socketType, protocol, address)
      where
        hints = N.defaultHints {
                N.addrFlags      = [N.AI_ADDRCONFIG, N.AI_NUMERICSERV]
            ,   N.addrSocketType = N.Stream
            }
    {-# INLINABLE resolveAddrInfo #-}

-- | Make a 'Connection' from a 'Socket' with given buffer size.
--
socketToConnection
    :: Int                      -- ^ receive buffer size
    -> (N.Socket, N.SockAddr)       -- ^ socket address pair
    -> IO Connection
socketToConnection bufsiz (sock, addr) = do
    is <- S.makeInputStream $ do
        s <- NB.recv sock bufsiz
        return $! if B.null s then Nothing else Just s
    return (Connection is (send sock) (N.close sock) addr)
  where
    send sock (L.Empty) = return ()
    send sock (L.Chunk bs L.Empty) = unless (B.null bs) (NB.sendAll sock bs)
    send sock lbs = NL.sendAll sock lbs

-- | Connect to server using 'defaultChunkSize'.
--
connect :: N.HostName             -- ^ hostname to connect to
        -> N.PortNumber           -- ^ port number to connect to
        -> IO Connection
connect host port = connectSocket host port >>= socketToConnection defaultChunkSize

-- | Bind and listen on port with a limit on connection count.
--
bindAndListen :: Int         -- connection limit
              -> N.PortNumber
              -> IO N.Socket
bindAndListen maxc port = do
    E.bracketOnError (N.socket N.AF_INET N.Stream 0)
                     N.close
                     (\sock -> do
                                  -- NoDelay causes an error for AF_UNIX.
                                  E.catch
                                    (do
                                        N.setSocketOption sock N.ReuseAddr 1
                                        N.setSocketOption sock N.NoDelay 1)
                                    (\ (E.SomeException _) -> return ())
                                  N.bind sock (N.SockAddrInet port N.iNADDR_ANY)
                                  N.listen sock maxc
                                  return sock
                     )

-- | Accept a connection with 'defaultChunkSize'
--
accept :: N.Socket -> IO Connection
accept sock = acceptWith sock (socketToConnection defaultChunkSize)

-- | Accept a connection with user customization.
--
acceptWith :: N.Socket
           -> ((N.Socket, N.SockAddr) -> IO Connection)
           -> IO Connection
acceptWith sock f = f =<< N.accept sock
