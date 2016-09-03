{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Concurrent             (forkIO, newEmptyMVar, putMVar,
                                                 takeMVar)
import qualified Control.Exception              as E
import qualified Network.Socket                 as N
import qualified Data.ByteString                as B
------------------------------------------------------------------------------
import qualified Data.TLSSetting                as TLS
import qualified Data.OpenSSLSetting            as SSL
import qualified System.IO.Streams              as Stream
import qualified System.IO.Streams.TCP          as TCP
import qualified System.IO.Streams.TLS          as TLS
import qualified System.IO.Streams.OpenSSL      as SSL

main :: IO ()
main = N.withSocketsDo $ do
    portMVar   <- newEmptyMVar
    resultMVar <- newEmptyMVar
    forkIO $ server portMVar
    client portMVar resultMVar
    takeMVar resultMVar
  where
    chunk = B.replicate (1024 * 1024 * 1024) 64
    client mvar resultMVar = do
        _ <- takeMVar mvar
        (is, os, sock) <- TCP.connectWithBufferSize "127.0.0.1" 8123 16384
        forkIO $ do
            print "??"
            Stream.fromList (replicate 1 chunk) >>= Stream.connectTo os
            print "??"
        echo <- Stream.readExactly (1024 * 1024 * 1024) is
        -- echo <- Stream.foldM (\ i s -> when (i `mod` 100 == 0) (print i) >> return (i+1)) 0 is
        print (B.length echo)
        -- print echo
        N.shutdown sock N.ShutdownSend
        putMVar resultMVar ()
        N.close sock

    server mvar = do
        sock <- TCP.bindAndListen 8123 1024
        putMVar mvar ()
        (is, os, csock, addr) <- TCP.accept sock
        os `Stream.connectTo` is
