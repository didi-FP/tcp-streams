{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

------------------------------------------------------------------------------
import           Control.Concurrent             (forkIO, newEmptyMVar, putMVar,
                                                 takeMVar, threadDelay)
import           Control.Monad                  (forever)
import qualified Network.Socket                 as N
import           System.Timeout                 (timeout)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
import qualified Data.ByteString                as B
------------------------------------------------------------------------------
import qualified Data.TLSSetting                as TLS
import qualified System.IO.Streams              as Stream
import qualified System.IO.Streams.Internal     as Stream
import qualified System.IO.Streams.TCP          as Raw
import qualified System.IO.Streams.TLS          as TLS
------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "TCP" rawTests
            , testGroup "TLS"  tlsTests
            , testGroup "HTTPS" httpsTests
            ]

------------------------------------------------------------------------------

rawTests :: [Test]
rawTests = [ testRawSocket ]

testRawSocket :: Test
testRawSocket = testCase "network/socket" $
    N.withSocketsDo $ do
    x <- timeout (10 * 10^(6::Int)) go
    assertEqual "ok" (Just ()) x

  where
    go = do
        portMVar   <- newEmptyMVar
        resultMVar <- newEmptyMVar
        forkIO $ client portMVar resultMVar
        server portMVar
        l <- takeMVar resultMVar
        assertEqual "testSocket" l ["ok"]

    client mvar resultMVar = do
        _ <- takeMVar mvar
        (is, os, sock) <- Raw.connect "127.0.0.1" 8888
        Stream.fromList ["", "ok"] >>= Stream.connectTo os
        N.shutdown sock N.ShutdownSend
        Stream.toList is >>= putMVar resultMVar
        N.sClose sock

    server mvar = do
        sock <- Raw.bindAndListen 8888 1024
        putMVar mvar ()
        (is, os, csock, _) <- Raw.accept sock
        Stream.toList is >>= flip Stream.writeList os
        os `Stream.connectTo` is
        N.sClose csock
        N.sClose sock

------------------------------------------------------------------------------

tlsTests :: [Test]
tlsTests = [ testTLSSocket ]

testTLSSocket :: Test
testTLSSocket = testCase "network/socket" $
    N.withSocketsDo $ do
    x <- timeout (10 * 10^(6::Int)) go
    assertEqual "ok" (Just ()) x

  where
    go = do
        portMVar   <- newEmptyMVar
        resultMVar <- newEmptyMVar
        forkIO $ client portMVar resultMVar
        server portMVar
        l <- takeMVar resultMVar
        assertEqual "testSocket" l (Just "ok")

    client mvar resultMVar = do
        _ <- takeMVar mvar
        cp <- TLS.makeClientParams (TLS.CustomCAStore "./test/cert/ca.pem")
        (is, os, ctx) <- TLS.connect cp (Just "Winter") "127.0.0.1" 8889
        Stream.fromList ["", "ok"] >>= Stream.connectTo os
        Stream.read is >>= putMVar resultMVar  -- There's no shutdown in tls, so we won't get a 'Nothing'
        TLS.closeTLS ctx

    server mvar = do
        sp <- TLS.makeServerParams "./test/cert/server.crt" [] "./test/cert/server.key"
        sock <- Raw.bindAndListen 8889 1024
        putMVar mvar ()
        (is, os, ctx, sockAddr) <- TLS.accept sp sock
        os `Stream.connectTo` is

------------------------------------------------------------------------------

httpsTests :: [Test]
httpsTests = [ testHTTPS ]

testHTTPS :: Test
testHTTPS = testCase "network/socket" $
    N.withSocketsDo $ do
    x <- timeout (10 * 10^(6::Int)) go
    assertEqual "ok" (Just 1024) x
  where
    go = do
        cp <- TLS.makeClientParams TLS.MozillaCAStore
        (is, os, ctx) <- TLS.connect cp Nothing "www.baidu.com" 443
        Stream.write (Just "GET / HTTP/1.1\r\n") os
        Stream.write (Just "Host: www.baidu.com\r\n") os
        Stream.write (Just "\r\n") os
        bs <- Stream.readExactly 1024 is
        return (B.length bs)
