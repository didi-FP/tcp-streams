{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}

module Main (main) where

------------------------------------------------------------------------------
import           Control.Concurrent             (forkIO, newEmptyMVar, putMVar,
                                                 takeMVar, threadDelay)
import qualified Control.Exception              as E
import qualified Network.Socket                 as N
import           System.Timeout                 (timeout)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
import qualified Data.ByteString                as B
import           System.Directory               (removeFile)
------------------------------------------------------------------------------
import qualified Data.TLSSetting                as TLS
import qualified System.IO.Streams              as Stream
import qualified System.IO.Streams.TCP          as Raw
import qualified System.IO.Streams.TCP.Timeout  as RawTimeout
import qualified System.IO.Streams.TLS          as TLS
------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "TCP" rawTests
            , testGroup "TLS" tlsTests
            ]

------------------------------------------------------------------------------

rawTests :: [Test]
rawTests = [ testRawSocket
           , testRawSocketClientTimeout
           , testRawSocketServerTimeout
           ]

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
        N.close sock

    server mvar = do
        sock <- Raw.bindAndListen 8888 1024
        putMVar mvar ()
        (is, os, csock, _) <- Raw.accept sock
        os' <- Stream.atEndOfOutput (N.close csock) os
        os' `Stream.connectTo` is

testRawSocketClientTimeout :: Test
testRawSocketClientTimeout = testCase "network/socket client timeout" $
    N.withSocketsDo $ do
    x <- timeout (10 * 10^(6::Int)) go
    assertEqual "ok" (Just ()) x

  where
    go = do
        portMVar   <- newEmptyMVar
        resultMVar <- newEmptyMVar
        forkIO $ server portMVar resultMVar (2 * 10 ^ (6::Int))
        client portMVar (1 * 10 ^ (6::Int))
        l <- takeMVar resultMVar
        assertEqual "testRawSocketClientTimeout" l (Right ["ok"])
        client portMVar (3 * 10 ^ (6::Int))
        l <- takeMVar resultMVar
        assertEqual "testRawSocketClientTimeout" l (Left RawTimeout.TimeoutException)

    client mvar n = do
        _ <- takeMVar mvar
        (_, os, sock) <- Raw.connect "127.0.0.1" 8890
        Stream.fromList ["", "ok"] >>= Stream.connectTo os
        threadDelay n
        N.shutdown sock N.ShutdownSend
        N.close sock

    server mvar resultMVar n = do
        sock <- Raw.bindAndListen 8890 1024
        let serveLoop = do
             putMVar mvar ()
             (is, _, csock, _) <- RawTimeout.accept sock n
             E.try (Stream.toList is) >>= putMVar resultMVar
             N.close csock
        serveLoop >> serveLoop -- run TCP server twice.

testRawSocketServerTimeout :: Test
testRawSocketServerTimeout = testCase "network/socket server timeout" $
    N.withSocketsDo $ do
    x <- timeout (10 * 10^(6::Int)) go
    assertEqual "ok" (Just ()) x

  where
    go = do
        portMVar   <- newEmptyMVar
        resultMVar <- newEmptyMVar
        forkIO $ server portMVar
        client portMVar resultMVar (1 * 10 ^ (6::Int))
        l <- takeMVar resultMVar
        assertEqual "testSocketServerTimeout" l (Left RawTimeout.TimeoutException)
        client portMVar resultMVar (3 * 10 ^ (6::Int))
        l <- takeMVar resultMVar
        assertEqual "testSocketServerTimeout" l (Right ["ok"])

    client mvar resultMVar n = do
        _ <- takeMVar mvar
        (is, os, sock) <- RawTimeout.connect "127.0.0.1" 8891 n
        Stream.fromList ["", "ok"] >>= Stream.connectTo os
        N.shutdown sock N.ShutdownSend
        E.try (Stream.toList is) >>= putMVar resultMVar
        N.close sock

    server mvar = do
        sock <- Raw.bindAndListen 8891 1024
        let serveLoop = do
             putMVar mvar ()
             (is, os, csock, _) <- Raw.accept sock
             os' <- Stream.atEndOfOutput (N.close csock) os
             threadDelay (2 * 10 ^ (6::Int))
             os' `Stream.connectTo` is
        serveLoop >> serveLoop -- run TCP server twice.

------------------------------------------------------------------------------

tlsTests :: [Test]
tlsTests = [ testTLSSocket, testHTTPS ]

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
        TLS.close ctx

    server mvar = do
        sp <- TLS.makeServerParams "./test/cert/server.crt" [] "./test/cert/server.key"
        sock <- Raw.bindAndListen 8889 1024
        putMVar mvar ()
        (is, os, tls, _) <- TLS.accept sp sock
        os' <- Stream.atEndOfOutput (TLS.close tls) os
        os' `Stream.connectTo` is

testHTTPS :: Test
testHTTPS = testCase "network/https" $
    N.withSocketsDo $ do
    x <- timeout (10 * 10^(6::Int)) go
    assertEqual "ok" (Just 1024) x
  where
    go = do
        cp <- TLS.makeClientParams TLS.MozillaCAStore
        (is, os, ctx) <- TLS.connect cp Nothing "www.google.com" 443
        Stream.write (Just "GET / HTTP/1.1\r\n") os
        Stream.write (Just "Host: www.google.com\r\n") os
        Stream.write (Just "\r\n") os
        bs <- Stream.readExactly 1024 is
        TLS.close ctx
        return (B.length bs)
