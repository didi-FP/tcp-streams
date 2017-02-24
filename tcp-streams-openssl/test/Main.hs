{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

------------------------------------------------------------------------------
import           Control.Concurrent             (forkIO, newEmptyMVar, putMVar,
                                                 takeMVar)
import qualified Control.Exception              as E
import qualified Network.Socket                 as N
import           System.Timeout                 (timeout)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as L
import           System.Directory               (removeFile)
------------------------------------------------------------------------------
import           Data.Connection
import qualified Data.OpenSSLSetting            as SSL
import qualified System.IO.Streams              as Stream
import qualified System.IO.Streams.TCP          as TCP
import qualified System.IO.Streams.OpenSSL      as SSL
------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests
  where
    tests = [ testGroup "TCP" tcpTests
            , testGroup "OpenSSL" sslTests
            ]

------------------------------------------------------------------------------

tcpTests :: [Test]
tcpTests = [ testTCPSocket ]

testTCPSocket :: Test
testTCPSocket = testCase "network/socket" $
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
        conn <- TCP.connect "127.0.0.1" 8888
        send conn "ok"
        Stream.toList (source conn) >>= putMVar resultMVar
        close conn

    server mvar = do
        sock <- TCP.bindAndListen 1024 8888
        putMVar mvar ()
        conn <- TCP.accept sock
        req <- Stream.readExactly 2 (source conn)
        send conn (L.fromStrict req)
        close conn

------------------------------------------------------------------------------

sslTests :: [Test]
sslTests = [ testSSLSocket, testHTTPS' ]

testSSLSocket :: Test
testSSLSocket = testCase "network/socket" $
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
        cp <- SSL.makeClientSSLContext (SSL.CustomCAStore "./test/cert/ca.pem")
        conn <- SSL.connect cp (Just "Winter") "127.0.0.1" 8890
        send conn "ok"
        Stream.toList (source conn) >>= putMVar resultMVar
        close conn

    server mvar = do
        sp <- SSL.makeServerSSLContext "./test/cert/server.crt" [] "./test/cert/server.key"
        sock <- TCP.bindAndListen 1024 8890
        putMVar mvar ()
        conn <- SSL.accept sp sock
        req <- Stream.readExactly 2 (source conn)
        send conn (L.fromStrict req)
        close conn

testHTTPS' :: Test
testHTTPS' = testCase "network/https" $
    N.withSocketsDo $ do
    x <- timeout (10 * 10^(6::Int)) go
    assertEqual "ok" (Just 1024) x
  where
    go = do
        cp <- SSL.makeClientSSLContext SSL.SystemCAStore
        conn <- SSL.connect cp (Just "*.bing.com") "www.bing.com" 443
        send conn "GET / HTTP/1.1\r\n"
        send conn "Host: www.bing.com\r\n"
        send conn "\r\n"
        bs <- Stream.readExactly 1024 (source conn)
        close conn
        return (B.length bs)
