tcp-streams
===========

[![Hackage](https://img.shields.io/hackage/v/tcp-streams.svg?style=flat)](http://hackage.haskell.org/package/tcp-streams)
[![Build Status](https://travis-ci.org/didi-FP/tcp-streams.svg)](https://travis-ci.org/didi-FP/tcp-streams)

One stop solution for tcp client and server with tls support!

+ use [io-streams](https://hackage.haskell.org/package/io-streams) for auto read buffering and easy streamming process.

+ use [tls](http://hackage.haskell.org/package/tls) for tls connection.

Built-in [mozilla CA list](https://curl.haxx.se/docs/caextract.html) date: 2017/06/07. 

From v0.6 TLS using [HsOpenSSL](http://hackage.haskell.org/package/HsOpenSSL) is split into [tcp-streams-openssl](http://hackage.haskell.org/package/tcp-streams-openssl) due to the difficulties of setting up openssl on many platform.

Also take a look at [wire-stream](http://hackage.haskell.org/package/wire-streams), for serialize/deserialize data. Happy hacking!

Example
-------

```haskell
import           Data.Connection
import qualified System.IO.Streams.TCP     as TCP
import qualified Data.TLSSetting           as TLS
import qualified System.IO.Streams.TLS     as TLS

--  TCP Client
...
conn <- TCP.connect "127.0.0.1" 8888
send conn "Hello! World." ..  -- sending
res <- Stream.read (source conn) ..  -- receiving
close conn                ..  -- closing
...


-- TCP Server
...
sock <- TCP.bindAndListen 1024 8888
conn <- TCP.accept sock
req <- Stream.read (source conn) ..   -- receiving
send conn "GoodBye!" ..  -- sending
...


--  TLS Client
...
cp <- TLS.makeTLSClientParams (TLS.CustomCAStore "myCA.pem")
conn <- TLS.connect cp (Just "myCAName") "127.0.0.1" 8888
...


--  TLS Server
...
sp <- TLS.makeServerParams "server.crt" [] "server.key"
sock <- TCP.bindAndListen 1024 8889
conn <- TLS.accept sp sock
...


-- HTTPS Client
...
cp <- TLS.makeClientParams TLS.MozillaCAStore
conn <- TLS.connect cp Nothing "www.google.com" 443
send conn "GET / HTTP/1.1\r\n"
send conn "Host: www.google.com\r\n"
send conn "\r\n"
bs <- Stream.readExactly 1024 (source conn)
...
```

License
-------

Copyright (c) 2017, Winterland

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of winterland1989 nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
