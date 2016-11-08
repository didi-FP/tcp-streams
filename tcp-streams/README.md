tcp-streams
===========

[![Hackage](https://img.shields.io/hackage/v/tcp-streams.svg?style=flat)](http://hackage.haskell.org/package/tcp-streams)
[![Build Status](https://travis-ci.org/winterland1989/tcp-streams.svg)](https://travis-ci.org/winterland1989/tcp-streams)

One stop solution for tcp client and server with tls support!

+ use [io-streams](https://hackage.haskell.org/package/io-streams) for auto read buffering and easy streamming process.

+ use [tls](http://hackage.haskell.org/package/tls) or [HsOpenSSL](http://hackage.haskell.org/package/HsOpenSSL) for tls connection.

Built-in [mozilla CA list](https://curl.haxx.se/docs/caextract.html) date: 2016/11/02. 

Mac user may need manually passing openssl library path if linker can't find them:

```
cabal install --extra-include-dirs=/usr/local/opt/openssl/include --extra-lib-dirs=/usr/local/opt/openssl/lib tcp-streams
```

You can disable openssl support with `cabal install -f -openssl`.

Also take a look at [wire-stream](http://hackage.haskell.org/package/wire-streams-0.0.2.0), for serialize/deserialize data. Happy hacking!

Example
-------

```haskell
import qualified System.IO.Streams         as Stream
import qualified System.IO.Streams.TCP     as TCP
import qualified Data.TLSSetting           as TLS
import qualified System.IO.Streams.TLS     as TLS

--  TCP Client
...
(is, os, sock) <- TCP.connect "127.0.0.1" 8888
Stream.write os =<< ..  -- sending
Stream.read is >>=  ..  -- receiving
TCP.close sock   ..  -- closing
...


-- TCP Server
...
sock <- TCP.bindAndListen 8888 1024
(is, os, csock, _) <- TCP.accept sock
Stream.write os =<< ..  -- sending
Stream.read is >>= ..   -- receiving
...


--  TLS Client
...
cp <- TLS.makeTLSClientParams (TLS.CustomCAStore "myCA.pem")
(is, os, ctx) <- TCP.connect cp (Just "myCAName") "127.0.0.1" 8888
Stream.write os =<< ..  -- sending
Stream.read is >>=  ..  -- receiving
TLS.close ctx    ..  -- closing
...


--  TLS Server
...
sp <- TLS.makeServerParams "server.crt" [] "server.key"
sock <- TCP.bindAndListen 8889 1024
(is, os, ctx, sockAddr) <- TLS.accept sp sock
Stream.write os =<< ..  -- sending
Stream.read is >>= ..   -- receiving
...


-- HTTPS Client
...
cp <- TLS.makeClientParams TLS.MozillaCAStore
(is, os, ctx) <- TLS.connect cp Nothing "www.google.com" 443
Stream.write (Just "GET / HTTP/1.1\r\n") os
Stream.write (Just "Host: www.google.com\r\n") os
Stream.write (Just "\r\n") os
bs <- Stream.readExactly 1024 is
...
```

License
-------

Copyright (c) 2016, Winterland

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
