# Revision history for tcp-streams

## 1.0.0.0

Major rewrite using new `Connection` interface.

* Introduce `Connection` in `Data.Connection` to leaverage vectorized IO.
* Now `connect`, `accept` return `Connection`, remove `withXXX` functions.
* Change exception behavior to work better with `bracket`.
* `bindAndListen` 's type changed from `PortNumber -> Int -> IO Socket` to `Int -> PortNumber -> IO Socket`.
* Add `bindAndListenWith` for custom socket options.
* Add unix domain socket support.
* Update built-in mozilla CA list(2017/01/18).

## 0.7.0.0

* Add built-in timeout support
* Change behavior using io-streams' convention: 
    * reading `InputStream` will throw exception, and the socket won't be closed.
    * writing to `OutputStream` will not cause a write until flush.
    * writing empty `ByteString` to `OutputStream` will do a flush.

## 0.6.0.0

* Update built-in mozilla CA list(2016/11/02).
* Split openssl part into [tcp-streams-openssl](hackage.haskell.org/package/tcp-streams-openssl)

## 0.5.0.0

* Update built-in mozilla CA list(2016/09/14).
* Export `Data.TLSSetting` from `System.IO.Streams.TLS`, and `Data.OpenSSLSetting` from `System.IO.Streams.OpenSSL`.

## 0.4.0.0

* Auto add `withOpenSSL`.
* Add `extra-libraries` to cabal file.

## 0.3.0.0

* Add qualified notes, rename `closeTLS/closeSSL` to `close`.
* Fix `Bad pipe` error in tls's `close`

## 0.2.3.0

* Add `acceptWithBufferSize`, `socketToStreamsWithBufferSize`, fix recv exception handler.

## 0.2.2.0

* Clean and document exception behavior.

## 0.2.1.0

* Fix broken document.

## 0.2.0.0

* Add support for openssl using HsOpenSSL package.

## 0.1.0.0

* First version. Released on an unsuspecting world.
