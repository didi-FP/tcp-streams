tcp-streams: Easy TCP with tls and streamming
=============================================

This package is based on `openssl-streams` package with following changes:

+ provide raw tcp / native `tls` / openssl bindings `HsOpenSSL`.

+ auto enable `TCP_NODELAY`.

+ add health check(WIP)

`io-streams` provided automate read buffering and easy streamming process, happy hacking!
