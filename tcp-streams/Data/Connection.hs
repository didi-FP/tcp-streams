module Data.Connection where

import qualified Data.ByteString           as B
import qualified Data.ByteString.Lazy.Internal as L
import qualified Network.Socket            as N
import qualified System.IO.Streams         as S

-- | A simple connection abstraction.
--
-- 'Connection' s from this package are supposed to have following properties:
--
--  * 'InputStream' is used to simplified streaming processing.
--
--  * 'send' will use <http://hackage.haskell.org/package/network-2.6.2.1/docs/Network-Socket-ByteString.html#g:2
--   vector-IO> automatically when there's more than one chunk to save system call.
--
--  * You should make sure there's no pending recv/send before you 'close' a 'Connection'.
--  That means either you call 'close' in the same thread you recv/send, or use async exception
--  to terminate recv/send thread before call 'close' in other thread(such as a reaper thread).
--  Otherwise you may run into <https://mail.haskell.org/pipermail/haskell-cafe/2014-September/115823.html
--  race-connections>.
--
--  * Exception or closed by other peer during recv/send will NOT close underline socket,
--  you should always use 'close' with 'E.bracket' to ensure safety.
--
--  @since 1.0
--
data Connection = Connection
    { source  :: S.InputStream B.ByteString
    , send    :: L.ByteString -> IO ()
    , close   :: IO ()
    , address :: N.SockAddr
    }
