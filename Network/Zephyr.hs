-- | Simple bindings to libzephyr.
-- 
--   All functions in this module properly serialize access to the C
--   libzephyr and behave correctly with regard to 'forkIO', so this
--   module should behave properly in threaded Haskell program.
--
--   At present, however, we only support maintaining a single,
--   global, set of Zephyr subscriptions. This may be extended to
--   support multiple clients within the same Haskell program.
module Network.Zephyr ( initialize, getSender, getRealm
                      , sendNotice, receiveNotice, pendingNotices, tryReceiveNotice
                      , cancelSubscriptions, subscribeTo, unsubscribeTo
                      , defaultFmt, emptyNotice
                      , ZNotice(..), ZNoticeKind(..), ZAuth(..)
                      , ZSubscription(..)
                      , kind_unsafe, kind_unacked, kind_acked
                      , kind_hmack, kind_hmctl, kind_servack
                      , kind_servnak, kind_clientack, kind_stat
                      ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import System.IO.Unsafe

import qualified Data.ByteString.Char8 as B

import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Control.Concurrent

import Network.Zephyr.CBits

-- | libzephyr is completely non-threadsafe, so we serialize all
--   accesses to libzephyr using a global MVar.
zephyrMVar :: MVar ()
zephyrMVar = unsafePerformIO $ newMVar ()

-- | A simple combinator to access the above MVar
withZephyr :: IO a -> IO a
withZephyr io = withMVar zephyrMVar $ \ _ -> io

defaultPort :: Port
defaultPort = 0

-- | fail with the appropriate error message unless the provided
--   Code_t is 0 (no error), using @com_err@
comErr :: (Monad m) => Code_t -> m ()
comErr c | c == zerr_none = return ()
         | otherwise      = fail $ error_message c

-- | Initialize libzephyr.
initialize :: IO ()
initialize = do z_initialize >>= comErr
                withZephyr $ alloca $ \ptr -> do
                  poke ptr 0
                  z_open_port ptr >>= comErr

-- | Return the name of the current Zephyr sender.
getSender :: IO String
getSender = withZephyr $ z_get_sender >>= peekCString

-- | Return the realm of the current host.
getRealm  :: IO String
getRealm  = withZephyr $ peekCString z_realm

-- | Send a 'ZNotice'.
sendNotice :: ZNotice -> IO ()
sendNotice note = withZNotice note $ \c_note -> do
                    withZephyr $ z_send_notice c_note cert >>= comErr
    where cert = case z_auth note of
                   Unauthenticated -> z_make_authentication
                   _               -> nullFunPtr

-- | Receive a 'ZNotice' from the zephyr servers. Blocks until a
--   notice is available.
receiveNotice :: IO ZNotice
receiveNotice = do fd <- peek z_fd
                   loop fd
    where loop fd = do note <- tryReceiveNotice
                       case note of
                         Just  n -> return n
                         Nothing -> loop fd

receiveNotice' :: IO ZNotice
receiveNotice' = allocaZNotice $ \c_note -> do
                  z_receive_notice c_note nullPtr >>= comErr
                  finally (parseZNotice  c_note) (z_free_notice c_note)

pendingNotices' :: IO Int
pendingNotices' = fromIntegral `liftM` z_pending

-- | Checks for new incoming packets and then returns the number of
--   pending messages in the queue.
pendingNotices  :: IO Int
pendingNotices = withZephyr pendingNotices

-- | Try to receive a ZNotice, returning 'Nothing' if no notice is
--   available.
tryReceiveNotice :: IO (Maybe ZNotice)
tryReceiveNotice = withZephyr $ do
                     p <- pendingNotices'
                     if (p > 0)
                      then Just `liftM` receiveNotice'
                      else return Nothing

-- | Cancel all zephyr subscriptions.
cancelSubscriptions :: IO ()
cancelSubscriptions = withZephyr $ z_cancel_subscriptions defaultPort >>= comErr

-- | Subscribe to one or more Zephyr triples.
subscribeTo :: [ZSubscription] -> IO ()
subscribeTo subs = withSubs subs $ \(c_subs, c_len) -> do
  withZephyr $ z_subscribe_to c_subs c_len defaultPort >>= comErr

-- | Unsubscribe from one or more Zephyr triples.
unsubscribeTo :: [ZSubscription] -> IO ()
unsubscribeTo subs = withSubs subs $ \(c_subs, c_len) -> do
  withZephyr $ z_unsubscribe_to c_subs c_len defaultPort >>= comErr

-- | Holds the default display format used by outgoing Zephyrs by
--   @zwrite@.
defaultFmt :: B.ByteString
defaultFmt = B.pack "Class $class, Instance $instance:\nTo: @bold($recipient) at $time $date\nFrom: @bold{$1 <$sender>}\n\n$2"

-- | A default 'ZNotice' suitable for use as a template when creating
--   a new notice for sending via 'sendNotice'.
emptyNotice :: ZNotice
emptyNotice = ZNotice { z_version     = undefined
                      , z_class       = undefined
                      , z_instance    = undefined
                      , z_recipient   = undefined
                      , z_opcode      = B.pack ""
                      , z_sender      = Nothing
                      , z_default_fmt = defaultFmt
                      , z_kind        = undefined
                      , z_auth        = Unauthenticated
                      , z_fields      = []
                      , z_time        = undefined
                      }

