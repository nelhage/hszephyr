module Network.Zephyr ( initialize, openPort, getSender, getRealm
                      , sendNotice, receiveNotice
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

import Network.Zephyr.CBits

defaultPort :: Port
defaultPort = 0

comErr :: (Monad m) => Code_t -> m ()
comErr c | c == zerr_none = return ()
         | otherwise      = fail $ error_message c

initialize :: IO ()
initialize = z_initialize >>= comErr

getSender :: IO String
getSender = z_get_sender >>= peekCString

getRealm  :: IO String
getRealm  = peekCString z_realm

openPort :: IO Int
openPort = alloca $ \ptr -> do
             poke ptr 0
             z_open_port ptr >>= comErr
             fromIntegral `liftM` peek ptr

sendNotice :: ZNotice -> IO ()
sendNotice note = withZNotice note $ \c_note -> do
                    z_send_notice c_note cert >>= comErr
    where cert = case z_auth note of
                   Unauthenticated -> z_make_authentication
                   _               -> nullFunPtr

receiveNotice :: IO ZNotice
receiveNotice = allocaZNotice $ \c_note -> do
                  z_receive_notice c_note nullPtr >>= comErr
                  finally (parseZNotice  c_note) (z_free_notice c_note)

pendingNotices :: IO Int
pendingNotices = fromIntegral `liftM` z_pending

cancelSubscriptions :: IO ()
cancelSubscriptions = z_cancel_subscriptions defaultPort >>= comErr

subscribeTo :: [ZSubscription] -> IO ()
subscribeTo subs = withSubs subs $ \(c_subs, c_len) -> do
  z_subscribe_to c_subs c_len defaultPort >>= comErr

unsubscribeTo :: [ZSubscription] -> IO ()
unsubscribeTo subs = withSubs subs $ \(c_subs, c_len) -> do
  z_unsubscribe_to c_subs c_len defaultPort >>= comErr

defaultFmt :: B.ByteString
defaultFmt = B.pack "Class $class, Instance $instance:\nTo: @bold($recipient) at $time $date\nFrom: @bold{$1 <$sender>}\n\n$2"

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
                      , z_authent     = undefined
                      , z_fields      = []
                      , z_time        = undefined
                      }

