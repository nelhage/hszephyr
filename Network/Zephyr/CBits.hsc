{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
{-# INCLUDE "zephyr/zephyr.h" #-}

module Network.Zephyr.CBits where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import qualified Data.ByteString.Char8 as B

import qualified Data.Time as Time

#include <zephyr/zephyr.h>

newtype Code_t = Code_t {unCode_t :: CInt}
    deriving (Eq, Show)

#{enum Code_t, Code_t
 , zerr_none            = ZERR_NONE
 , zauth_no             = ZAUTH_NO
 , zauth_yes            = ZAUTH_YES
 , zauth_failed         = ZAUTH_FAILED
  }

newtype ZNoticeKind = ZNoticeKind { unZNoticeKind :: CInt }
    deriving (Show, Eq, Storable);

#{enum ZNoticeKind, ZNoticeKind
 , kind_unsafe    = UNSAFE
 , kind_unacked   = UNACKED
 , kind_acked     = ACKED
 , kind_hmack     = HMACK
 , kind_hmctl     = HMCTL
 , kind_servack   = SERVACK
 , kind_servnak   = SERVNAK
 , kind_clientack = CLIENTACK
 , kind_stat      = STAT
 }

data ZAuth = Authenticated | Unauthenticated | AuthenticationFailed
           deriving (Show, Eq, Enum, Bounded)

data ZSubscription = ZSubscription { sub_class     :: B.ByteString
                                   , sub_instance  :: B.ByteString
                                   , sub_recipient :: B.ByteString
                                   }

withZSubscription :: ZSubscription -> (Ptr ZSubscription -> IO a) -> IO a
withZSubscription sub code = do
  B.useAsCString (sub_class sub)        $  \c_class -> do
  B.useAsCString (sub_instance sub)     $  \c_inst  -> do
  B.useAsCString (sub_recipient sub)    $  \c_recip -> do
   allocaBytes (#{size ZSubscription_t}) $ \c_sub   -> do
        #{poke ZSubscription_t, zsub_recipient} c_sub c_recip
        #{poke ZSubscription_t, zsub_class}     c_sub c_class
        #{poke ZSubscription_t, zsub_classinst} c_sub c_inst
        code c_sub

subSize :: Int
subSize = #{size ZSubscription_t}

withSubs :: [ZSubscription] -> ((Ptr ZSubscription, CInt) -> IO a) -> IO a
withSubs subs code = let n_subs = length subs in
  withMany withZSubscription subs $ \c_subs  -> do
  allocaBytes (n_subs * subSize)  $ \c_array -> do
    copySubs c_array c_subs
    code (c_array, fromIntegral n_subs)
      where copySubs to (s:ss) = do copyBytes to s subSize
                                    copySubs (to `plusPtr` subSize) ss
            copySubs _ []      = return ()

data ZNotice = ZNotice { z_version     :: B.ByteString
                       , z_class       :: B.ByteString
                       , z_instance    :: B.ByteString
                       , z_recipient   :: B.ByteString
                       , z_opcode      :: B.ByteString
                       -- z_sender will always be a Just on received
                       -- notices. Set z_sender = Nothing to
                       -- automatically populate it when sending.
                       , z_sender      :: Maybe B.ByteString
                       , z_default_fmt :: B.ByteString
                       , z_kind        :: ZNoticeKind
                       , z_auth        :: ZAuth
                       , z_authent     :: B.ByteString
                       , z_fields      :: [B.ByteString]
                       , z_time        :: Time.LocalTime
                        }

withZNotice_t :: ZNotice -> (Ptr ZNotice -> IO a) -> IO a
withZNotice_t note comp = do
  allocaBytes (#{size ZNotice_t})     $ \c_note      -> do
    B.useAsCString (z_class note)     $ \c_class     -> do
    B.useAsCString (z_instance note)  $ \c_instance  -> do
    B.useAsCString (z_recipient note) $ \c_recipient -> do
    B.useAsCString (z_opcode note)    $ \c_opcode    -> do
    B.useAsCString (z_default_fmt note) $ \c_fmt     -> do
    B.useAsCStringLen message         $ \(c_message, c_msg_len) -> do
    maybeWith B.useAsCString (z_sender note)  $ \c_sender       -> do
        memset c_note 0 (#{size ZNotice_t})
        #{poke ZNotice_t, z_kind}        c_note (z_kind note)
        #{poke ZNotice_t, z_port}        c_note (0::CInt)
        #{poke ZNotice_t, z_class}       c_note c_class
        #{poke ZNotice_t, z_class_inst}  c_note c_instance
        #{poke ZNotice_t, z_opcode}      c_note c_opcode
        #{poke ZNotice_t, z_default_format} c_note c_fmt
        #{poke ZNotice_t, z_message}     c_note c_message
        #{poke ZNotice_t, z_message_len} c_note c_msg_len
        #{poke ZNotice_t, z_sender}      c_note c_sender
        comp c_note
      where message :: B.ByteString
            message = B.append (B.intercalate (B.pack "\0") $ z_fields note) (B.pack "\0")


newtype SockAddr = SockAddr { unSockAddr :: SockAddr }

type Port = CUShort

foreign import ccall unsafe "error_message"
        c_error_message :: Code_t -> IO CString

foreign import ccall unsafe "ZInitialize"
        z_initialize :: IO Code_t

foreign import ccall unsafe "ZOpenPort"
        z_open_port :: Ptr Port -> IO Code_t

foreign import ccall unsafe "ZClosePort"
        z_close_port :: IO Code_t

foreign import ccall unsafe "ZGetSender"
        z_get_sender :: IO CString

type ZAuthProc = FunPtr (Ptr ZNotice -> CString -> CInt -> IO (Ptr CInt))

foreign import ccall unsafe "ZSendNotice"
        z_send_notice :: Ptr ZNotice -> ZAuthProc -> IO Code_t

foreign import ccall unsafe "&ZMakeAuthentication"
        z_make_authentication :: ZAuthProc

foreign import ccall unsafe "ZCancelSubscriptions"
        z_cancel_subscriptions :: Port -> IO Code_t

foreign import ccall unsafe "ZSubscribeTo"
        z_subscribe_to :: Ptr ZSubscription -> CInt -> Port -> IO Code_t

foreign import ccall unsafe "ZUnsubscribeTo"
        z_unsubscribe_to :: Ptr ZSubscription -> CInt -> Port -> IO Code_t

foreign import ccall unsafe "ZReceiveNotice"
        z_receive_notice :: Ptr ZNotice -> Ptr SockAddr -> IO Code_t

foreign import ccall unsafe "ZCheckAuthentication"
        z_check_authentication :: Ptr ZNotice -> Ptr SockAddr -> IO Code_t

foreign import ccall unsafe "string.h"
    memset  :: Ptr a -> CInt -> CSize -> IO ()
