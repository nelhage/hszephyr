{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Network.Zephyr.CBits where

import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import Foreign.C.String

import System.Posix.Types (Fd(Fd))
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as B

import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as POSIXTime

#include <zephyr/zephyr.h>

-- | Wrapper for the Zephyr @Code_t@ type
newtype Code_t = Code_t {unCode_t :: CInt}
    deriving (Eq, Show)

#{enum Code_t, Code_t
 , zerr_none            = ZERR_NONE
 , zauth_no             = ZAUTH_NO
 , zauth_yes            = ZAUTH_YES
 , zauth_failed         = ZAUTH_FAILED
  }

-- | Translate a 'Code_t' into a human-readable error using @com_err@.
error_message :: Code_t -> String
error_message c = unsafePerformIO $ c_error_message c >>= peekCString

-- | 'ZNoticeKind' represent the kinds of 'ZNotice's sent or received
--   by the Zephyr system.
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

-- | 'ZAuth' represents the authentication used when sending or
--   receiving a Zephyr.
data ZAuth = Authenticated    -- ^ The message was received with
                              -- correct authentication, or should be
                              -- authenticated for outgoing notices.
           | Unauthenticated  -- ^ The message was or will be sent
                              -- with no authentication.
           | AuthenticationFailed -- ^ The message was received with
                                  -- invalid authentication.
           deriving (Show, Eq, Enum, Bounded)

-- | Represents a Zephyr triple for the purposes of subscribing or
--   unsubscribing to zephyrs.
data ZSubscription = ZSubscription { sub_class     :: B.ByteString
                                   , sub_instance  :: B.ByteString
                                   , sub_recipient :: B.ByteString
                                   }

-- | Helper combinator to marshal a 'ZSubscription'
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

-- | Helper combinator to marshal a list of 'ZSubscription's
withSubs :: [ZSubscription] -> ((Ptr ZSubscription, CInt) -> IO a) -> IO a
withSubs subs code = let n_subs = length subs in
  withMany withZSubscription subs $ \c_subs  -> do
  allocaBytes (n_subs * subSize)  $ \c_array -> do
    copySubs c_array c_subs
    code (c_array, fromIntegral n_subs)
      where copySubs to (s:ss) = do copyBytes to s subSize
                                    copySubs (to `plusPtr` subSize) ss
            copySubs _ []      = return ()

{- | 'ZNotice' represents a Zephyr notice. All fields of this record
      are filled-in for received notices. For outoing notices, only
      the following fields are relevant:

       * @z_class@

       * @z_instance@

       * @z_opcode@

       * @z_sender@

       * @z_default_fmt@

       * @z_kind@

       * @z_auth@

       * @z_fields@
-}
data ZNotice = ZNotice { z_version     :: B.ByteString
                       -- ^ The Zephyr version this notice was sent with.
                       , z_class       :: B.ByteString
                       -- ^ The Zephyr class of this notice.
                       , z_instance    :: B.ByteString
                       -- ^ The Zephyr instance of this notice.
                       , z_recipient   :: B.ByteString
                       -- ^ The recipient of this notice.
                       , z_opcode      :: B.ByteString
                       -- ^ The opcode of this notice.
                       , z_sender      :: Maybe B.ByteString
                       -- ^ The sender of this Notice.
                       -- This field is always a 'Just' for received
                       -- notices. Setting it to 'Nothing' for sent
                       -- notices will cause it to automatically be
                       -- filled in.
                       , z_default_fmt :: B.ByteString
                       -- ^ The default format clients should use to
                       --   render this notice.
                       , z_kind        :: ZNoticeKind
                       -- ^ The kind of this notice (determines how it
                       --   will be ACK'd).
                       , z_auth        :: ZAuth
                       -- ^ Whether this notice is authenticated.
                       , z_fields      :: [B.ByteString]
                       -- ^ A list of the fields in this notice.
                       , z_time        :: Time.UTCTime
                       -- ^ The time this notice was sent.
                        }

-- | A helper to allocate memory for a single C @ZNotice_t@.
allocaZNotice :: (Ptr ZNotice -> IO a) -> IO a
allocaZNotice comp = allocaBytes (#{size ZNotice_t}) $ \c_note -> do
                       memset c_note 0 (#{size ZNotice_t})
                       comp c_note

-- | A helper combinator to marshal a 'ZNotice' to a @ZNotice_t@.
withZNotice :: ZNotice -> (Ptr ZNotice -> IO a) -> IO a
withZNotice note comp = do
  allocaZNotice                       $ \c_note      -> do
    B.useAsCString (z_class note)     $ \c_class     -> do
    B.useAsCString (z_instance note)  $ \c_instance  -> do
    B.useAsCString (z_recipient note) $ \c_recipient -> do
    B.useAsCString (z_opcode note)    $ \c_opcode    -> do
    B.useAsCString (z_default_fmt note) $ \c_fmt     -> do
    B.useAsCStringLen message         $ \(c_message, c_msg_len) -> do
    maybeWith B.useAsCString (z_sender note)  $ \c_sender       -> do
        #{poke ZNotice_t, z_kind}        c_note (z_kind note)
        #{poke ZNotice_t, z_port}        c_note (0::CInt)
        #{poke ZNotice_t, z_class}       c_note c_class
        #{poke ZNotice_t, z_class_inst}  c_note c_instance
        #{poke ZNotice_t, z_recipient}   c_note c_recipient
        #{poke ZNotice_t, z_opcode}      c_note c_opcode
        #{poke ZNotice_t, z_default_format} c_note c_fmt
        #{poke ZNotice_t, z_message}     c_note c_message
        #{poke ZNotice_t, z_message_len} c_note c_msg_len
        #{poke ZNotice_t, z_sender}      c_note c_sender
        comp c_note
      where message :: B.ByteString
            message = B.concat $ map (`B.append` B.singleton 0) $ z_fields note

-- | Parse a @ZNotice_t@ into a 'ZNotice' record.
parseZNotice :: Ptr ZNotice -> IO ZNotice
parseZNotice c_note = do
  version <- #{peek ZNotice_t, z_version}         c_note >>= B.packCString
  cls     <- #{peek ZNotice_t, z_class}           c_note >>= B.packCString
  inst    <- #{peek ZNotice_t, z_class_inst}      c_note >>= B.packCString
  recip   <- #{peek ZNotice_t, z_recipient}       c_note >>= B.packCString
  opcode  <- #{peek ZNotice_t, z_opcode}          c_note >>= B.packCString
  sender  <- #{peek ZNotice_t, z_sender}          c_note >>= B.packCString
  fmt     <- #{peek ZNotice_t, z_default_format}  c_note >>= B.packCString
  kind    <- #{peek ZNotice_t, z_kind}            c_note
  secs    <- #{peek ZNotice_t, z_time.tv_sec}     c_note
  time    <- return $ POSIXTime.posixSecondsToUTCTime (realToFrac (secs :: CTime))
  c_len'  <- #{peek ZNotice_t, z_message_len}     c_note
  let c_len = fromIntegral (c_len' :: CInt)
  c_msg   <- #{peek ZNotice_t, z_message}         c_note
  message <- B.packCStringLen (c_msg, c_len)
  fields  <- return $ filterFields $ B.split 0 message
  c_auth  <- z_check_authentication c_note $ #{ptr ZNotice_t, z_uid.zuid_addr} c_note
  auth    <- case c_auth of
               _ | c_auth == zauth_no  -> return Unauthenticated
                 | c_auth == zauth_yes -> return Authenticated
                 | c_auth == zauth_failed -> return AuthenticationFailed
                 | otherwise           -> fail $ error_message c_auth
  return $ ZNotice { z_version     = version
                   , z_class       = cls
                   , z_instance    = inst
                   , z_recipient   = recip
                   , z_opcode      = opcode
                   , z_sender      = Just sender
                   , z_default_fmt = fmt
                   , z_kind        = kind
                   , z_auth        = auth
                   , z_fields      = fields
                   , z_time        = time
                   }
    where filterFields = id
          -- filterFields fields = if (B.null $ last fields)
          --                       then init fields
          --                      else fields


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

foreign import ccall unsafe "&__Zephyr_realm"
        z_realm      :: CString

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

foreign import ccall unsafe "ZFreeNotice"
        z_free_notice   :: Ptr ZNotice -> IO ()

foreign import ccall unsafe "ZPending"
        z_pending       :: IO CInt

foreign import ccall unsafe "&__Zephyr_fd"
        z_fd            :: Ptr Fd

foreign import ccall unsafe "string.h"
    memset  :: Ptr a -> CInt -> CSize -> IO ()

