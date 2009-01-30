{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# INCLUDE "zephyr/zephyr.h" #-}

module Network.Zephyr where

import Foreign
import Foreign.C.Types
import Foreign.C.String

import System.IO.Unsafe

import qualified Data.ByteString as B

import Control.Monad
import Control.Monad.Trans

import Network.Zephyr.Notice

#include <zephyr/zephyr.h>
#include <com_err.h>             

newtype Code_t = Code_t {unCode_t :: CInt}
    deriving (Eq, Show)

#{enum Code_t, Code_t
  , zerr_none            = ZERR_NONE
  }

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

type Port = CUShort

defaultPort :: Port
defaultPort = 0

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

error_message :: Code_t -> String
error_message c = unsafePerformIO $ c_error_message c >>= peekCString

comErr :: (MonadIO m) => Code_t -> m ()
comErr c | c == zerr_none = return ()
         | otherwise      = liftIO $ do fail $ error_message c

initialize :: IO ()
initialize = z_initialize >>= comErr

getSender :: IO String
getSender = z_get_sender >>= peekCString

openPort :: IO Int
openPort = alloca $ \ptr -> do
             poke ptr 0
             z_open_port ptr >>= comErr
             fromIntegral `liftM` peek ptr

sendNotice :: ZNotice -> IO ()
sendNotice note = withZNotice_t note $ \c_note -> do
                    z_send_notice c_note cert >>= comErr
    where cert = if z_auth note
                 then z_make_authentication
                 else nullFunPtr

cancelSubscriptions :: IO ()
cancelSubscriptions = z_cancel_subscriptions defaultPort >>= comErr

subscribeTo :: [ZSubscription] -> IO ()
subscribeTo subs = withSubs subs $ \(c_subs, c_len) -> do
  z_subscribe_to c_subs c_len defaultPort >>= comErr

unsubscribeTo :: [ZSubscription] -> IO ()
unsubscribeTo subs = withSubs subs $ \(c_subs, c_len) -> do
  z_unsubscribe_to c_subs c_len defaultPort >>= comErr
