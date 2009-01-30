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

foreign import ccall unsafe "error_message"
        c_error_message :: Code_t -> IO CString

foreign import ccall unsafe "ZInitialize"
        z_initialize :: IO Code_t

foreign import ccall unsafe "ZOpenPort"
        z_open_port :: Ptr CUShort -> IO Code_t

foreign import ccall unsafe "ZClosePort"
        z_close_port :: IO Code_t

foreign import ccall unsafe "ZGetSender"
        z_get_sender :: IO CString

type ZAuthProc = FunPtr (Ptr ZNotice -> CString -> CInt -> IO (Ptr CInt))

foreign import ccall unsafe "ZSendNotice"
        z_send_notice :: Ptr ZNotice -> ZAuthProc -> IO Code_t

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
                    z_send_notice c_note nullFunPtr >>= comErr
