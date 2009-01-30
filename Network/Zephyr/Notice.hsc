{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
{-# INCLUDE "zephyr/zephyr.h" #-}

module Network.Zephyr.Notice ( ZNotice(..)
                             , emptyNotice
                             , withZNotice_t
                             , ZNoticeKind(..)
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

import qualified Data.Time as Time

#include <zephyr/zephyr.h>

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

-- instance Storable ZNoticeKind where
--    sizeOf = sizeOf (undefined::CInt)
--    alignment = alignment (undefined::Cint)


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
                       , z_auth        :: Int
                       , z_authent     :: B.ByteString
                       , z_fields      :: [B.ByteString]
                       , z_time        :: Time.LocalTime
                        }

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
                      , z_auth        = 0
                      , z_authent     = undefined
                      , z_fields      = []
                      , z_time        = undefined
                      }

foreign import ccall unsafe "string.h"
    memset  :: Ptr a -> CInt -> CSize -> IO ()

withZNotice_t :: ZNotice -> (Ptr ZNotice -> IO a) -> IO a
withZNotice_t note comp = do
  allocaBytes (#{size ZNotice_t})     $ \c_note      -> do
    B.useAsCString (z_class note)     $ \c_class     -> do
    B.useAsCString (z_instance note)  $ \c_instance  -> do
    B.useAsCString (z_recipient note) $ \c_recipient -> do
    B.useAsCString (z_opcode note)    $ \c_opcode    -> do
    B.useAsCString (z_default_fmt note) $ \c_fmt     -> do
    B.useAsCStringLen message         $ \(c_message, c_msg_len) -> do
        memset c_note 0 (#{size ZNotice_t})
        #{poke ZNotice_t, z_kind}        c_note (z_kind note)
        #{poke ZNotice_t, z_port}        c_note (0::CInt)
        #{poke ZNotice_t, z_class}       c_note c_class
        #{poke ZNotice_t, z_class_inst}  c_note c_instance
        #{poke ZNotice_t, z_opcode}      c_note c_opcode
        #{poke ZNotice_t, z_default_format} c_note c_fmt
        #{poke ZNotice_t, z_message}     c_note c_message
        #{poke ZNotice_t, z_message_len} c_note c_msg_len
        case (z_sender note) of
          Nothing     -> comp c_note
          Just sender -> B.useAsCString sender $ \c_sender -> do
                             #{poke ZNotice_t, z_sender} c_note c_sender
                             comp c_note
      where message :: B.ByteString
            message = B.append (B.intercalate (B.pack "\0") $ z_fields note) (B.pack "\0")
