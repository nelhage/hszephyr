{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Network.Zephyr where

import Foreign
import Foreign.C.Types

#include <zephyr/zephyr.h>

type Code_t = CInt

foreign import ccall unsafe "zephyr/zephyr.h ZInitialize"
        z_initialize :: IO Code_t
