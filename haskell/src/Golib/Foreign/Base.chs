{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Golib.Foreign.Base
(
 checkNullPtr) where

import C2HS
import Foreign.C.Types
import Foreign.Ptr


#include "cpp/matrix.h"

{# context lib="golib" #}

checkNullPtr :: Ptr a -> IO Bool
checkNullPtr p = goCheckNullPtr (castPtr p) >>= \i ->
  case i of
    1 -> return True
    _ -> return False

foreign import ccall "matrix.h golib_check_null_ptr"
  goCheckNullPtr :: Ptr () -> IO CInt

