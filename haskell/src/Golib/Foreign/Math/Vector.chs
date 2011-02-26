{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Golib.Foreign.Math.Vector
( Vector, 
  vectorNew) where

import Golib.Foreign.Base
import Golib.Math.Base
import C2HS
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable


#include "cpp/vector.h"

{# context lib="golib" #}

{# pointer *golib_vector as Vector foreign newtype #}
-- {# fun unsafe golib_matrix_new as matrixNew' {} -> `Ptr Matrix' id #}

{# fun unsafe golib_vector_new {`Int'} -> `Ptr Vector' id #}
{# fun pure golib_vector_size as vectorSize {withVector* `Vector'} -> `Int' fromIntegral #}
{# fun unsafe golib_vector_get_elem as getElem {withVector* `Vector', fromIntegral `Int'} -> `Double' realToFrac #}
{# fun unsafe golib_vector_set_elem as unsafeSetElem {withVector* `Vector', fromIntegral `Int', realToFrac `Double'} -> `Bool' cToBool #}
{# fun unsafe golib_vector_fill as unsafeFill {withVector* `Vector', realToFrac `Double'} -> `()' #}
{# fun unsafe golib_vector_copy as unsafeCopy {withVector* `Vector', withVector* `Vector'} -> `()' #}
{# fun pure golib_vector_equals as equals {withVector* `Vector', withVector* `Vector'} -> `Bool' cToBool #}


foreign import ccall "vector.h &golib_vector_destroy"
  golib_vector_finalizer :: FunPtr (Ptr Vector -> IO ())

vectorNew :: Index -> IO Vector
vectorNew n = golib_vector_new (fromIntegral n) >>= \vp ->
  checkNullPtr vp >>= \nn ->
  if nn
  then error ("Vector: allocation error when allocating " ++ show n ++ "-vector!")
  else return () >>
  newForeignPtr golib_vector_finalizer vp >>= return . Vector

-- foreign import ccall "vector.h 