{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Golib
(GoMatrix,
 matrixNew) where

import C2HS
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable


#include "test.h"

{# context lib="golib" #}

-- getNumber = {#call unsafe getNumber as _getNumber#}
-- {# fun unsafe test as test {} -> `Int' peekIntConv*  #}
{# fun unsafe test as test {} -> `Int' #}

{# pointer *golib_matrix as GoMatrix foreign newtype #}
{# fun unsafe golib_matrix_new as matrixNew {} -> `ForeignPtr GoMatrix' goMatrixOutMarshal #}

goMatrixOutMarshal = newForeignPtr goMatrixFinalize

foreign import ccall "test.h &golib_matrix_destroy"
	goMatrixFinalize :: FunPtr (Ptr GoMatrix -> IO ())



-- {# fun unsafe golib_matrix_new as matrixNew {} -> `GoMatrix' id #}

{- main = do
     n <- test
     putStrLn $ "test () yields " ++ show n -}