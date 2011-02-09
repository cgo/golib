{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Golib.Foreign.Math.Matrix
(GoMatrix,
 goMatrixRowCount,
 goMatrixColCount,
 goMatrixGetElem,
 matrixNew) where

import C2HS
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable


#include "matrix.h"

{# context lib="golib" #}

-- getNumber = {#call unsafe getNumber as _getNumber#}
-- {# fun unsafe test as test {} -> `Int' peekIntConv*  #}
{# fun unsafe test as test {} -> `Int' #}

{# pointer *golib_matrix as GoMatrix newtype #}
-- {# fun unsafe golib_matrix_new as matrixNew' {} -> `Ptr GoMatrix' id #}

matrixNew = goMatrixNew >>= newForeignPtr goMatrixFinalize

foreign import ccall "matrix.h &golib_matrix_destroy"
	goMatrixFinalize :: FunPtr (Ptr GoMatrix -> IO ())

foreign import ccall "matrix.h golib_matrix_new"
	goMatrixNew :: IO (Ptr GoMatrix)

foreign import ccall "matrix.h golib_matrix_row_count"
	goMatrixRowCount :: Ptr GoMatrix -> IO CSize
foreign import ccall "matrix.h golib_matrix_col_count"
	goMatrixColCount :: Ptr GoMatrix -> IO CSize

foreign import ccall "matrix.h golib_matrix_get_elem"
	goMatrixGetElem :: Ptr GoMatrix -> CSize -> CSize -> IO CDouble



-- {# fun unsafe golib_matrix_new as matrixNew {} -> `GoMatrix' id #}

{- main = do
     n <- test
     putStrLn $ "test () yields " ++ show n -}