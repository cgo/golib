{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Golib.Foreign.Math.Matrix
(GoMatrix,
 goMatrixRowCount,
 goMatrixColCount,
 goMatrixGetElem,
 goMatrixMatrixMult,
 matrixNew,
 withGoMatrix) where

import C2HS
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable


#include "cpp/matrix.h"

{# context lib="golib" #}

-- getNumber = {#call unsafe getNumber as _getNumber#}
-- {# fun unsafe test as test {} -> `Int' peekIntConv*  #}
{# fun unsafe test as test {} -> `Int' #}

{# pointer *golib_matrix as GoMatrix foreign newtype #}
-- {# fun unsafe golib_matrix_new as matrixNew' {} -> `Ptr GoMatrix' id #}

matrixNew r c = goMatrixNew (fromIntegral r) (fromIntegral c) >>= \mp ->
  checkNullPtr mp >>= \n ->
  if n
  then error ("Matrix: allocation error when allocating " ++ show r ++ "," ++
              show c ++ " matrix!")
  else return () >>
  newForeignPtr goMatrixFinalize mp >>= return . GoMatrix
-- These are generated automatically by the above c2hs line.
--withGoMatrix :: GoMatrix -> (Ptr GoMatrix -> IO a) -> IO a
--withGoMatrix (GoMatrix m) = withForeignPtr m

checkNullPtr :: Ptr a -> IO Bool
checkNullPtr p = goCheckNullPtr (castPtr p) >>= \i ->
  case i of
    1 -> return True
    _ -> return False


foreign import ccall "matrix.h golib_check_null_ptr"
	goCheckNullPtr :: Ptr () -> IO CInt

foreign import ccall "matrix.h &golib_matrix_destroy"
	goMatrixFinalize :: FunPtr (Ptr GoMatrix -> IO ())

foreign import ccall "matrix.h golib_matrix_new"
	goMatrixNew :: CSize -> CSize -> IO (Ptr GoMatrix)

foreign import ccall "matrix.h golib_matrix_row_count"
	goMatrixRowCount :: Ptr GoMatrix -> IO CSize
foreign import ccall "matrix.h golib_matrix_col_count"
	goMatrixColCount :: Ptr GoMatrix -> IO CSize

foreign import ccall "matrix.h golib_matrix_get_elem"
	goMatrixGetElem :: Ptr GoMatrix -> CSize -> CSize -> IO CDouble

foreign import ccall "matrix.h golib_matrix_matrix_mult"
	goMatrixMatrixMult :: CDouble -> Ptr GoMatrix -> CInt -> Ptr GoMatrix -> CInt -> CDouble -> Ptr GoMatrix -> IO ()


-- {# fun unsafe golib_matrix_new as matrixNew {} -> `GoMatrix' id #}

{- main = do
     n <- test
     putStrLn $ "test () yields " ++ show n -}