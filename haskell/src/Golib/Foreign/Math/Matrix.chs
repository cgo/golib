{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Golib.Foreign.Math.Matrix
(Matrix,
 goMatrixRowCount,
 goMatrixColCount,
 goMatrixGetElem,
 goMatrixSetElem,
 goMatrixFill,
 goMatrixTranspose,
 goMatrixMatrixMult,
 goMatrixCopy,
 goMatrixEquals,
 matrixNew,
 withMatrix) where

import C2HS
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable


#include "cpp/matrix.h"

{# context lib="golib" #}

-- getNumber = {#call unsafe getNumber as _getNumber#}
-- {# fun unsafe test as test {} -> `Int' peekIntConv*  #}
{# fun unsafe test as test {} -> `Int' #}

{# pointer *golib_matrix as Matrix foreign newtype #}
-- {# fun unsafe golib_matrix_new as matrixNew' {} -> `Ptr Matrix' id #}

matrixNew r c = goMatrixNew (fromIntegral r) (fromIntegral c) >>= \mp ->
  checkNullPtr mp >>= \n ->
  if n
  then error ("Matrix: allocation error when allocating " ++ show r ++ "," ++
              show c ++ " matrix!")
  else return () >>
  newForeignPtr goMatrixFinalize mp >>= return . Matrix
-- These are generated automatically by the above c2hs line.
--withMatrix :: Matrix -> (Ptr Matrix -> IO a) -> IO a
--withMatrix (Matrix m) = withForeignPtr m

checkNullPtr :: Ptr a -> IO Bool
checkNullPtr p = goCheckNullPtr (castPtr p) >>= \i ->
  case i of
    1 -> return True
    _ -> return False


foreign import ccall "matrix.h golib_check_null_ptr"
  goCheckNullPtr :: Ptr () -> IO CInt

foreign import ccall "matrix.h &golib_matrix_destroy"
  goMatrixFinalize :: FunPtr (Ptr Matrix -> IO ())

foreign import ccall "matrix.h golib_matrix_new"
  goMatrixNew :: CSize -> CSize -> IO (Ptr Matrix)

foreign import ccall "matrix.h golib_matrix_row_count"
  goMatrixRowCount :: Ptr Matrix -> IO CSize
foreign import ccall "matrix.h golib_matrix_col_count"
  goMatrixColCount :: Ptr Matrix -> IO CSize

foreign import ccall "matrix.h golib_matrix_get_elem"
  goMatrixGetElem :: Ptr Matrix -> CSize -> CSize -> IO CDouble

foreign import ccall "matrix.h golib_matrix_set_elem"
  goMatrixSetElem :: Ptr Matrix -> CSize -> CSize -> CDouble -> IO ()

foreign import ccall "matrix.h golib_matrix_fill"
  goMatrixFill :: Ptr Matrix -> CDouble -> IO ()

foreign import ccall "matrix.h golib_matrix_transpose"
  goMatrixTranspose :: Ptr Matrix -> IO ()

foreign import ccall "matrix.h golib_matrix_matrix_mult"
  goMatrixMatrixMult :: CDouble -> Ptr Matrix -> CInt -> Ptr Matrix -> CInt -> CDouble -> Ptr Matrix -> IO ()

foreign import ccall "matrix.h golib_matrix_copy"
  goMatrixCopy :: Ptr Matrix -> Ptr Matrix -> IO ()

foreign import ccall "matrix.h golib_matrix_equals"
  goMatrixEquals :: Ptr Matrix -> Ptr Matrix -> IO CInt

-- {# fun unsafe golib_matrix_new as matrixNew {} -> `Matrix' id #}

{- main = do
     n <- test
     putStrLn $ "test () yields " ++ show n -}