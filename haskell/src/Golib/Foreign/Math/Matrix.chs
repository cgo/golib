{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Golib.Foreign.Math.Matrix
(IOMatrix,
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
 withIOMatrix) where

import C2HS
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable


#include "cpp/matrix.h"

{# context lib="golib" #}

-- getNumber = {#call unsafe getNumber as _getNumber#}
-- {# fun unsafe test as test {} -> `Int' peekIntConv*  #}
{# fun unsafe test as test {} -> `Int' #}

{# pointer *golib_matrix as IOMatrix foreign newtype #}
-- {# fun unsafe golib_matrix_new as matrixNew' {} -> `Ptr IOMatrix' id #}

matrixNew r c = goMatrixNew (fromIntegral r) (fromIntegral c) >>= \mp ->
  checkNullPtr mp >>= \n ->
  if n
  then error ("Matrix: allocation error when allocating " ++ show r ++ "," ++
              show c ++ " matrix!")
  else return () >>
  newForeignPtr goMatrixFinalize mp >>= return . IOMatrix
-- These are generated automatically by the above c2hs line.
--withIOMatrix :: IOMatrix -> (Ptr IOMatrix -> IO a) -> IO a
--withIOMatrix (IOMatrix m) = withForeignPtr m

checkNullPtr :: Ptr a -> IO Bool
checkNullPtr p = goCheckNullPtr (castPtr p) >>= \i ->
  case i of
    1 -> return True
    _ -> return False


foreign import ccall "matrix.h golib_check_null_ptr"
  goCheckNullPtr :: Ptr () -> IO CInt

foreign import ccall "matrix.h &golib_matrix_destroy"
  goMatrixFinalize :: FunPtr (Ptr IOMatrix -> IO ())

foreign import ccall "matrix.h golib_matrix_new"
  goMatrixNew :: CSize -> CSize -> IO (Ptr IOMatrix)

foreign import ccall "matrix.h golib_matrix_row_count"
  goMatrixRowCount :: Ptr IOMatrix -> IO CSize
foreign import ccall "matrix.h golib_matrix_col_count"
  goMatrixColCount :: Ptr IOMatrix -> IO CSize

foreign import ccall "matrix.h golib_matrix_get_elem"
  goMatrixGetElem :: Ptr IOMatrix -> CSize -> CSize -> IO CDouble

foreign import ccall "matrix.h golib_matrix_set_elem"
  goMatrixSetElem :: Ptr IOMatrix -> CSize -> CSize -> CDouble -> IO ()

foreign import ccall "matrix.h golib_matrix_fill"
  goMatrixFill :: Ptr IOMatrix -> CDouble -> IO ()

foreign import ccall "matrix.h golib_matrix_transpose"
  goMatrixTranspose :: Ptr IOMatrix -> IO ()

foreign import ccall "matrix.h golib_matrix_matrix_mult"
  goMatrixMatrixMult :: CDouble -> Ptr IOMatrix -> CInt -> Ptr IOMatrix -> CInt -> CDouble -> Ptr IOMatrix -> IO ()

foreign import ccall "matrix.h golib_matrix_copy"
  goMatrixCopy :: Ptr IOMatrix -> Ptr IOMatrix -> IO ()

foreign import ccall "matrix.h golib_matrix_equals"
  goMatrixEquals :: Ptr IOMatrix -> Ptr IOMatrix -> IO CInt

-- {# fun unsafe golib_matrix_new as matrixNew {} -> `IOMatrix' id #}

{- main = do
     n <- test
     putStrLn $ "test () yields " ++ show n -}