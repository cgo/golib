{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Golib.Math.Matrix
       (
        Matrix,
        matrix,
        module Golib.Math.Matrix.Class,
        module Golib.Math.Matrix.Base,
        rowCountIO,
        colCountIO,
        unsafeGetElemIO,
        getElemIO,
        unsafeSetElemIO,
        unsafeMatrixMultIO,
        unsafeTransposeIO,
        unsafeFillIO,
        unsafeMatrixCopyIO,
        matrixCopyIO,
        matrixMultIO,
        matrixNew
       ) where

import Golib.Foreign.Math.Matrix
import Golib.Math.Matrix.Class
import Golib.Math.Matrix.Base
import Foreign.C.Types
import Foreign
import Ix

import Foreign.Marshal.Utils


instance MatrixClass Int Double Matrix where
  rowCount gm = unsafePerformIO . rowCountIO $ gm
  colCount gm = unsafePerformIO . colCountIO $ gm
  m ! (r,c) = unsafePerformIO $ getElemIO m (r,c)
  -- a *> mat = mat
  m1 <**> m2 = unsafePerformIO $ matrixMultIO 1 m1 NoTrans m2 NoTrans


instance Eq Matrix where
  m1 == m2 = unsafePerformIO $ matrixEqualsIO m1 m2


{-| Construct a new matrix of given shape. 
    r are the number of rows, c the number of columns. -}
matrix :: (Integral i) => i -> i -> Matrix
matrix r c = unsafePerformIO matrix'
  where
    r' = fromIntegral r
    c' = fromIntegral c
    matrix' = matrixNew r' c'
{-# NOINLINE matrix #-}


rowCountIO mat = withMatrix mat goMatrixRowCount >>= return . fromIntegral
colCountIO mat = withMatrix mat goMatrixColCount >>= return . fromIntegral


-- No range checks
unsafeGetElemIO :: Matrix -> (Index,Index) -> IO Double
unsafeGetElemIO mat (r,c) = withMatrix mat (\m -> goMatrixGetElem m r' c') >>= return . realToFrac
  where
    c' = fromIntegral c
    r' = fromIntegral r


getElemIO :: Matrix -> (Index,Index) -> IO Double
getElemIO mat (r,c) =
  if inMatrixRange mat (r,c)
    then unsafeGetElemIO mat (r,c)
    else error $ "Range violation --- matrix is of size " ++ show (shape mat) ++ ", index was " ++ show (r,c)
                           

-- FIXME: Real is not good for the future (Complex should also be possible!)
{-| Sets the given element in the given matrix, in-place (therefore unsafe). -}
unsafeSetElemIO :: Matrix -> (Index,Index) -> Double -> IO ()
unsafeSetElemIO mat (i,j) a = do
  let t = inMatrixRange mat (i,j)
  if t
    then (withMatrix mat (\m -> goMatrixSetElem m i' j' a'))
    else error $ "setElem: range violation --- index was " ++ show (i,j)
  where
    i' = fromIntegral i
    j' = fromIntegral j
    a' = realToFrac a


{-| Fills the given matrix with the given value, in-place (therefore unsafe) -}
unsafeFillIO :: Real a => Matrix -> a -> IO ()
unsafeFillIO mat a = withMatrix mat $ \m -> goMatrixFill m (realToFrac a)


unsafeTransposeIO :: Matrix -> IO ()
unsafeTransposeIO = flip withMatrix $ goMatrixTranspose


unsafeMatrixCopyIO :: Matrix -> Matrix -> IO ()
unsafeMatrixCopyIO source target = 
  withMatrix source $ \s -> withMatrix target $ \t -> goMatrixCopy s t
  

matrixCopyIO :: Matrix -> IO Matrix
matrixCopyIO m = let (r,c) = shape m in
  matrixNew r c >>= \mc -> unsafeMatrixCopyIO m mc >> return mc


matrixEqualsIO :: Matrix -> Matrix -> IO Bool
matrixEqualsIO m1 m2 = 
  withMatrix m1 $ \mm1 -> withMatrix m2 $ \mm2 ->
  goMatrixEquals mm1 mm2 >>= return . toBool


{-| Computes c <- beta * c + alpha * a(^T) * b(^T).
    That means c is either updated in-place, or created as needed. 
    No sanity checks are made. Non-matching shapes of a and b leads to undefined behaviour. -}
unsafeMatrixMultIO :: Double -> Matrix -> Trans -> Matrix -> Trans -> Double -> Matrix -> IO ()
unsafeMatrixMultIO alpha a atrans b btrans beta c = do
  withMatrix a $ \ma ->
    withMatrix b $ \mb -> 
      withMatrix c $ \mc ->
        goMatrixMatrixMult alpha' ma atrans' mb btrans' beta' mc
  where
    alpha'  = realToFrac alpha
    beta'   = realToFrac beta
    atrans' = fromIntegral $ fromEnum atrans
    btrans' = fromIntegral $ fromEnum btrans
  
  
{-| Safe matrix multiplication in the IO monad.
    See unsafeMatrixMultIO. The result is newly created and none of the existing
    objects are changed. 
    In case the shapes of the matrices a and b do not match, an exception is raised with error. -}
matrixMultIO :: Double -> Matrix -> Trans -> Matrix -> Trans -> IO Matrix
matrixMultIO alpha a atrans b btrans = 
  let (r,ac) = shape a
      (br,c) = shape b in
  if ac /= br 
  then (error $ "Trying to multiply " ++ show (r,ac) ++ " by " ++ show (br,c) ++ " matrix.")
  else
    matrixNew r c >>= \result -> 
    unsafeMatrixMultIO alpha a NoTrans b NoTrans 0 result >> return result

