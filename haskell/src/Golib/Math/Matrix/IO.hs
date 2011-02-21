{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts  #-}
module Golib.Math.Matrix.IO
       (
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
         module Golib.Math.Matrix.Class.Monadic,
         IOMatrix,
         matrixNew
       ) where

import Golib.Math.Matrix.Base
import Golib.Foreign.Math.Matrix
import Foreign.Marshal.Utils
import Foreign
import Golib.Math.Matrix.Class.Monadic
import Golib.Math.Matrix.Monadic
import qualified Control.Applicative as App


instance MMatrixClass Int Double IOMatrix IO where
  rowCount = rowCountIO
  colCount = colCountIO
  m ! (r,c) = getElemIO m (r,c)
  a *> mat = return mat
  mat1 <**> mat2 = matrixMultIO 1 mat1 NoTrans mat2 NoTrans 
  

instance Eq IOMatrix where
  m1 == m2 = unsafePerformIO $ matrixEqualsIO m1 m2


rowCountIO mat = withIOMatrix mat goMatrixRowCount >>= return . fromIntegral
colCountIO mat = withIOMatrix mat goMatrixColCount >>= return . fromIntegral

-- No range checks
unsafeGetElemIO :: (Integral i, Fractional a) => IOMatrix -> (i,i) -> IO a
unsafeGetElemIO mat (r,c) = withIOMatrix mat (\m -> goMatrixGetElem m r' c') >>= return . realToFrac
  where
    c' = fromIntegral c
    r' = fromIntegral r


inRange s e a = a >= s && a < e

inMatrixRangeIO :: (Integral i, Fractional a, MMatrixClass i a mat IO) => mat -> (i,i) -> IO Bool
inMatrixRangeIO = inMatrixRangeM

getElemIO :: (Integral i, Fractional a) => IOMatrix -> (i,i) -> IO a
getElemIO mat (r,c) = do
  r' <- rowCountIO mat
  c' <- colCountIO mat
  let rInRange = inRange 0 r' r
      cInRange = inRange 0 c' c
  if (rInRange && cInRange)
    then unsafeGetElemIO mat (r,c) 
    else error $ "Range violation --- matrix is of size " ++ show (r',c') ++ ", index was " ++ show (r,c)
                           

-- FIXME: Real is not good for the future (Complex should also be possible!)
{-| Sets the given element in the given matrix, in-place (therefore unsafe). -}
unsafeSetElemIO :: (Integral i, Real a) => IOMatrix -> (i,i) -> a -> IO ()
unsafeSetElemIO mat (i,j) a = do
  t <- inMatrixRangeIO mat (fromIntegral i,fromIntegral j)
  if t
    then (withIOMatrix mat (\m -> goMatrixSetElem m i' j' a'))
    else error $ "setElem: range violation --- index was " ++ show (i,j)
  where
    i' = fromIntegral i
    j' = fromIntegral j
    a' = realToFrac a


{-| Fills the given matrix with the given value, in-place (therefore unsafe) -}
unsafeFillIO :: Real a => IOMatrix -> a -> IO ()
unsafeFillIO mat a = withIOMatrix mat $ \m -> goMatrixFill m (realToFrac a)


unsafeTransposeIO :: IOMatrix -> IO ()
unsafeTransposeIO = flip withIOMatrix $ goMatrixTranspose


unsafeMatrixCopyIO :: IOMatrix -> IOMatrix -> IO ()
unsafeMatrixCopyIO source target = 
  withIOMatrix source $ \s -> withIOMatrix target $ \t -> goMatrixCopy s t
  

matrixCopyIO :: IOMatrix -> IO IOMatrix
matrixCopyIO m = shape m >>= \(r,c) -> 
  matrixNew r c >>= \mc -> unsafeMatrixCopyIO m mc >> return mc


matrixEqualsIO :: IOMatrix -> IOMatrix -> IO Bool
matrixEqualsIO m1 m2 = 
  withIOMatrix m1 $ \mm1 -> withIOMatrix m2 $ \mm2 ->
  goMatrixEquals mm1 mm2 >>= return . toBool


{-| Computes c <- beta * c + alpha * a(^T) * b(^T).
    That means c is either updated in-place, or created as needed. 
    No sanity checks are made. Non-matching shapes of a and b leads to undefined behaviour. -}
unsafeMatrixMultIO :: Double -> IOMatrix -> Trans -> IOMatrix -> Trans -> Double -> IOMatrix -> IO ()
unsafeMatrixMultIO alpha a atrans b btrans beta c = do
  withIOMatrix a $ \ma ->
    withIOMatrix b $ \mb -> 
      withIOMatrix c $ \mc ->
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
matrixMultIO :: Double -> IOMatrix -> Trans -> IOMatrix -> Trans -> IO IOMatrix
matrixMultIO alpha a atrans b btrans = 
  shape a >>= \(r,ac) -> shape b >>= \(br,c) -> 
  if ac /= br 
  then (error $ "Trying to multiply " ++ show (r,ac) ++ " by " ++ show (br,c) ++ " matrix.")
  else
    matrixNew r c >>= \result -> 
    unsafeMatrixMultIO alpha a NoTrans b NoTrans 0 result >> return result

