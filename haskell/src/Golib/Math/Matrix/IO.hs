{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies  #-}
module Golib.Math.Matrix.IO
       (
         rowCountIO,
         colCountIO,
         unsafeGetElemIO,
         getElemIO,
         unsafeMatrixMultIO,
         matrixMultIO,
         module Golib.Math.Matrix.Class.Monadic
       ) where

import Golib.Math.Matrix.Base
import Golib.Foreign.Math.Matrix
import Golib.Math.Matrix.Class.Monadic
import qualified Control.Applicative as App


instance MMatrixClass Int Double GoMatrix IO where
  rowCount = rowCountIO
  colCount = colCountIO
  m ! (r,c) = getElemIO m (r,c)
  a *> mat = return mat
  mat1 <**> mat2 = matrixMultIO 1 mat1 NoTrans mat2 NoTrans 
  

rowCountIO mat = withGoMatrix mat goMatrixRowCount >>= return . fromIntegral
colCountIO mat = withGoMatrix mat goMatrixColCount >>= return . fromIntegral

-- No range checks
unsafeGetElemIO :: (Integral i, Fractional a) => GoMatrix -> (i,i) -> IO a
unsafeGetElemIO mat (r,c) = withGoMatrix mat (\m -> goMatrixGetElem m r' c') >>= return . realToFrac
  where
    c' = fromIntegral c
    r' = fromIntegral r


inRange s e a = a >= s && a < e

getElemIO :: (Integral i, Fractional a) => GoMatrix -> (i,i) -> IO a
getElemIO mat (r,c) = do
  r' <- rowCountIO mat
  c' <- colCountIO mat
  let rInRange = inRange 0 r' r
      cInRange = inRange 0 c' c
  if (rInRange && cInRange)
    then unsafeGetElemIO mat (r,c) 
    else error $ "Range violation --- matrix is of size " ++ show (r',c') ++ ", index was " ++ show (r,c)
                           

{-| Computes c <- beta * c + alpha * a(^T) * b(^T).
    That means c is either updated in-place, or created as needed. 
    No sanity checks are made. Non-matching shapes of a and b leads to undefined behaviour. -}
unsafeMatrixMultIO :: Double -> GoMatrix -> Trans -> GoMatrix -> Trans -> Double -> GoMatrix -> IO ()
unsafeMatrixMultIO alpha a atrans b btrans beta c = do
  withGoMatrix a $ \ma ->
    withGoMatrix b $ \mb -> 
      withGoMatrix c $ \mc ->
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
matrixMultIO :: Double -> GoMatrix -> Trans -> GoMatrix -> Trans -> IO GoMatrix
matrixMultIO alpha a atrans b btrans = 
  shape a >>= \(r,ac) -> shape b >>= \(br,c) -> 
  if ac /= br 
  then (error $ "Trying to multiply " ++ show (r,ac) ++ " by " ++ show (br,c) ++ " matrix.")
  else
    matrixNew r c >>= \result -> 
    unsafeMatrixMultIO alpha a NoTrans b NoTrans 0 result >> return result

