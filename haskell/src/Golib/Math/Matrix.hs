{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module Golib.Math.Matrix
       (
        Matrix,
        matrix,
        toList,
        module Golib.Math.Matrix.Class
       ) where

import Golib.Foreign.Math.Matrix
import Golib.Math.Matrix.Class
import Golib.Math.Matrix.Base
import qualified Golib.Math.Matrix.IO as MIO
import Foreign.C.Types
import Foreign
import Ix

data Matrix = Matrix GoMatrix


-- instance Eq GoMatrix where

instance MatrixClass Int Double Matrix where
  rowCount (Matrix gm) = unsafePerformIO . MIO.rowCountIO $ gm
  colCount (Matrix gm) = unsafePerformIO . MIO.colCountIO $ gm
  (Matrix m) ! (r,c) = unsafePerformIO $ m MIO.! (r,c)
  -- a *> mat = mat
  (Matrix m1) <**> (Matrix m2) = Matrix $ unsafePerformIO $ MIO.matrixMultIO 1 m1 NoTrans m2 NoTrans
  


{-| Construct a new matrix of given shape. 
    r are the number of rows, c the number of columns. -}
matrix :: (Integral i) => i -> i -> Matrix
matrix r c = Matrix $ unsafePerformIO matrix'
  where
    r' = fromIntegral r
    c' = fromIntegral c
    matrix' = matrixNew r' c'
{-# INLINE matrix #-}
    
toList :: (Integral i, Num a, MatrixClass i a mat) => mat -> [a]
toList mat = let (r,c) = shape mat
             in [mat ! (i,j) | i <- [0..(r-1)], j <- [0..(c-1)]]

-- fromList :: (Integral i, Num a, MatrixClass i a mat) => i -> i -> [a] -> mat
-- fromList r c l = 