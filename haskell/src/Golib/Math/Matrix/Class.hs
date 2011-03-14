{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies  #-}
module Golib.Math.Matrix.Class
       (MatrixClass(..),
        MatrixVectorClass(..),
        toList,
        toLists,
        matrixMap,
        inMatrixRange
       ) where

import Ix
import qualified Golib.Math.Vector.Class as VC
import qualified Golib.Math.Vector as V

{-| Matrix / Vector operations -}
class (MatrixClass i a mat, VC.VectorClass i a vec) => 
      MatrixVectorClass i a mat vec | mat -> a, mat -> i, mat -> vec where
        (#|) :: mat -> vec -> vec
        --(|#) :: vec -> mat -> Maybe vec
        --outer :: vec -> vec -> mat
  


class MatrixClass i a mat | mat -> a, mat -> i where
  matrix   :: i -> i -> mat
  shape    :: mat -> (i, i)
  rowCount :: mat -> i
  colCount :: mat -> i
  shape mat = (rowCount mat, colCount mat)
  rowCount = fst . shape
  colCount = snd . shape
  
  (!) :: mat -> (i, i) -> a
  -- Scalar mult
  infixl 7 *>
  (*>) :: a -> mat -> mat
  infixl 6 <+>
  (<+>) :: mat -> mat -> mat
  infixl 6 <->
  (<->) :: mat -> mat -> mat
  -- Matrix mult
  infixl 7 <**>
  (<**>) :: mat -> mat -> mat

  


toList :: (Integral i, Num a, MatrixClass i a mat) => mat -> [a]
toList mat = let (r,c) = shape mat
             in [mat ! (i,j) | i <- [0..(r-1)], j <- [0..(c-1)]]

toLists :: (Integral i, Num a, MatrixClass i a mat) => mat -> [[a]]
toLists mat = let (r,c) = shape mat
              in [[mat ! (i,j) | j <- [0..(c-1)]] | i <- [0..(r-1)]]


-- FIXME: Das soll eine neue Matrix zurueckgeben; 
-- ich brauche eine ST-Variante der Matrix, so dass ich auftauen, einfrieren
-- und modifizieren kann.
matrixMap :: (Integral i, MatrixClass i a mat) => (a -> a) -> mat -> [a]
matrixMap f m = 
  map f $ map (\i -> m ! i) 
              [(fromIntegral i,fromIntegral j) | i <- [0..(r-1)], j <- [0..(c-1)]]
    where
      (r,c) = shape m

inMatrixRange :: (Ix i, Integral i, MatrixClass i a mat) => mat -> (i,i) -> Bool
inMatrixRange mat (i,j) = inRange (0,r-1) i && inRange (0,c-1) j
  where
    (r,c) = shape mat
