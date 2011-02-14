{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Golib.Math.Matrix
       (   MatrixClass(..)
         , Dummy (..)
        ) where

import Golib.Foreign.Math.Matrix

class MatrixClass i a mat | mat -> a, mat -> i where
  shape    :: mat -> (i, i)
  rowCount :: mat -> i
  colCount :: mat -> i
  shape mat = (rowCount mat, colCount mat)
  rowCount = fst . shape
  colCount = snd . shape
  
--  (!) :: mat -> (i, i) -> a
  -- Scalar mult
  (*>) :: a -> mat -> mat
--  (<*) :: mat -> a -> mat
  -- Matrix mult
  (<**>) :: mat -> mat -> mat
  -- Matrix vector mult
  -- (<**) :: (VectorClass v) => m -> v -> v
  -- (**>) :: (VectorClass v) => v -> m -> v
  

newtype Dummy = Dummy Int deriving Show

instance MatrixClass Int Double Dummy where
  shape _ = (3, 4)
  s *> m = m
  m1 <**> m2 = m2
  

