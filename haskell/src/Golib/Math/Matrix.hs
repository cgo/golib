{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Golib.Math.Matrix
       (   MatrixClass(..)
         , Dummy (..)
         , Index  
        ) where

import Golib.Foreign.Math.Matrix

newtype Index = Index Int deriving Show

class MatrixClass a m | m -> a where
  shape ::  m -> (Index, Index)
  rowCount :: m -> Index
  colCount :: m -> Index
  shape m  = (rowCount m, colCount m)
  rowCount = fst . shape
  colCount = snd . shape
  
  -- Scalar mult
  (*>) :: a -> m -> m
  -- Matrix mult
  (<**>) :: m -> m -> m
  -- Matrix vector mult
  -- (<*>) :: (VectorClass v) => m -> v -> v
  

newtype Dummy = Dummy Int deriving Show

instance MatrixClass Double Dummy where
  shape _ = (Index 3, Index 4)
  s *> m = m
  m1 <**> m2 = m2
  
  