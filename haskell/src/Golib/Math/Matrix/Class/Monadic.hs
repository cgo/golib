{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies  #-}
module Golib.Math.Matrix.Class.Monadic
       (MMatrixClass(..),
       ) where

import Ix
import Control.Monad

class (Monad m, Ix i) => MMatrixClass i a mat m | mat -> a, mat -> i where
  shape    :: mat -> m (i, i)
  rowCount :: mat -> m i
  colCount :: mat -> m i
  shape mat = rowCount mat >>= \m -> colCount mat >>= \n -> return (m, n)
  rowCount mat = shape mat >>= return . fst
  colCount mat = shape mat >>= return . snd
  
  (!) :: mat -> (i, i) -> m a
  -- Scalar mult
  (*>) :: a -> mat -> m mat
--  (<*) :: mat -> a -> mat
  -- Matrix mult
  (<**>) :: mat -> mat -> m mat
  -- Matrix vector mult
  -- (<**) :: (VectorClass v) => m -> v -> v
  -- (**>) :: (VectorClass v) => v -> m -> v

