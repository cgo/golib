{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Golib.Math.Vector.Class
       (VectorClass (..)) where

import Golib.Math.Base

class VectorClass i a v | v -> a, v -> i where
  vector :: i -> v
  vecSize :: v -> i
  (!) :: v -> i -> a
  dot :: v -> v -> a
  scalarProduct :: a -> v -> v