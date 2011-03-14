{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
module Golib.Math.Vector.Class
       (VectorClass (..), 
        toList) where

import Golib.Math.Base

class VectorClass i a v | v -> a, v -> i where
  vector :: i -> v
  vecSize :: v -> i
  (!) :: v -> i -> a
  dot :: v -> v -> a
  infixl 7 *>
  (*>) :: a -> v -> v
  infixl 6 <+>
  (<+>) :: v -> v -> v
  infixl 6 <->
  (<->) :: v -> v -> v
  

{-| Return the elements of the given vector as a list. -}
toList :: (Enum i, Num i, VectorClass i a v) => v -> [a]
toList v = map (v !) [0..n] where n = (vecSize v) - 1
