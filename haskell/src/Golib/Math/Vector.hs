{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-} -- for Index.

module Golib.Math.Vector
       (
         module Golib.Foreign.Math.Vector,
         module Golib.Math.Vector.Class, 
         fromList ) where

import Golib.Math.Base
import Golib.Foreign.Math.Vector (VMM, Vector, createVector, modifyVector, getElem, setElem, setElems, getVector, vectorAdd)
import qualified Golib.Foreign.Math.Vector as F
import Golib.Math.Vector.Class
import Foreign

instance VectorClass Index Double F.Vector where
  vector i = unsafePerformIO $ F.vectorNew i
  
  vecSize = F.vectorSize
  
  v ! i = case F.getElem' v i of
    Just e -> e
    Nothing -> 0.0
    
  dot v1 v2 = case F.dot v1 v2 of
    Just e -> e
    Nothing -> 0.0
  
  s *> v = unsafePerformIO $ 
           F.vectorNew (vecSize v) >>= \result -> 
           F.unsafeCopy v result >> 
           F.unsafeScalarMult result s >> return result
  
  v1 <+> v2 = modifyVector v1 $ vectorAdd 1 v2
  v1 <-> v2 = modifyVector v1 $ vectorAdd (-1) v2


{-| Create a vector from the given list of values. See also toList. -}
fromList :: [Double] -> F.Vector
fromList as = createVector n $ setElems (zip [0..(n-1)] as)
  where n = length as


instance Eq F.Vector where
  (==) = F.equals
  
instance Show F.Vector where
  show v = "fromList " ++ show (toList v)

