module Golib.Math.Matrix.Monadic
       (matrixMapM) where

import Golib.Math.Matrix.Class.Monadic
import Control.Monad

-- FIXME: Das soll eine neue Matrix zurueckgeben; 
-- ich brauche eine ST-Variante der Matrix, so dass ich auftauen, einfrieren
-- und modifizieren kann.
matrixMapM :: (Monad m, Integral i, MMatrixClass i a mat m) => (a -> a) -> mat -> m [a]
matrixMapM f m = 
  shape m >>= \(r,c) ->
  mapM (\i -> m ! i) [(fromIntegral i,fromIntegral j) | i <- [0..(r-1)], j <- [0..(c-1)]] >>=
  return . map f
