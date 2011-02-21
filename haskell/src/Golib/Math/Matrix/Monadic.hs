{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Golib.Math.Matrix.Monadic
       (matrixMapM,
        inMatrixRangeM) where

import Golib.Math.Matrix.Class.Monadic
import Control.Monad
import Ix

-- FIXME: Das soll eine neue Matrix zurueckgeben; 
-- ich brauche eine ST-Variante der Matrix, so dass ich auftauen, einfrieren
-- und modifizieren kann.
matrixMapM :: (Monad m, Integral i, MMatrixClass i a mat m) => (a -> a) -> mat -> m [a]
matrixMapM f m = 
  shape m >>= \(r,c) ->
  mapM (\i -> m ! i) [(fromIntegral i,fromIntegral j) | i <- [0..(r-1)], j <- [0..(c-1)]] >>=
  return . map f

inMatrixRangeM :: (Monad m, Integral i, MMatrixClass i a mat m) => mat -> (i,i) -> m Bool
inMatrixRangeM mat (i,j) = shape mat >>= \(r,c) -> return $ inRange (0,r-1) i && inRange (0,c-1) j


-- Spezialisierter matrix modification monad:
-- Matrix in state speichern (StateT?), modifizieren.
-- Oder: allgemeinere Monade, alles moegliche zulassen?
newtype MatrixST s a = MatrixST { unMatrixST :: IO a } deriving Monad

-- thawMatrix :: (Integral i, Fractional a, MatrixClass i a mat) => mat -> MatrixST s mat
-- freezeMatrix :: (Integral i, Fractional a, MatrixClass i a mat) => mat -> MatrixST s mat
-- setElem :: (Integral i, Fractional a, MatrixClass i a mat) => mat -> (i,i) -> a -> MatrixST s ()