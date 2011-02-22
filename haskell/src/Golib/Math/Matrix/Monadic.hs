{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Golib.Math.Matrix.Monadic
       (MMM, 
        getMatrix,
        runMMM,
        setElem,
        getElem) where

import Golib.Math.Matrix
import Golib.Math.Matrix.Class
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Ix


-- Spezialisierter matrix modification monad:
-- Matrix in state speichern (StateT?), modifizieren.
-- Oder: allgemeinere Monade, alles moegliche zulassen?

-- This type is not exported.
type MMonad = StateT Matrix IO

newtype MMM s a = MMM { unMMM :: MMonad a } deriving Monad

-- Make a copy of the matrix, put it in the state, and let modification functions run on it.
runMMM :: Matrix -> MMM s a -> IO Matrix
runMMM mat m = matrixCopyIO mat >>= execStateT (unMMM m)

getMatrix :: MMM s Matrix
getMatrix = MMM $ get

setElem :: (Index,Index) -> Double -> MMM s ()
setElem (i,j) a = MMM $ get >>= \m -> (liftIO $ unsafeSetElemIO m (i,j) a)

getElem :: (Index,Index) -> MMM s Double
getElem (i,j) = MMM $ get >>= \m -> (liftIO $ getElemIO m (i,j))

-- freezeMatrix :: (Integral i, Fractional a, MatrixClass i a mat) => mat -> MatrixST s mat
-- setElem :: (Integral i, Fractional a, MatrixClass i a mat) => mat -> (i,i) -> a -> MatrixST s ()