{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Golib.Math.Matrix
       (
         -- Pure things
        Matrix,
        matrix,
        fromList,
        setElems,
        prettyPrintMatrix,
        invert,
        trans,
        module Golib.Math.Matrix.Class,
        module Golib.Math.Matrix.Base,
        -- IO things
        prettyPrintMatrixIO,
        rowCountIO,
        colCountIO,
        unsafeGetElemIO,
        getElemIO,
        unsafeSetElemIO,
        unsafeMatrixMultIO,
        unsafeTransposeIO,
        transposeIO,
        unsafeFillIO,
        unsafeMatrixCopyIO,
        matrixCopyIO,
        matrixMultIO,
        unsafeInvertIO,
        invertIO,
        matrixNew,

        --- Monadic things
        MMM, 
        getMatrix,
        runMMM,
        createMatrix,
        modifyMatrix,
        setElem,
        getElem
       ) where

import Golib.Foreign.Math.Matrix
import Golib.Math.Matrix.Class
import Golib.Math.Matrix.Base
import Foreign.C.Types
import Foreign
import Ix
import Maybe
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Foreign.Marshal.Utils


instance MatrixClass Int Double Matrix where
  rowCount gm = unsafePerformIO . rowCountIO $ gm
  colCount gm = unsafePerformIO . colCountIO $ gm
  m ! (r,c) = unsafePerformIO $ getElemIO m (r,c)
  -- a *> mat = mat
  m1 <**> m2 = unsafePerformIO $ matrixMultIO 1 m1 NoTrans m2 NoTrans


instance Eq Matrix where
  m1 == m2 = unsafePerformIO $ matrixEqualsIO m1 m2


instance Show Matrix where
  show m = "fromJust (fromList " ++ show r ++ " " ++ show c ++ " " ++ show (toList m) ++ ")"
    where
      (r,c) = shape m


{-| Construct a new matrix of given shape. 
    r are the number of rows, c the number of columns. -}
matrix :: (Integral i) => i -> i -> Matrix
matrix r c = unsafePerformIO matrix'
  where
    r' = fromIntegral r
    c' = fromIntegral c
    matrix' = matrixNew r' c'
{-# NOINLINE matrix #-}


fromList :: Index -> Index -> [Double] -> Maybe Matrix
fromList r c l = if (c >= 0 && c >= 0 && (r*c == length l))
                 then Just $ createMatrix r c $ 
                      sequence $ map (\(i,a) -> setElem i a) $ zip [(i,j) | i <- [0..(r-1)], j <- [0..(c-1)]] l
                 else Nothing


{-| Sets elements in a matrix; caution: invalid indices are silently ommitted. -}
setElems :: Matrix -> [((Index,Index),Double)] -> Matrix
setElems mat p = modifyMatrix mat $ sequence $ map (\(ij,a) -> setElem ij a) p
                                           

{-| Returns Just the inverse of the given matrix if it can be computed, or Nothing. -}
invert :: Matrix -> Maybe Matrix
invert m = unsafePerformIO $ invertIO m


trans :: Matrix -> Matrix
trans m = fromJust . unsafePerformIO $ transposeIO m -- Assuming transposition always works (why shouldn't it?)


prettyPrintMatrix :: Matrix -> [String]
prettyPrintMatrix m = map ppl $ toLists m
  where
    pp a = show a ++ " "
    ppl = concatMap pp


-------------------------------------------------------
-- All things IO and/or unsafe are coming up now.
-------------------------------------------------------

prettyPrintMatrixIO :: Matrix -> IO ()
prettyPrintMatrixIO m = sequence (map putStrLn $ prettyPrintMatrix m) >> return ()


rowCountIO mat = withMatrix mat goMatrixRowCount >>= return . fromIntegral
colCountIO mat = withMatrix mat goMatrixColCount >>= return . fromIntegral


-- No range checks
unsafeGetElemIO :: Matrix -> (Index,Index) -> IO Double
unsafeGetElemIO mat (r,c) = withMatrix mat (\m -> goMatrixGetElem m r' c') >>= return . realToFrac
  where
    c' = fromIntegral c
    r' = fromIntegral r


getElemIO :: Matrix -> (Index,Index) -> IO Double
getElemIO mat (r,c) =
  if inMatrixRange mat (r,c)
    then unsafeGetElemIO mat (r,c)
    else error $ "Range violation --- matrix is of size " ++ show (shape mat) ++ ", index was " ++ show (r,c)
                           

-- FIXME: Real is not good for the future (Complex should also be possible!)
{-| Sets the given element in the given matrix, in-place (therefore unsafe). -}
unsafeSetElemIO :: Matrix -> (Index,Index) -> Double -> IO Bool
unsafeSetElemIO mat (i,j) a = do
  let t = inMatrixRange mat (i,j)
  if t
    then liftM (toEnum . fromIntegral) $ (withMatrix mat (\m -> goMatrixSetElem m i' j' a'))
    else error $ "setElem: range violation --- index was " ++ show (i,j)
  where
    i' = fromIntegral i
    j' = fromIntegral j
    a' = realToFrac a


{-| Fills the given matrix with the given value, in-place (therefore unsafe) -}
unsafeFillIO :: Real a => Matrix -> a -> IO ()
unsafeFillIO mat a = withMatrix mat $ \m -> goMatrixFill m (realToFrac a)


{-| Transposition of a matrix in place, therefore unsafe. -}
unsafeTransposeIO :: Matrix -> IO Bool
unsafeTransposeIO m = liftM (toEnum . fromIntegral) $ withMatrix m (\pm -> goMatrixTranspose pm)


{-| Safe version of transposition; the matrix is copied, the transposed, and the copy is returned. 
    If something went wrong, Nothing is returned. -}
transposeIO :: Matrix -> IO (Maybe Matrix)
transposeIO m = 
  matrixNew c r >>= \result ->
  (withMatrix m $ \pm ->
    withMatrix result $ \presult ->
    goMatrixTransposeTo pm presult) >>= \i -> 
  case toEnum (fromIntegral i) of
    True -> return $ Just result
    False -> return Nothing
  where
    (r,c) = shape m
    

unsafeInvertIO :: Matrix -> IO Bool
unsafeInvertIO m = liftM (toEnum . fromIntegral) $ withMatrix m goMatrixInvert


invertIO :: Matrix -> IO (Maybe Matrix)
invertIO m = 
  matrixCopyIO m >>= \result -> 
  withMatrix result goMatrixInvert >>= \i ->
  case (toEnum . fromIntegral) i of
    True -> return $ Just result
    False -> return Nothing
    

unsafeMatrixCopyIO :: Matrix -> Matrix -> IO ()
unsafeMatrixCopyIO source target = 
  withMatrix source $ \s -> withMatrix target $ \t -> goMatrixCopy s t
  

matrixCopyIO :: Matrix -> IO Matrix
matrixCopyIO m = let (r,c) = shape m in
  matrixNew r c >>= \mc -> unsafeMatrixCopyIO m mc >> return mc


matrixEqualsIO :: Matrix -> Matrix -> IO Bool
matrixEqualsIO m1 m2 = 
  withMatrix m1 $ \mm1 -> withMatrix m2 $ \mm2 ->
  goMatrixEquals mm1 mm2 >>= return . toBool


{-| Computes c <- beta * c + alpha * a(^T) * b(^T).
    That means c is either updated in-place, or created as needed. 
    No sanity checks are made. Non-matching shapes of a and b leads to undefined behaviour. -}
unsafeMatrixMultIO :: Double -> Matrix -> Trans -> Matrix -> Trans -> Double -> Matrix -> IO ()
unsafeMatrixMultIO alpha a atrans b btrans beta c = do
  withMatrix a $ \ma ->
    withMatrix b $ \mb -> 
      withMatrix c $ \mc ->
        goMatrixMatrixMult alpha' ma atrans' mb btrans' beta' mc
  where
    alpha'  = realToFrac alpha
    beta'   = realToFrac beta
    atrans' = fromIntegral $ fromEnum atrans
    btrans' = fromIntegral $ fromEnum btrans
  
  
{-| Safe matrix multiplication in the IO monad.
    See unsafeMatrixMultIO. The result is newly created and none of the existing
    objects are changed. 
    In case the shapes of the matrices a and b do not match, an exception is raised with error. -}
matrixMultIO :: Double -> Matrix -> Trans -> Matrix -> Trans -> IO Matrix
matrixMultIO alpha a atrans b btrans = 
  let (r,ac) = shape a
      (br,c) = shape b in
  if ac /= br 
  then (error $ "Trying to multiply " ++ show (r,ac) ++ " by " ++ show (br,c) ++ " matrix.")
  else
    matrixNew r c >>= \result -> 
    unsafeMatrixMultIO alpha a NoTrans b NoTrans 0 result >> return result


---------------------------------------------------------
-- Monadic matrix manipulation
---------------------------------------------------------


-- This type is not exported.
type MMonad = StateT Matrix IO

newtype MMM s a = MMM { unMMM :: MMonad a } deriving Monad


-- Make a copy of the matrix, put it in the state, and let modification functions run on it.
-- Does /not/ allow anything else to be modified than the copy of the matrix that is given as argument.
runMMM :: Matrix -> MMM s a -> IO Matrix
runMMM mat m = matrixCopyIO mat >>= execStateT (unMMM m)


createMatrix :: Index -> Index -> MMM s a -> Matrix
createMatrix r c m = unsafePerformIO $ matrixNew r c >>= execStateT (unMMM m)


modifyMatrix :: Matrix -> MMM s a -> Matrix
modifyMatrix mat m = unsafePerformIO $ matrixCopyIO mat >>= execStateT (unMMM m)


getMatrix :: MMM s Matrix
getMatrix = MMM $ get


setElem :: (Index,Index) -> Double -> MMM s Bool
setElem (i,j) a = MMM $ get >>= \m -> if inMatrixRange m (i,j)
                                     then (liftIO $ unsafeSetElemIO m (i,j) a) >> return True
                                     else return False


getElem :: (Index,Index) -> MMM s Double
getElem (i,j) = MMM $ get >>= \m -> (liftIO $ getElemIO m (i,j))

-- freezeMatrix :: (Integral i, Fractional a, MatrixClass i a mat) => mat -> MatrixST s mat
-- setElem :: (Integral i, Fractional a, MatrixClass i a mat) => mat -> (i,i) -> a -> MatrixST s ()
