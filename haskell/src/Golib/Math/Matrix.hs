{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, TypeSynonymInstances #-}
module Golib.Math.Matrix
       (
         -- Pure things
        Matrix,
        fromList,
        prettyPrintMatrix,
        invert,
        pseudoInverse,
        trans,
        diagIndices,
        idMatrix,
        subMatrix,
        module Golib.Math.Matrix.Class,
        module Golib.Math.Base,
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
        setElems,
        setRow,
        setColumn,
        getElem,
        fill,
        setDiag
       ) where

import Golib.Foreign.Math.Matrix
import qualified Golib.Math.Vector as V
import qualified Golib.Foreign.Math.Vector as FV
import Golib.Math.Matrix.Class
import Golib.Math.Base
import Foreign.C.Types
import Foreign
import Ix
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Foreign.Marshal.Utils
import Data.Maybe (fromJust)


instance MatrixVectorClass Index Double Matrix V.Vector where
  mat #| vec = unsafePerformIO $ FV.vectorNew n >>= \ret -> 
    unsafeMatrixVectorMult 1 mat NoTrans vec 0 ret >>= \t -> 
    if t 
    then return ret 
    else error "Function (#|) failed."
      where n = V.vecSize vec



instance MatrixClass Int Double Matrix where
  matrix = matrix'
  rowCount gm = unsafePerformIO . rowCountIO $ gm
  colCount gm = unsafePerformIO . colCountIO $ gm
  m ! (r,c) = unsafePerformIO $ getElemIO m (r,c)
  a *> mat = unsafePerformIO $ matrixCopyIO mat >>= \m -> unsafeMatrixScalarMult m a >> return m
  m1 <**> m2 = unsafePerformIO $ matrixMultIO 1 m1 NoTrans m2 NoTrans
  -- NOTE: No range checks are done!
  m1 <+> m2 = unsafePerformIO $ matrixCopyIO m1 >>= \m -> unsafeMatrixAdd m m2 >> return m
  m1 <-> m2 = unsafePerformIO $ matrixCopyIO m1 >>= \m -> unsafeMatrixSub m m2 >> return m

 
instance Eq Matrix where
  m1 == m2 = unsafePerformIO $ matrixEqualsIO m1 m2


instance Show Matrix where
  show m = "fromJust (fromList " ++ show r ++ " " ++ show c ++ " " ++ show (toList m) ++ ")"
    where
      (r,c) = shape m



{-| Construct a new matrix of given shape. 
    r are the number of rows, c the number of columns. -}
matrix' :: (Integral i) => i -> i -> Matrix
matrix' r c = unsafePerformIO matrix'
  where
    r' = fromIntegral r
    c' = fromIntegral c
    matrix' = matrixNew r' c'
{-# NOINLINE matrix' #-}

ones :: (Real i) => [i]
ones = 1 : ones

idMatrix :: (Integral i) => i -> Matrix
idMatrix i = createMatrix i' i' $ setDiag 0 ones
  where i' = fromIntegral i


fromList :: Index -> Index -> [Double] -> Maybe Matrix
fromList r c l = if c >= 0 && c >= 0 && (r*c == length l)
                 then Just $ createMatrix r c $ 
                      mapM (uncurry setElem) $ zip [(i,j) | i <- [0..(r-1)], j <- [0..(c-1)]] l
                 else Nothing

{-| Create the sub matrix of given shape at the given position in the given matrix. -}
subMatrix :: Shape -- ^ The shape of the sub matrix. Note that the shape may be smaller than this, depending on whether the submatrix fits in the given matrix at that position.
            -> Index -- ^ Row position
            -> Index -- ^ Column position
            -> Matrix -- ^ The source matrix to copy values from
            -> Matrix
subMatrix (r,c) i j src = createMatrix r' c' $ setElems [(p,src ! (p' p)) | p <- range((0,0),(r',c'))]
  where
    p' (i',j') = (i' + i, j' + j)
    r' = min (rr - i) r
    c' = min (cc - j) c
    (rr,cc) = shape src

-- setElems :: Matrix -> [((Index,Index),Double)] -> Matrix
-- setElems mat p = modifyMatrix mat $ mapM (\(ij,a) -> setElem ij a) p
-- setElems mat p = modifyMatrix mat $ mapM (uncurry setElem) p
                                           

{-| Returns Just the inverse of the given matrix if it can be computed, or Nothing. -}
invert :: Matrix -> Maybe Matrix
invert m = unsafePerformIO $ invertIO m

{-| Returns the transposed version of the input matrix. -}
trans :: Matrix -> Matrix
trans m = fromJust . unsafePerformIO $ transposeIO m -- Assuming transposition always works (why shouldn't it?)


{-| P^T (P P^T)^(-1)  --  works only for fat matrices (?) -}
pseudoInverse :: Matrix -> Maybe Matrix
pseudoInverse m = invert (m <**> mT) >>= \mmT' -> return $ mT <**> mmT'
  where mT = trans m

prettyPrintMatrix :: Matrix -> [String]
prettyPrintMatrix m = map ppl $ toLists m
  where
    pp a = show a ++ " "
    ppl = concatMap pp


{-| Generate indices of a diagonal in a matrix of given shape. -}
diagIndices :: (Index,Index)  -- ^ The shape of the matrix (rows,columns)
              -> Index        -- ^ The index of the diagonal -- 0: main diagonal; < 0: lower diagonals; >0: upper diagonals
              -> [(Index,Index)] -- ^ Index list. Empty if there is no such diagonal.
diagIndices (r,c) d
  | d >= 0 && d < c    = diagIndices' (0, d, min (c - d) r)
  | d < 0 && d > (-r) = diagIndices' (-d, 0, min (r + d) c)
  | otherwise        = []
    where
      diagIndices' :: (Index,Index,Index) -> [(Index,Index)]
      diagIndices' (rstart,cstart,n) = [(rstart + i, cstart + i) | i <- [0..(max 0 (n-1))]]



-------------------------------------------------------
-- All things IO and/or unsafe are coming up now.
-------------------------------------------------------

prettyPrintMatrixIO :: Matrix -> IO ()
prettyPrintMatrixIO m = mapM_ putStrLn $ prettyPrintMatrix m


rowCountIO mat = fmap fromIntegral $ withMatrix mat goMatrixRowCount
colCountIO mat = fmap fromIntegral $ withMatrix mat goMatrixColCount


-- No range checks
unsafeGetElemIO :: Matrix -> (Index,Index) -> IO Double
unsafeGetElemIO mat (r,c) = fmap realToFrac $ withMatrix mat (\m -> goMatrixGetElem m r' c')
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
    then liftM (toEnum . fromIntegral) $ withMatrix mat (\m -> goMatrixSetElem m i' j' a')
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
unsafeTransposeIO m = liftM (toEnum . fromIntegral) $ withMatrix m goMatrixTranspose


{-| Safe version of transposition; the matrix is copied, the transposed, and the copy is returned. 
    If something went wrong, Nothing is returned. -}
transposeIO :: Matrix -> IO (Maybe Matrix)
transposeIO m = 
  matrixNew c r >>= \result ->
  withMatrix m (\pm ->
    withMatrix result $ \presult ->
    goMatrixTransposeTo pm presult) >>= \i -> 
  if toEnum (fromIntegral i) then return (Just result) else return Nothing
    where (r,c) = shape m
    

unsafeInvertIO :: Matrix -> IO Bool
unsafeInvertIO m = liftM (toEnum . fromIntegral) $ withMatrix m goMatrixInvert


invertIO :: Matrix -> IO (Maybe Matrix)
invertIO m = 
  matrixCopyIO m >>= \result -> 
  withMatrix result goMatrixInvert >>= \i ->
  if (toEnum . fromIntegral) i then return (Just result) else return Nothing
    

unsafeMatrixCopyIO :: Matrix -> Matrix -> IO ()
unsafeMatrixCopyIO source target = 
  withMatrix source $ \s -> withMatrix target $ \t -> goMatrixCopy s t
  

matrixCopyIO :: Matrix -> IO Matrix
matrixCopyIO m = let (r,c) = shape m in
  matrixNew r c >>= \mc -> unsafeMatrixCopyIO m mc >> return mc


matrixEqualsIO :: Matrix -> Matrix -> IO Bool
matrixEqualsIO m1 m2 = 
  withMatrix m1 $ \mm1 -> withMatrix m2 $ \mm2 ->
  fmap toBool $ goMatrixEquals mm1 mm2 


{-| Computes c <- beta * c + alpha * a(^T) * b(^T).
    That means c is either updated in-place, or created as needed. 
    No sanity checks are made. Non-matching shapes of a and b leads to undefined behaviour. -}
unsafeMatrixMultIO :: Double -> Matrix -> Trans -> Matrix -> Trans -> Double -> Matrix -> IO ()
unsafeMatrixMultIO alpha a atrans b btrans beta c =
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
  then error $ "Trying to multiply " ++ show (r,ac) ++ " by " ++ show (br,c) ++ " matrix."
  else
    matrixNew r c >>= \result -> 
    unsafeMatrixMultIO alpha a NoTrans b NoTrans 0 result >> return result


---------------------------------------------------------
-- Monadic matrix manipulation
---------------------------------------------------------


-- This type is not exported.
type MMonad = StateT Matrix IO

{-| Matrix modification monad. This is used for creating and modifying matrices efficiently. -}
newtype MMM s a = MMM { unMMM :: MMonad a } deriving (Monad, Functor)


-- Make a copy of the matrix, put it in the state, and let modification functions run on it.
-- Does /not/ allow anything else to be modified than the copy of the matrix that is given as argument.
runMMM :: Matrix -> MMM s a -> IO Matrix
runMMM mat m = matrixCopyIO mat >>= execStateT (unMMM m)


{-| Create a new matrix of given size and run the given modification action on it; then return
    The new matrix. -}
createMatrix :: Index -- ^ The number of rows
               -> Index -- ^ The number of columns 
               -> MMM s a -- ^ Modification action
               -> Matrix -- ^ Return value: The newly created matrix.
createMatrix r c m = unsafePerformIO $ matrixNew r c >>= execStateT (unMMM m)


{-| Modify the given matrix using the given modification action; return the modified matrix. -}
modifyMatrix :: Matrix -> MMM s a -> Matrix
modifyMatrix mat m = unsafePerformIO $ matrixCopyIO mat >>= execStateT (unMMM m)


getMatrix :: MMM s Matrix
getMatrix = MMM get


{-| Modification action: Set the value of the given element. Returns True on success, or False if the element is out of bounds. -}
setElem :: (Index,Index) -> Double -> MMM s Bool
setElem (i,j) a = MMM $ get >>= \m -> if inMatrixRange m (i,j)
                                     then liftIO (unsafeSetElemIO m (i,j) a) >> return True
                                     else return False


{-| Fills the matrix that is currently under modification with a given value. -}
fill :: Double -> MMM s ()
fill a = MMM $ get >>= \m -> liftIO $ withMatrix m (`goMatrixFill` a')
  where a' = realToFrac a


{-| Sets the diagonal with given index to the given values. Operates on the matrix that is currently under modification. -}
setDiag :: Index -- ^ Number of the diagonal. 0 Means the main diagonal, negative values mean lower diagonals, positive values mean upper diagonals.
          -> [Double] -- ^ The values of the diagonal. Only as many values as fit in the diagonal are used.
          -> MMM s ()  -- ^ Returns the action that sets the diagonal.
setDiag d as = MMM $ get >>= \m -> 
  let (r,c) = shape m 
      idxs  = diagIndices (r,c) d
  in
   case idxs of
     [] -> return ()
     ijs -> setDiag' m ijs as
     where
       setDiag' :: Matrix -> [(Index,Index)] -> [Double] -> MMonad ()
       setDiag' m ijs as = mapM_ (\(ij,e) -> liftIO $ unsafeSetElemIO m ij e) (zip ijs as)


{-| Sets elements in a matrix; caution: invalid indices are silently ommitted. -}
setElems :: [((Index,Index),Double)] -> MMM s ()
setElems = mapM_ (uncurry setElem)

setRow :: Index -> [Double] -> MMM s ()
setRow i as = fmap shape getMatrix >>= \(_,c) -> setElems (zip (zip [i,i..] [0..(c-1)]) as)


setColumn :: Index -> [Double] -> MMM s ()
setColumn i as = fmap shape getMatrix >>= \(r,_) -> setElems (zip (zip [0..(r-1)] [i,i..]) as)


{-| Get an element of the matrix currently under modification. -}
getElem :: (Index,Index) -> MMM s Double
getElem (i,j) = MMM $ get >>= \m -> liftIO (getElemIO m (i,j))

-- freezeMatrix :: (Integral i, Fractional a, MatrixClass i a mat) => mat -> MatrixST s mat
-- setElem :: (Integral i, Fractional a, MatrixClass i a mat) => mat -> (i,i) -> a -> MatrixST s ()
