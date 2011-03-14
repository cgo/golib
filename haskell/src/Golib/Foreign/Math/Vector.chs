{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving #-}

module Golib.Foreign.Math.Vector
( Vector,
  withVector,
  vectorNew,
  getElem',
  dot,
  equals,
  vectorSize,
  unsafeFill,
  unsafeCopy,
  unsafeScalarMult,
  unsafeVectorAdd,
  VMM,
  createVector,
  modifyVector,
  vectorAdd,
  getVector,
  getElem,
  setElem,
  setElems) where

import Golib.Foreign.Base
import Golib.Math.Base
import C2HS
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Control.Monad.State

#include "cpp/vector.h"

{# context lib="golib" #}

{# pointer *golib_vector as Vector foreign newtype #}
-- {# fun unsafe golib_matrix_new as matrixNew' {} -> `Ptr Matrix' id #}

{# fun unsafe golib_vector_new {`Int'} -> `Ptr Vector' id #}
{# fun pure golib_vector_size as vectorSize {withVector* `Vector'} -> `Int' fromIntegral #}
{# fun unsafe golib_vector_get_elem as unsafeGetElem {withVector* `Vector', fromIntegral `Int'} -> `Double' realToFrac #}
{# fun unsafe golib_vector_set_elem as unsafeSetElem {withVector* `Vector', fromIntegral `Int', realToFrac `Double'} -> `Bool' cToBool #}
{# fun unsafe golib_vector_fill as unsafeFill {withVector* `Vector', realToFrac `Double'} -> `()' #}
{# fun unsafe golib_vector_copy as unsafeCopy {withVector* `Vector', withVector* `Vector'} -> `()' #}
{# fun pure golib_vector_equals as equals {withVector* `Vector', withVector* `Vector'} -> `Bool' cToBool #}
{# fun unsafe golib_vector_dot as unsafeDot {withVector* `Vector', withVector* `Vector'} -> `Double' realToFrac #}
{# fun unsafe golib_vector_scalar_mult as unsafeScalarMult {withVector* `Vector', realToFrac `Double' } -> `()' #}
{-| Computes unsafeVectorAdd alpha y x computed y <- alpha * x + y. Note that y is overwritten, therefore this is highly unsafe. -}
{# fun unsafe golib_vector_add as unsafeVectorAdd {realToFrac `Double', withVector* `Vector', withVector* `Vector'} -> `Bool' cToBool #}

rangeCheck :: Vector -> Index -> Bool
rangeCheck v i = let i' = fromIntegral i 
                 in 
                  i' >= 0 && i' < vectorSize v 


dot :: Vector -> Vector -> Maybe Double
dot v1 v2 = if vectorSize v1 == vectorSize v2 
            then Just $ unsafePerformIO $ unsafeDot v1 v2
            else Nothing


getElem' :: Vector         -- ^ The vector
            -> Index        -- ^ The index of the element
            -> Maybe Double -- ^ Just a value, or Nothing on range violation.
getElem' v i = if rangeCheck v i 
               then Just $ unsafePerformIO $ unsafeGetElem v i 
               else Nothing


vectorNew :: Index -> IO Vector
vectorNew n = golib_vector_new (fromIntegral n) >>= \vp ->
  checkNullPtr vp >>= \nn ->
  if nn
  then error ("Vector: allocation error when allocating " ++ show n ++ "-vector!")
  else return () >>
  newForeignPtr golib_vector_finalizer vp >>= return . Vector

foreign import ccall "vector.h &golib_vector_destroy"
  golib_vector_finalizer :: FunPtr (Ptr Vector -> IO ())


--------------------------
-- Monadic vector manipulations

type VMMMonad a = StateT Vector IO a

newtype VMM s a = VMM { unVMM :: VMMMonad a } deriving Monad

runVMM :: Vector -> VMM s a -> IO a
runVMM v action = evalStateT action' v
  where
    action' = unVMM action

createVector :: Index -> VMM s a -> Vector
createVector n action = unsafePerformIO $ 
                        vectorNew n >>= \mv -> runVMM mv (action >> (VMM get))
    
getVector :: VMM s Vector
getVector = VMM get

modifyVector :: Vector -> VMM s a -> Vector
modifyVector v action = unsafePerformIO $
  vectorNew n >>= \nv -> 
  unsafeCopy v nv >> runVMM nv (action >> (VMM get))
  where
    n = vectorSize v
    
{-| Adds alpha * v to the current vector. -}
vectorAdd :: Double -> Vector -> VMM s ()
vectorAdd alpha x = VMM $ (get >>= \v -> liftIO $ unsafeVectorAdd alpha v x >>= \t ->
                            if t then return () else error "vectorAdd failed.")

{-| unsafeSetElem may fail gracefully,
therefore this method may or may not set the element, depending on a successful range check. -}
setElem :: Index -> Double -> VMM s ()
setElem i e = VMM $ (get >>= \v -> liftIO $ unsafeSetElem v i e >> return ()) 

setElems :: [(Index,Double)] -> VMM s ()
setElems ies = VMM $ (get >>= \v -> liftIO $ mapM_ (\(i,e) -> unsafeSetElem v i e) ies)

{-| Note: getElem' returns a Maybe. -}
getElem :: Index -> VMM s (Maybe Double)
getElem i = VMM $ (get >>= \v -> return $ getElem' v i)

-- foreign import ccall "vector.h 