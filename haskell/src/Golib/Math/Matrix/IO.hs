{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts  #-}
module Golib.Math.Matrix.IO
       (
         rowCountIO,
         colCountIO,
         unsafeGetElemIO,
         getElemIO,
         unsafeSetElemIO,
         unsafeMatrixMultIO,
         unsafeTransposeIO,
         unsafeFillIO,
         unsafeMatrixCopyIO,
         matrixCopyIO,
         matrixMultIO,
         Matrix,
         matrixNew
       ) where


-- deprecated
-- instance MMatrixClass Int Double Matrix IO where
--   rowCount = rowCountIO
--   colCount = colCountIO
--   m ! (r,c) = getElemIO m (r,c)
--   a *> mat = return mat
--   mat1 <**> mat2 = matrixMultIO 1 mat1 NoTrans mat2 NoTrans 
  


