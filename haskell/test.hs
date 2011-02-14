module Main (main) where

import Golib.Foreign.Math.Matrix
import Foreign.ForeignPtr
import Foreign.C.Types

import qualified Golib.Math.Matrix as Matrix
import Golib.Math.MMatrix

forEachElem :: ForeignPtr GoMatrix -> (CDouble -> IO b) -> IO [b]
forEachElem m f = do
  rows <- withForeignPtr m goMatrixRowCount
  cols <- withForeignPtr m goMatrixColCount
  es <- sequence [withForeignPtr m (\m -> goMatrixGetElem m x y) | x <- [0..(rows-1)], y <- [0..(cols-1)]]
  sequence $ map f es

                                                                                               
main = do
  m <- matrixNew 10 20
  putStrLn "Hello, World!"
  m2 <- matrixNew 30 40 
  rows <- withForeignPtr m goMatrixRowCount
  cols <- withForeignPtr m goMatrixColCount
  putStrLn $ "Matrix size: " ++ show rows ++ "," ++ show cols
  forEachElem m $ putStrLn . show
  let d = Matrix.Dummy 3
  putStrLn $ "Dummy shape: " ++ show (Matrix.shape d)
  
  (r,c) <- shape m2
  putStrLn $ "Shape: " ++ show r ++ "," ++ show c
  r <- rowCount m2
  c <- colCount m2
  putStrLn $ "Shape with *Count: " ++ show r ++ "," ++ show c
  return ()
  
