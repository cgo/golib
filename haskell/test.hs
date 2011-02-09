module Main (main) where

import Golib.Foreign.Math.Matrix
import Foreign.ForeignPtr
import Foreign.C.Types

import Golib.Math.Matrix

forEachElem :: ForeignPtr GoMatrix -> (CDouble -> IO b) -> IO [b]
forEachElem m f = do
  rows <- withForeignPtr m goMatrixRowCount
  cols <- withForeignPtr m goMatrixColCount
  es <- sequence [withForeignPtr m (\m -> goMatrixGetElem m x y) | x <- [0..(rows-1)], y <- [0..(cols-1)]]
  sequence $ map f es

                                                                                               
main = do
  m <- matrixNew
  putStrLn "Hello, World!"
  m2 <- matrixNew
  rows <- withForeignPtr m goMatrixRowCount
  cols <- withForeignPtr m goMatrixColCount
  putStrLn $ "Matrix size: " ++ show rows ++ "," ++ show cols
  forEachElem m $ putStrLn . show
  let d = Dummy 3
  putStrLn $ "Dummy shape: " ++ show (shape d)
  return ()
  
