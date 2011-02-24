module Main (main) where

import Golib.Foreign.Math.Matrix
import Foreign.C.Types

import Golib.Math.Matrix

forEachElem :: Matrix -> (CDouble -> IO b) -> IO [b]
forEachElem m f = do
  rows <- withMatrix m goMatrixRowCount
  cols <- withMatrix m goMatrixColCount
  es <- sequence [withMatrix m (\m -> goMatrixGetElem m x y) | x <- [0..(rows-1)], y <- [0..(cols-1)]]
  sequence $ map f es

testMult = m1 <**> m2 
  where
    m1 = matrix 10 20
    m2 = matrix 20 30
                                                                                               
main = do
  m <- matrixNew 10 20
  putStrLn "Hello, World!"
  m2 <- matrixNew 30 40 
  forEachElem m $ putStrLn . show
  
  let (r,c) = shape m2
  putStrLn $ "Shape: " ++ show r ++ "," ++ show c
  let r = rowCount m2
  let c = colCount m2
  putStrLn $ "Shape with *Count: " ++ show r ++ "," ++ show c
  
  let e = m2 ! (29,5)
  -- let ee = m2 Matrix.! (5,5)
  let m = matrix 10 20
  let ee = m ! (6,6)
  let ee2 = m ! (6,7)
  putStrLn $ "e = " ++ show e ++ ", ee = " ++ show ee ++ ", ee2 = " ++ show ee2
  
  let m2 = matrix (2^10) (2^10)
  -- putStrLn $ "element: " ++ show (m2 Matrix.! (2^11,10))
  
  -- Multiplication
  m <- matrixNew 10 20
  m2 <- matrixNew 20 30
  let m3 = m <**> m2
  let s = shape m3
  putStrLn $ "shape of m <**> m2: " ++ show s
  
  let s = shape testMult
  putStrLn $ "shape of testMult: " ++ show s
  putStrLn $ "testMult: " ++ show (toList testMult)
  
  --r <- matrixMapM (+1) m
  --putStrLn $ show r

  let m = matrix 10 10
  m2 <- runMMM m $ (setElem (1,1) 2 >> setElem (1,2) 3)
  putStrLn $ show (toLists m2)
  putStrLn $ show (toLists m)
