module Main (main) where

import Golib.Foreign.Math.Matrix
import Foreign.C.Types

import qualified Golib.Math.Matrix as Matrix
import Golib.Math.Matrix.IO
import Golib.Math.Matrix.Monadic

forEachElem :: GoMatrix -> (CDouble -> IO b) -> IO [b]
forEachElem m f = do
  rows <- withGoMatrix m goMatrixRowCount
  cols <- withGoMatrix m goMatrixColCount
  es <- sequence [withGoMatrix m (\m -> goMatrixGetElem m x y) | x <- [0..(rows-1)], y <- [0..(cols-1)]]
  sequence $ map f es

testMult = m1 Matrix.<**> m2 
  where
    m1 = Matrix.matrix 10 20
    m2 = Matrix.matrix 20 30
                                                                                               
main = do
  m <- matrixNew 10 20
  putStrLn "Hello, World!"
  m2 <- matrixNew 30 40 
  forEachElem m $ putStrLn . show
  
  (r,c) <- shape m2
  putStrLn $ "Shape: " ++ show r ++ "," ++ show c
  r <- rowCount m2
  c <- colCount m2
  putStrLn $ "Shape with *Count: " ++ show r ++ "," ++ show c
  
  e <- m2 ! (29,5)
  -- let ee = m2 Matrix.! (5,5)
  let m = Matrix.matrix 10 20
  let ee = m Matrix.! (6,6)
  let ee2 = m Matrix.! (6,7)
  putStrLn $ "e = " ++ show e ++ ", ee = " ++ show ee ++ ", ee2 = " ++ show ee2
  
  let m2 = Matrix.matrix (2^10) (2^10)
  -- putStrLn $ "element: " ++ show (m2 Matrix.! (2^11,10))
  
  -- Multiplication
  m <- matrixNew 10 20
  m2 <- matrixNew 20 30
  m3 <- m <**> m2
  s <- shape m3
  putStrLn $ "shape of m <**> m2: " ++ show s
  
  let s = Matrix.shape testMult
  putStrLn $ "shape of testMult: " ++ show s
  putStrLn $ "testMult: " ++ show (Matrix.toList testMult)
  
  --r <- matrixMapM (+1) m
  --putStrLn $ show r