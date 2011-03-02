module Main (main) where

import Golib.Math.Matrix

main = do
  let m = createMatrix 10 10 $ setDiag 0 [1,1..] >> setDiag 1 [2,2..] >> setDiag (-1) [-2,-2..]
  mapM_ (putStrLn . show) $ prettyPrintMatrix m