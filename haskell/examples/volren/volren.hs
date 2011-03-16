{-# LANGUAGE PackageImports #-}

module Main (main, defaultCamera, cameraMatrix, calibrationMatrix, rot, rodrigues, ray, imageRays) where

import Golib.Math.Matrix as M
import Golib.Math.Vector as V

import Golib.Math.Matrix (Matrix)
import Golib.Math.Vector (Vector)

import Ix
import "ieee754" Data.AEq
import Data.Maybe

data Camera e = Camera { camFocalLength :: e,
                         camYScale :: e, -- == my, mx is considered "1".
                         camPrincipalPoint :: (e,e),
                         camSkew :: e,
                         camCentre :: (e,e,e) } deriving (Eq, Show)

defaultCamera :: Camera Double
defaultCamera = Camera 1.0 1.0 (0,0) 0 (0,0,-1)

-- Generate the projective matrix taking homogeneous world coordinates to an image plane.
cameraMatrix :: Camera Double -> Matrix
cameraMatrix (Camera f my (px,py) s (cx,cy,cz)) = 
  createMatrix 3 4 $ do
    M.setElems [(ij, kr M.! ij) | ij <- range ((0,0),(2,2))]
    M.setColumn 3 (V.toList ((-1) V.*> (kr #| c)))
      where
        r = createMatrix 3 3 $ setDiag 0 [1,1..] -- Rotation matrix
        k = calibrationMatrix f (1, my) s (px, py) -- calibration matrix
        kr = k <**> r
        c = V.fromList [cx,cy,cz]


calibrationMatrix :: Double -> (Double,Double) -> Double -> (Double,Double) -> Matrix
calibrationMatrix f (mx, my) s (px, py) = M.createMatrix 3 3 $ 
                                          M.fill 0 >>
                                          M.setElem (0,0) (f * mx) >> 
                                          M.setElem (1,1) (f * my) >>
                                          M.setElem (0,1) s >>
                                          M.setColumn 2 [px, py, 1]
  

rot :: (AEq e, Floating e, Real e) => (e,e,e) -> e -> Matrix
rot (x,y,z) theta = rodrigues s (realToFrac theta)
  where
    s = fromJust $ M.fromList 3 3 [0, -z', y',                  
                                   z',  0, -x',
                                   -y', x', 0]
    (x',y',z') = (realToFrac (x/l),realToFrac (y/l),realToFrac (z/l))
    l' = sqrt (x*x + y*y + z*z)
    l = case (l' ~== 0) of  -- from AEq module, means "approximately equal".
      True -> 1
      False -> l'
    
    
-- The Rodrigues formula is the exponential map for tangent vectors in the tangent space of 
-- the space of rotations. The tangent vectors are rotations at the origin (i.e. the identity matrix),
-- and are all skew-symmetric matrices (see function rot).
-- rodrigues :: (Floating e, BLAS3 e, IMatrix a e) => a (n,p) e -> e -> Matrix (n,p) e
rodrigues s theta = i M.<+> ((sin theta) M.*> s) M.<+> ((1 - (cos theta)) M.*> (s M.<**> s))
  where i = M.createMatrix 3 3 $ M.fill 0 >> M.setDiag 0 [1,1..]


ray :: Matrix   -- ^ Pseudo-inverse of the camera matrix, backprojecting image points to rays.
      -> Vector -- ^ Camera centre
      -> Double -- ^ Image x coordinate
      -> Double -- ^ Image y coordinate
      -> (Double -> Vector)  -- ^ Returns a function, the parametrised ray in homogeneous coordinates.
ray p' c xi yi = \l -> l V.*> p'x V.<+> (1-l) V.*> c
  where x = V.fromList [xi, yi, 1]
        p'x = let temp = p' #| x 
                  normed = (1 / temp V.! 3) V.*> temp
              in normed


imageRays :: [Double -> Vector]
imageRays = [ray p' c xi yi | xi <- [0.0], yi <- [-1.0,0.0,1.0]]
  where 
    cam = defaultCamera
    c = V.fromList [a,b,c,1] where (a,b,c) = camCentre cam
    p = cameraMatrix cam
    p' = fromJust $ pseudoInverse p


main = M.prettyPrintMatrixIO $ cameraMatrix defaultCamera