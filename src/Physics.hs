{-# LANGUAGE GADTs, BangPatterns #-}
module Physics where
import Graphics.Rendering.OpenGL.Raw
import Numeric.LinearAlgebra
import Data.Packed.ST

type F = Float
type V = Vector F
type M = Matrix F

vec3 :: F -> F -> F -> V
vec3 x y z = fromList [x,y,z]
vec4 :: F -> F -> F -> F -> V
vec4 x y z w = fromList [x,y,z,w]

normalize :: V -> V
normalize v = cmap (/ norm2 v) v

cross :: V -> V -> V
cross x y = runSTVector $ do
    new <- newUndefinedVector 3
    writeVector new 0 $! x2*y3 - x3*y2
    writeVector new 1 $! x3*y1 - x1*y3
    writeVector new 2 $! x1*y2 - x2*y1
    return new
  where
    !x1 = atIndex x 0
    !x2 = atIndex x 1
    !x3 = atIndex x 2
    !y1 = atIndex y 0
    !y2 = atIndex y 1
    !y3 = atIndex y 2

-- based on Vec's LinAlg.hs's perspective and rotationLookAt functions

-- | Perspective given near-view clipping plane, far view clipping plane, the FOV and the aspect ratio
perspective :: F -> F -> F -> F -> M
perspective n f fovY aspect = runSTMatrix $ do
    matrix <- newMatrix 0 4 4
    writeMatrix matrix 0 0 $! 2*n/(r-l)
    writeMatrix matrix 0 2 $! -(r+l)/(r-l)
    writeMatrix matrix 1 1 $! 2*n/(t-b)
    writeMatrix matrix 1 2 $! (t+b)/(t-b)
    writeMatrix matrix 2 2 $! -(f+n)/(f-n)
    writeMatrix matrix 2 3 $! -2*f*n/(f-n)
    writeMatrix matrix 3 2 $! -1
    return matrix
  where
    t = n*tan (fovY/2)
    b = -t
    r = aspect * t
    l = -r

upVector :: V
upVector = vec3 0 1 0

-- | A 4x4 matrix given position of the camera, and its target
lookAt :: V -> V -> M
lookAt p t = runSTMatrix $ do
    matrix <- newMatrix 0 4 4
    writeMatrix matrix 0 0 $! atIndex l 0
    writeMatrix matrix 1 0 $! atIndex l 1
    writeMatrix matrix 2 0 $! atIndex l 2
    writeMatrix matrix 0 1 $! atIndex u 0
    writeMatrix matrix 1 1 $! atIndex u 1
    writeMatrix matrix 2 1 $! atIndex u 2
    writeMatrix matrix 0 2 $! atIndex f 0
    writeMatrix matrix 1 2 $! atIndex f 1
    writeMatrix matrix 2 2 $! atIndex f 2
    writeMatrix matrix 3 3 1
    return matrix
  where
    !f  = normalize (p - t)
    !l  = normalize (upVector `cross` f)
    !u  = f `cross` l

