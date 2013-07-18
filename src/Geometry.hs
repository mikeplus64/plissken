{-# LANGUAGE GADTs, BangPatterns, ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Geometry
    ( vec2, vec3, vec4
    , F, V, Vec, V2, V3, V4, M, I, I2, I3
    , normalize
    , cross
    , translation
    , perspective
    , lookAt
    , module X
    ) where

import Numeric.LinearAlgebra as X hiding (readMatrix, (|>), join, i)
import Data.Packed.ST as X hiding (join)
import Data.Packed.Foreign as X hiding (join)
import Data.Packed.Vector as X hiding (join)

type Vec = Vector

type F = Float

-- n indices
type I = Int
type I2 = (Int,Int)
type I3 = (Int,Int,Int)


type V = Vector F
type V2 = V
type V3 = V
type V4 = V
type M = Matrix F

vec2 :: F -> F -> V
vec2 x y = runSTVector $ do
    v <- newUndefinedVector 2
    unsafeWriteVector v 0 x
    unsafeWriteVector v 1 y
    return v
vec3 :: F -> F -> F -> V
vec3 x y z = runSTVector $ do
    v <- newUndefinedVector 3
    unsafeWriteVector v 0 x
    unsafeWriteVector v 1 y
    unsafeWriteVector v 2 z
    return v
vec4 :: F -> F -> F -> F -> V
vec4 x y z w = runSTVector $ do
    v <- newUndefinedVector 4
    unsafeWriteVector v 0 x
    unsafeWriteVector v 1 y
    unsafeWriteVector v 2 z
    unsafeWriteVector v 3 w
    return v

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

translation :: V -> M
translation v = runSTMatrix $ do
    matrix <- newMatrix 0 4 4
    writeMatrix matrix 0 0 1
    writeMatrix matrix 1 1 1
    writeMatrix matrix 2 2 1
    writeMatrix matrix 3 3 1
    writeMatrix matrix 0 3 $! atIndex v 0
    writeMatrix matrix 1 3 $! atIndex v 1
    writeMatrix matrix 2 3 $! atIndex v 2
    return matrix

rads :: F -> F
rads d = pi * d / 180

-- | Perspective given near-view clipping plane, far view clipping plane, the FOV and the aspect ratio
perspective :: F -> F -> F -> F -> M
perspective (rads -> fovY) aspect near far = runSTMatrix $ do
    matrix <- newMatrix 0 4 4
    writeMatrix matrix 0 0 $! 1/(aspect * t)
    writeMatrix matrix 1 1 $! 1/t
    writeMatrix matrix 2 2 $! -(far+near)/(far-near)
    writeMatrix matrix 3 2 $! -1
    writeMatrix matrix 2 3 $! - (2*far*near)/(far-near)
    return matrix
  where
    t = tan (fovY/2)

{-  matrix <- newMatrix 0 4 4
    writeMatrix matrix 0 0 $! 2*n/(r-l)
    writeMatrix matrix 0 2 $! -(r+l)/(r-l)
    writeMatrix matrix 1 1 $! 2*n/(t-b)
    writeMatrix matrix 1 2 $! (t+b)/(t-b)
    writeMatrix matrix 2 2 $! -(f+n)/(f-n)
    writeMatrix matrix 2 3 $! -2*f*n/(f-n)
    writeMatrix matrix 3 2 $! -1
    return matrix
    b = -t
    r = aspect * t
    l = -r
-}

{-# INLINE x #-}
{-# INLINE y #-}
{-# INLINE z #-}
{-# INLINE w #-}
x,y,z,w :: Container Vector e => Vector e -> e
x v = atIndex v 0
y v = atIndex v 1
z v = atIndex v 2
w v = atIndex v 3

-- | A 4x4 matrix given position of the camera, and its target
lookAt :: V -> V -> V -> M
lookAt eye center up = runSTMatrix $ do
    matrix <- newMatrix 0 4 4

    writeMatrix matrix 0 0  $!  x s
    writeMatrix matrix 0 1  $!  y s
    writeMatrix matrix 0 2  $!  z s
    writeMatrix matrix 1 0  $!  x u
    writeMatrix matrix 1 1  $!  y u
    writeMatrix matrix 1 2  $!  z u
    writeMatrix matrix 2 0  $! -x f
    writeMatrix matrix 2 1  $! -y f
    writeMatrix matrix 2 2  $! -z f
    writeMatrix matrix 0 3  $! -dot s eye
    writeMatrix matrix 1 3  $! -dot u eye
    writeMatrix matrix 2 3  $!  dot f eye
    writeMatrix matrix 3 3  $! 1

    return matrix
  where
    u' = normalize up
    f = normalize (center - eye)
    s = normalize (cross f u')
    u = cross s f

