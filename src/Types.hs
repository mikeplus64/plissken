{-# LANGUAGE BangPatterns, TypeOperators, TypeFamilies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Types where
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector
import Data.Vector.Storable
import Data.Word
import Data.IORef
import Data.Fixed
import Linear
import Graphics.Rendering.OpenGL.Raw

type Radius = Double

type Vec = V3 GLfloat
type Rot = Quaternion GLfloat

-- Flat arrays of GLfloat
-- Assumptions made:
--  - a vertex is 3 GLfloats (x,y,z)
--  - a normal is 3 GLfloats
--  - a UV is 2 GLshorts (x,y)
--  - UVs and vertex indices "align"
type Vertices = Vector GLfloat
type Elements = Vector GLfloat
type Normals  = Vector GLfloat
type UVs      = Vector GLfloat
type GLSL     = FilePath

data Danger
    = Deflect
    | Lava
  deriving (Eq, Ord, Show, Read)

data Block 
    = Empty
    | Wall
    | Danger !Danger
    | Spawn
  deriving (Eq, Ord, Show, Read)
