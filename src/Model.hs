{-# LANGUAGE TemplateHaskell, FunctionalDependencies, FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Model where

import qualified Data.DList         as DL

import qualified Data.Vector.Storable as S

import Graphics.Rendering.OpenGL.Raw

import Control.Monad.State.Strict
import Control.Lens

import Geometry
import OBJ

-- | A face is a triangle, defined by indices in _vertices, _normals and/or _uvs
-- When a tuple is used, the order of indices is the same as the order of the compounded
-- words for the constructor name.
--
-- As per .obj, indices start at 1, and /NOT/ 1.
data Face
    -- | "f a b c d" in a .obj, corresponds to FV
    = Verts       !I       !I       !I
    -- | "f a/a b/b c/c d/d" in a .obj, corresponds to FVT
    | VertTex     !(I,I)   !(I,I)   !(I,I)
    -- | "f a//a b//b c//c d//d" in a .obj, corresponds to FVN
    | VertNorm    !(I,I)   !(I,I)   !(I,I)
    -- | "f a/a/a b/b/b c/c/c d/d/d" in a .obj, corresponds to FVTN
    | VertTexNorm !(I,I,I) !(I,I,I) !(I,I,I)

-- | Generic 'Model' type. 
data ModelT f = Model
    { _mVertices :: !(f V3)
    , _mNormals  :: !(f V3)
    , _mUvs      :: !(f V2)
    , _mFaces    :: !(f Face)
    }

data Mesh = Mesh
    { _gVerts   :: !V
    , _gNorms   :: !V
    , _gUvs     :: !V
    , _gFaces   :: !(Vector GLushort)
    } deriving (Eq,Show,Read)

makeFields ''ModelT
makeFields ''Mesh

--------------------------------------------------------------------------------
--  Building models from .obj

-- | 'Builder' is an efficient type used to /build/ a mesh from a list of .obj commands
type Builder = State (ModelT DL.DList) ()
type Built = ModelT []

emptyDListModel :: ModelT DL.DList
emptyDListModel = Model DL.empty DL.empty DL.empty DL.empty

addObjCommand :: ObjCommand -> Builder
addObjCommand obj = case obj of
    V    x y z -> addTo vertices (vec3 x y z)
    VN   x y z -> addTo normals  (vec3 x y z)
    VT   x y   -> addTo uvs      (vec2 x y)
    FV   a b c -> addTo faces    (Verts a b c)
    FVT  a b c -> addTo faces    (VertTex a b c)
    FVN  a b c -> addTo faces    (VertNorm a b c)
    FVTN a b c -> addTo faces    (VertTexNorm a b c)
    _          -> return ()
  where
    addTo :: Lens' (ModelT DL.DList) (DL.DList a) -> a -> Builder
    addTo label a = label %= (`DL.snoc` a)

runBuilder :: Builder -> Built
runBuilder b = case execState b emptyDListModel of
    Model v n u f -> Model (DL.toList v) (DL.toList n) (DL.toList u) (DL.toList f)

faceToIndices :: Face -> [GLushort]
faceToIndices = map (subtract 1 . fromIntegral) . toIndices
  where
    toIndices (Verts x y z)                         = [x,y,z]
    toIndices (VertTex (x,_) (y,_) (z,_))           = [x,y,z]
    toIndices (VertNorm (x,_) (y,_) (z,_))          = [x,y,z]
    toIndices (VertTexNorm (x,_,_) (y,_,_) (z,_,_)) = [x,y,z]

builtToMesh :: Built -> Mesh
builtToMesh (Model v n u f) = Mesh
    (S.concat v)
    (S.concat n)
    (S.concat u)
    (S.fromList (concatMap faceToIndices f))

loadMesh :: FilePath -> IO Mesh
loadMesh path = do
    Right obj <- readObj path
    return $! builtToMesh . runBuilder $! forM_ obj addObjCommand

