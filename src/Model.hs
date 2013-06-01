{-# LANGUAGE TemplateHaskell, FunctionalDependencies, FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Model where
import OBJ

import qualified Data.Vector        as V
import qualified Data.DList         as DL

import qualified Data.Vector.Storable as S

import Graphics.Rendering.OpenGL.Raw

import Control.Monad.State.Strict
import Control.Lens
import Linear

-- | A face is a triangle, defined by indices in _vertices, _normals and/or _uvs
-- When a tuple is used, the order of indices is the same as the order of the compounded
-- words for the constructor name.
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
    { _vertices :: !(f (V3 P))
    , _normals  :: !(f (V3 P))
    , _uvs      :: !(f (V2 P))
    , _faces    :: !(f Face)
    }

makeLenses ''ModelT

-- | 'Model' is suitable for actual use -- it's underlying type is 'V.Vector'
type Model   = ModelT V.Vector
type Hull    = ModelT V.Vector

data Renderable = Renderable
    { rVerts  :: !(S.Vector GLfloat)
    , rNorms  :: !(S.Vector GLfloat)
    , rUVs    :: !(S.Vector GLfloat)
    , rFaces  :: !(S.Vector GLushort)
    }

--------------------------------------------------------------------------------
--  Building models from .obj

switchModelType :: (forall a. f a -> g a) -> ModelT f -> ModelT g
switchModelType morph (Model v n u f) = Model (morph v) (morph n) (morph u) (morph f)

-- | 'Builder' is an efficient type used to /build/ a mesh from a list of .obj commands
type Builder = State (ModelT DL.DList) ()

emptyDListModel :: ModelT DL.DList
emptyDListModel = Model DL.empty DL.empty DL.empty DL.empty

addObjCommand :: ObjCommand -> Builder
addObjCommand obj = case obj of
    V    x y z -> addTo vertices (V3 x y z)
    VN   x y z -> addTo normals  (V3 x y z)
    VT   x y   -> addTo uvs      (V2 x y)
    FV   a b c -> addTo faces    (Verts a b c)
    FVT  a b c -> addTo faces    (VertTex a b c)
    FVN  a b c -> addTo faces    (VertNorm a b c)
    FVTN a b c -> addTo faces    (VertTexNorm a b c)
    _          -> return ()
  where
    addTo :: Lens' (ModelT DL.DList) (DL.DList a) -> a -> Builder
    addTo label a = label %= (`DL.snoc` a)

buildFromObj :: OBJ -> Model
buildFromObj obj = switchModelType 
    (V.fromList . DL.toList) 
    (execState (mapM_ addObjCommand obj) emptyDListModel)


--------------------------------------------------------------------------------
-- OpenGL stuff
--

openGLModel :: Model -> Renderable
openGLModel (Model v n u f)
    = Renderable
        (convertBy v3ToGLfloats v)
        (convertBy v3ToGLfloats n)
        (convertBy v2ToGLfloats u)
        (convertBy faceToGLushort f)
  where
    convertBy :: S.Storable b => (a -> V.Vector b) -> V.Vector a -> S.Vector b
    convertBy through xs = V.convert (V.concatMap through xs)

    v3ToGLfloats   (V3 x y z)   = V.map realToFrac (V.fromList [x,y,z])
    v2ToGLfloats   (V2 x y)     = V.map realToFrac (V.fromList [x,y])

    faceToGLushort (Verts i j k)                         = V.map fromIntegral (V.fromList [i,j,k])
    faceToGLushort (VertTex (i,_) (j,_) (k,_))           = V.map fromIntegral (V.fromList [i,j,k])
    faceToGLushort (VertNorm (i,_) (j,_) (k,_))          = V.map fromIntegral (V.fromList [i,j,k])
    faceToGLushort (VertTexNorm (i,_,_) (j,_,_) (k,_,_)) = V.map fromIntegral (V.fromList [i,j,k])
