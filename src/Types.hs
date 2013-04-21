{-# OPTIONS_GHC -funbox-strict-fields #-}
module Types where
import Data.Vector.Storable
import qualified Data.Vector as V
import Data.IORef
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

data Scene = Scene
    { models         :: !(IORef (V.Vector Model))
    }

data Model = Model
    { offsetR        :: !(IORef Vec)
    , rotationR      :: !(IORef Rot)
    , scaleR         :: !(IORef Vec)
    , offset         :: !Uniform
    , rotation       :: !Uniform
    , scale          :: !Uniform
    , vertices       :: !GLuint
    , normals        :: !GLuint
    , uvs            :: !GLuint
    , size           :: !GLsizei
    , linkedProgram  :: !Program
    }

type Shader     = GLuint
type ShaderType = GLenum
type Program    = GLuint

type Uniform = GLint

data ModelData = MD
    { mVertices      :: !(Vector GLfloat)
    , mVertexNormals :: !(Vector GLfloat)
    , mUVs           :: !(Vector GLfloat)
    } deriving Show
