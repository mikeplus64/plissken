{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Engine where
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import Graphics.Rendering.OpenGL.Raw
import Control.Applicative
import Control.Monad
import Data.IORef
import Foreign.C
import Foreign
import qualified Data.ByteString as B
import Util
import Types

import Linear

data Scene = Scene
    { models         :: !(IORef (V.Vector Model))
    }

newScene :: IO Scene
newScene = Scene <$> newIORef V.empty

drawScene :: Scene -> IO ()
drawScene s = do
    glClearColor 0 0 0 0
    glClear gl_COLOR_BUFFER_BIT
    glLoadIdentity
    readIORef (models s) >>= V.mapM_ drawModel

addModelToScene :: Scene -> Model -> IO ()
addModelToScene s m = modifyIORef' (models s) (V.cons m)

addModelsToScene :: Scene ->  V.Vector Model -> IO ()
addModelsToScene s m = modifyIORef' (models s) (V.++ m)
 
--------------------------------------------------------------------------------
--  Vectors

-- DEPRECATED
-- using hmatrix now, everything is roses

--------------------------------------------------------------------------------
--  Uniforms

type Uniform = GLint

newUniform :: CString -> Program -> IO Uniform
newUniform name prog = fromIntegral
    `fmap` glGetUniformLocation prog name

--------------------------------------------------------------------------------
--  Models

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

vertexAttribute :: GLuint
vertexAttribute = 0

tri :: GLSL -> GLSL -> IO Model
tri = newModel 
    (VS.fromList 
        [ 0.75 , 0.75 , 0.0
        , 0.75 , -0.75, 0.0
        , -0.75, -0.75, 0.0 ])
    VS.empty 
    VS.empty 
    VS.empty

newModel :: Vertices -> Normals -> UVs -> GLSL -> GLSL -> IO Model
newModel vert norm uv vshad fshad = do
    offsetR       <- newIORef (V3 0 0 0)                 -- offset
    rotationR     <- newIORef (Quaternion 0 (V3 0 0 0))  -- rotation
    scaleR        <- newIORef (V3 1 1 1)                 -- scale
    linkedProgram <- makeProgram vshad fshad
    offset        <- newUniform "offset" linkedProgram
    rotation      <- newUniform "rotation" linkedProgram
    scale         <- newUniform "scale" linkedProgram
    vertices      <- staticArray vert
    normals       <- staticArray norm
    uvs           <- staticArray uv
    let size = fromIntegral (sizeOf (0::GLfloat) * VS.length vert)
    return Model{..}

swapModelVecs :: Model -> IORef Vec -> IORef Rot -> IORef Vec -> Model
swapModelVecs m off rot scl = m{ offsetR = off, rotationR = rot, scaleR = scl }

uniformFromVec :: GLint -> Vec -> IO ()
uniformFromVec un (V3 x y z) = glUniform3f un x y z

uniformFromQuat :: GLint -> Rot -> IO ()
uniformFromQuat un (Quaternion w (V3 x y z)) = glUniform4f un x y z w

drawModel :: Model -> IO ()
drawModel m = do
    -- Read offset/rotation/scale data from model
    offsetv <- readIORef (offsetR m)
    rotatev <- readIORef (rotationR m)
    scalev  <- readIORef (scaleR m)

    glUseProgram (linkedProgram m)

    -- Upload uniforms to the shader
    uniformFromVec  (offset m)   offsetv
    uniformFromQuat (rotation m) rotatev
    uniformFromVec  (scale m)    scalev
    
    -- Vertice attribute buffer
    glEnableVertexAttribArray vertexAttribute
    glBindBuffer gl_ARRAY_BUFFER (vertices m)
    glVertexAttribPointer vertexAttribute 3 gl_FLOAT (fromIntegral gl_FALSE) 0 nullPtr
    glDrawArrays gl_TRIANGLES 0 (size m `div` 12)

    -- UV attribute buffer
    glEnableVertexAttribArray 

    -- Clean up
    glDisableVertexAttribArray vertexAttribute
    glBindBuffer gl_ARRAY_BUFFER 0
    glUseProgram 0

--------------------------------------------------------------------------------
--  Shaders

type Shader     = GLuint
type ShaderType = GLenum
type Program    = GLuint

-- | Load a shader from a file
loadShader :: FilePath -> ShaderType -> IO Shader
loadShader srcPath shad = do
    shader <- glCreateShader shad
    srcBS  <- B.readFile srcPath
    srcBS `B.useAsCString` 
        \c -> c `with` 
        \s -> glShaderSource shader 1 s nullPtr
    glCompileShader shader
    return shader

-- | From a vertex and fragment shader, make a program
makeProgram :: FilePath -> FilePath -> IO Program
makeProgram v f = do
    vert <- loadShader v gl_VERTEX_SHADER
    frag <- loadShader f gl_FRAGMENT_SHADER
    prog <- linkProgram vert frag
    printLog prog gl_LINK_STATUS glGetProgramiv glGetProgramInfoLog
    glDeleteShader vert
    glDeleteShader frag
    glLinkProgram prog
    return prog

linkProgram :: Shader -> Shader -> IO Program
linkProgram vert frag = do
    prog <- glCreateProgram
    glAttachShader prog vert
    glAttachShader prog frag
    glLinkProgram prog
    return prog

--------------------------------------------------------------------------------
--  Misc OpenGL functions

newBuffer :: Storable a => GLenum -> GLenum -> VS.Vector a -> IO GLuint
newBuffer target hint (buf :: VS.Vector a) = do
    gid <- alloca' (glGenBuffers 1)
    glBindBuffer target gid
    VS.unsafeWith buf (\ptr -> glBufferData target size ptr hint)
    glBindBuffer target 0
    return gid
  where
    size = fromIntegral (sizeOf (undefined :: a) * VS.length buf)

staticArray :: Storable a => VS.Vector a -> IO GLuint
staticArray = newBuffer gl_ARRAY_BUFFER gl_STATIC_DRAW

-- | Print the OpenGL compile log
printLog 
    :: GLuint 
    -> GLenum 
    -> (GLuint -> GLenum -> Ptr GLint -> IO ()) 
    -> (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) 
    -> IO ()
printLog gid from getLog getInfoLog = do
    result <- alloca' (getLog gid from)
    len    <- alloca' (getLog gid gl_INFO_LOG_LENGTH)
    log'   <- mallocArray (fromIntegral len)
    getInfoLog gid len nullPtr log'
    puts log'
    free log'

