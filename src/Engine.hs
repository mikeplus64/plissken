{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, RecordWildCards, BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Engine where
import qualified Data.Vector.Storable as VS
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw
import Control.Applicative
import Control.Monad
import Data.IORef
import Foreign.C
import Foreign
import qualified Data.ByteString as B
import Data.Bits

import Linear hiding (norm)

import Util
import Model

newScene :: [Model] -> [Block] -> IO Scene
newScene ms bs 
    = Scene <$> pure (V.fromList ms) 
            <*> V.unsafeThaw (V.fromList bs)

drawScene :: Scene -> IO ()
drawScene s = do
    glClearColor 0 0 0 0
    glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)
    glEnable gl_DEPTH_TEST
    glDepthFunc gl_LESS
    glLoadIdentity
    gluPerspective 90 (4/3) 1 100
    forIOV (blocks s) $ \ b -> 
        draw (models s V.! fromEnum (blockType b))
    
--------------------------------------------------------------------------------
--  Vectors
-- using hmatrix now, everything is roses

--------------------------------------------------------------------------------
--  Uniforms

newUniform :: CString -> Program -> IO Uniform
newUniform name prog = fromIntegral
    `fmap` glGetUniformLocation prog name

--------------------------------------------------------------------------------
--  Models

vertexAttribute, uvAttribute, normalAttribute :: GLuint
vertexAttribute = 0
uvAttribute     = 1
normalAttribute = 2

newModel :: Vertices -> Normals -> UVs -> Elements -> GLSL -> GLSL -> IO Model
newModel vert norm uv elems vshad fshad = do
    moffset       <- newIORef (V3 0 0 0)                 -- offset
    mrotation     <- newIORef (Quaternion 0 (V3 0 0 0))  -- rotation
    mscale        <- newIORef (V3 1 1 1)                 -- scale

    linkedProgram <- makeProgram vshad fshad
    offset        <- newUniform "offset" linkedProgram
    rotation      <- newUniform "rotation" linkedProgram
    scale         <- newUniform "scale" linkedProgram
    vertices      <- staticArray vert
    normals       <- staticArray norm
    uvs           <- staticArray uv
    elements      <- staticElementArray elems
    let !size = 2*fromIntegral (VS.length vert)
        !draw = do
            -- Read offset/rotation/scale data from model
            offsetv <- readIORef moffset
            rotatev <- readIORef mrotation
            scalev  <- readIORef mscale

            glUseProgram linkedProgram

            -- Upload uniforms to the shader
            uniformFromVec  offset   offsetv
            uniformFromQuat rotation rotatev
            uniformFromVec  scale    scalev
            
            -- Vertice attribute buffer
            glEnableVertexAttribArray vertexAttribute
            glBindBuffer gl_ARRAY_BUFFER vertices
            glVertexAttribPointer
                vertexAttribute 
                3 
                gl_FLOAT 
                (fromIntegral gl_FALSE)
                0 
                0

            -- UV attribute buffer
            glEnableVertexAttribArray uvAttribute
            glBindBuffer gl_ARRAY_BUFFER uvs
            glVertexAttribPointer
                uvAttribute
                2
                gl_FLOAT
                (fromIntegral gl_FALSE)
                0
                0

            glEnableVertexAttribArray normalAttribute
            glBindBuffer gl_ARRAY_BUFFER normals
            glVertexAttribPointer
                normalAttribute
                3
                gl_FLOAT
                (fromIntegral gl_FALSE)
                0
                0

            -- Vertex indices
            glBindBuffer gl_ELEMENT_ARRAY_BUFFER elements

            glDrawElements gl_TRIANGLES size gl_UNSIGNED_SHORT 0
            
            {-
            glDrawArrays gl_TRIANGLES 0 (size m `div` 12)
            -}

            -- Clean up
            glDisableVertexAttribArray vertexAttribute
            glDisableVertexAttribArray uvAttribute
            glUseProgram 0
    return Model{..}

mdToModel :: ModelData -> FilePath -> FilePath -> IO Model
mdToModel MD{..} = newModel mVertices mVertexNormals mUVs mIndices

swapModelVecs :: Model -> IORef Vec -> IORef Rot -> IORef Vec -> Model
swapModelVecs m off rot scl = m{ moffset = off, mrotation = rot, mscale = scl }

uniformFromVec :: GLint -> Vec -> IO ()
uniformFromVec un (V3 x y z) = glUniform3f un x y z

uniformFromQuat :: GLint -> Rot -> IO ()
uniformFromQuat un (Quaternion w (V3 x y z)) = glUniform4f un x y z w

drawModel :: Model -> IO ()
drawModel (Model _ _ _ d) = d

--------------------------------------------------------------------------------
--  Shaders
--

-- | Convenient aliases
type Shader  = GLuint
type Program = GLuint

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
    glLinkProgram prog
    printLog prog gl_COMPILE_STATUS glGetProgramiv glGetProgramInfoLog
    printLog prog gl_LINK_STATUS glGetProgramiv glGetProgramInfoLog
    glDeleteShader vert
    glDeleteShader frag
    return prog

-- | Link the vertex and fragment shaders together
linkProgram :: Shader -> Shader -> IO Program
linkProgram vert frag = do
    prog <- glCreateProgram
    glAttachShader prog vert
    glAttachShader prog frag
    return prog

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

--------------------------------------------------------------------------------
--  Buffer creation

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

staticElementArray :: Storable a => VS.Vector a -> IO GLuint
staticElementArray = newBuffer gl_ELEMENT_ARRAY_BUFFER gl_STATIC_DRAW


