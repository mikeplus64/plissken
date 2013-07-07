{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, RecordWildCards, BangPatterns, NamedFieldPuns, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Engine where
import qualified Data.Vector.Storable as S
import Graphics.Rendering.OpenGL.Raw
import Foreign.C
import Foreign
import qualified Data.ByteString as B

import Data.Packed.Foreign

import Util
import Model
import Geometry

clear :: IO ()
clear = glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

frameInit :: IO ()
frameInit = do
    glPolygonMode gl_FRONT_AND_BACK gl_LINE
    
--------------------------------------------------------------------------------
--  Vectors
-- using linear now, everything is roses

--------------------------------------------------------------------------------
--  Uniforms

type Uniform = GLint

newUniform :: CString -> Program -> IO Uniform
newUniform name prog = fromIntegral
    `fmap` glGetUniformLocation prog name

--------------------------------------------------------------------------------
--  Models

{-# INLINE loadModel #-}
loadModel :: FilePath -> FilePath -> FilePath -> IO GLmodel
loadModel model vshad fshad = do
    mesh <- loadMesh model
    meshToGL mesh vshad fshad

vertexAttribute, uvAttribute, normalAttribute :: GLuint
vertexAttribute = 0
uvAttribute     = 1
normalAttribute = 2

data GLmodel = GLmodel
    { vertArray :: !GLuint
    , normArray :: !GLuint
    , uvArray   :: !GLuint
    , ixArray   :: !GLuint
    , program   :: !Program
    , mvpU      :: !Uniform
    , arrSize   :: !GLsizei
    }

meshToGL :: Mesh -> FilePath -> FilePath -> IO GLmodel
meshToGL (Mesh v n u f) vshad fshad = newGLmodel (S.map realToFrac v) (S.map realToFrac n) (S.map realToFrac u) f vshad fshad

-- | 'newGLmodel' vertices normals uvs indices vertexShader fragmentShader
{-# INLINE newGLmodel #-}
newGLmodel :: S.Vector GLfloat 
           -> S.Vector GLfloat 
           -> S.Vector GLfloat 
           -> S.Vector GLushort 
           -> FilePath -> FilePath -> IO GLmodel
newGLmodel !vert !norm !uv !elems !vshad !fshad = do
    program       <- makeProgram vshad fshad
    mvpU          <- newUniform "mvp" program
    vertArray     <- staticArray vert
    normArray     <- staticArray norm
    uvArray       <- staticArray uv
    ixArray       <- staticElementArray elems
    return GLmodel{ arrSize = fromIntegral (S.length elems), .. }

drawModel :: GLmodel -> Matrix Float -> IO ()
drawModel GLmodel{..} !mvp = do
    glUseProgram program
    glUniformMatrix4fv mvpU 1 (fromIntegral gl_TRUE) . castPtr `appMatrix` mvp

    -- Vertice attribute buffer
    glEnableVertexAttribArray vertexAttribute
    glBindBuffer gl_ARRAY_BUFFER vertArray
    glVertexAttribPointer
        vertexAttribute 
        3 
        gl_FLOAT 
        0
        0 
        nullPtr

    -- UV attribute buffer
    glEnableVertexAttribArray uvAttribute
    glBindBuffer gl_ARRAY_BUFFER uvArray
    glVertexAttribPointer
        uvAttribute
        2
        gl_FLOAT
        0
        0
        nullPtr

    -- Normal attribute buffer
    glEnableVertexAttribArray normalAttribute
    glBindBuffer gl_ARRAY_BUFFER normArray
    glVertexAttribPointer
        normalAttribute
        3
        gl_FLOAT
        0
        0
        nullPtr

    -- Vertex indices
    glBindBuffer gl_ELEMENT_ARRAY_BUFFER ixArray
    glDrawElements gl_TRIANGLES arrSize gl_UNSIGNED_SHORT 0

    -- Clean up
    glDisableVertexAttribArray vertexAttribute
    glDisableVertexAttribArray uvAttribute
    glDisableVertexAttribArray normalAttribute
    glUseProgram 0

--------------------------------------------------------------------------------
--  Shaders
--


-- | Convenient aliases
type Shader  = GLuint
type Program = GLuint
type ShaderType = GLenum

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
    -- printLog prog gl_LINK_STATUS glGetProgramiv glGetProgramInfoLog
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
    _result <- alloca' (getLog gid from)
    len     <- alloca' (getLog gid gl_INFO_LOG_LENGTH)
    log'    <- mallocArray (fromIntegral len)
    getInfoLog gid len nullPtr log'
    puts log'

--------------------------------------------------------------------------------
--  Buffer creation

newBuffer :: Storable a => GLenum -> GLenum -> S.Vector a -> IO GLuint
newBuffer target hint (buf :: S.Vector a) = do
    gid <- alloca' (glGenBuffers 1)
    glBindBuffer target gid
    S.unsafeWith buf (\ptr -> glBufferData target size ptr hint)
    glBindBuffer target 0
    return gid
  where
    size = fromIntegral (sizeOf (undefined :: a) * S.length buf)

staticArray :: Storable a => S.Vector a -> IO GLuint
staticArray = newBuffer gl_ARRAY_BUFFER gl_STATIC_DRAW

staticElementArray :: Storable a => S.Vector a -> IO GLuint
staticElementArray = newBuffer gl_ELEMENT_ARRAY_BUFFER gl_STATIC_DRAW


