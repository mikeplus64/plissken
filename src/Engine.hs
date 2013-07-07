{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, RecordWildCards, BangPatterns, NamedFieldPuns, ForeignFunctionInterface #-}
{-# LANGUAGE GADTs, TypeFamilies, KindSignatures, DataKinds, PolyKinds, TypeOperators, EmptyDataDecls #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Engine where
import qualified Data.Vector.Storable as S
import Graphics.Rendering.OpenGL.Raw
import Foreign.C
import Foreign
import qualified Data.ByteString as B
import Control.Monad

import Data.Packed.Foreign
import GHC.TypeLits

import Util
import Model
import Geometry
import Uniform

clear :: IO ()
clear = glClear (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

--------------------------------------------------------------------------------
--  Models

{-# INLINE loadModel #-}
loadModel :: FilePath -> FilePath -> FilePath 
          -> (Program -> IO (Uniforms s))
          -> IO (GLmodel s)
loadModel model vshad fshad mk = do
    mesh <- loadMesh model
    meshToGL mesh mk vshad fshad

vertexAttribute, uvAttribute, normalAttribute :: GLuint
vertexAttribute = 0
uvAttribute     = 1
normalAttribute = 2

data GLmodel s = GLmodel
    { vertArray :: !GLuint
    , normArray :: !GLuint
    , uvArray   :: !GLuint
    , ixArray   :: !GLuint
    , program   :: !Program
    , uniforms  :: !(Uniforms s)
    , arrSize   :: !GLsizei
    }

{-# INLINE meshToGL #-}
meshToGL :: Mesh -> (Program -> IO (Uniforms s)) -> FilePath -> FilePath -> IO (GLmodel s)
meshToGL (Mesh v n u f) = newGLmodel v' n' u' f
  where
    v' = S.map realToFrac v
    n' = S.map realToFrac n
    u' = S.map realToFrac u

-- | 'newGLmodel' vertices normals uvs indices vertexShader fragmentShader
{-# INLINE newGLmodel #-}
newGLmodel :: S.Vector GLfloat 
           -> S.Vector GLfloat 
           -> S.Vector GLfloat 
           -> S.Vector GLushort 
           -> (Program -> IO (Uniforms s))
           -> FilePath -> FilePath -> IO (GLmodel s)
newGLmodel !vert !norm !uv !elems !mkUniforms !vshad !fshad = do
    program       <- makeProgram vshad fshad
    uniforms      <- mkUniforms program
    vertArray     <- staticArray vert
    normArray     <- staticArray norm
    uvArray       <- staticArray uv
    ixArray       <- staticElementArray elems
    return GLmodel{ arrSize = fromIntegral (S.length elems), .. }

{-# INLINE drawModel #-}
drawModel :: Upload s => GLmodel s -> s
drawModel GLmodel{..} = uploadUniforms uniforms $ \ glUniforms -> do
    glUseProgram program
    glUniforms

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


