{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import Graphics.Rendering.OpenGL.Raw
import Control.Applicative
import Control.Monad
import Data.IORef
import Foreign.C
import Foreign
import qualified Data.ByteString as B

data Env = Env
    { thisModel :: !(IORef GLint)
    }

--------------------------------------------------------------------------------
--  Models

data Buffer = Buffer !GLenum !GLuint

data Model = Model
    { vertices    :: !GLuint
    , elements    :: !GLuint
    , normals     :: !GLuint
    , uvs         :: !GLuint
    , position    :: !(IORef (GLfloat, GLfloat, GLfloat))
    }

buffer' :: Storable a => V.Vector a -> IO GLuint
buffer' = buffer gl_ARRAY_BUFFER gl_STATIC_DRAW

model :: V.Vector GLfloat -> V.Vector GLfloat -> V.Vector GLfloat -> V.Vector GLfloat -> V.Vector GLuint -> IO Model
model vert elem norm uv tex
    = Model
        <$> buffer' vert
        <*> buffer' elem
        <*> buffer' norm
        <*> buffer' uv
        <*> newIORef (0,0,0)

renderModel :: Model -> Env -> IO ()
renderModel m e = do
    modifyIORef' (modelsInScene m) (+1)
    mods <- readIORef (modelsInScene e)
    glBindBuffer gl_ARRAY_BUFFER (vertices m)
    glVertexAttribPointer
        mods
        3
        gl_FLOAT
        (fromIntegral gl_FALSE)
        0
        nullPtr
    glBindBuffer gl_ELEMENT_ARRAY_BUFFER (elements m)
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

buffer :: Storable a => GLenum -> GLenum -> V.Vector a -> IO GLuint
buffer target hint (buf :: V.Vector a) = do
    gid <- point (glGenBuffers 1)
    glBindBuffer target gid
    V.unsafeWith buf (\ptr -> 
        glBufferData target size ptr hint)
    return gid
  where
    size = fromIntegral (sizeOf (undefined :: a) * V.length buf)

-- | Print the OpenGL compile log
printLog 
    :: GLuint 
    -> GLenum 
    -> (GLuint -> GLenum -> Ptr GLint -> IO ()) 
    -> (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ()) 
    -> IO ()
printLog gid from getLog getInfoLog = do
    result <- point (getLog gid from)
    len    <- point (getLog gid gl_INFO_LOG_LENGTH)
    log'   <- mallocArray (fromIntegral len)
    getInfoLog gid len nullPtr log'
    puts log'
    free log'

--------------------------------------------------------------------------------
--  Misc helper functions

foreign import ccall "stdio.h puts" puts :: CString -> IO ()

-- | Temporarily allocate a pointer for some function, read it, then 
-- free the pointer, but return the read value.
point :: Storable a => (Ptr a -> IO b) -> IO a
point f = do
    x' <- malloc
    f x'
    x <- peek x'
    free x'
    return x

