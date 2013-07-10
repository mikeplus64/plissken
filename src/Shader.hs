{-# OPTIONS_GHC -funbox-strict-fields #-}
module Shader where
import Graphics.Rendering.OpenGL.Raw

import qualified Data.ByteString as B

import Foreign

import Uniform
import Util

data Shaders s = Shaders
    { program  :: !GLuint
    , uniforms :: !(Uniforms s)
    }

{-# INLINE runShaders #-}
runShaders :: Uploadable s r => Shaders s -> IO () -> r
runShaders (Shaders prog us) f = uploadUniforms us $ \ loadUniforms -> do
    glUseProgram prog
    loadUniforms
    f

{-# INLINE makeShaders #-}
makeShaders :: GetUniforms s 
            => FilePath -> FilePath 
            -> (Needs (IO ()) -> Needs s) 
            -> IO (Shaders s)
makeShaders v f n = do
    prog <- makeProgram v f
    us   <- getUniforms (n OK) prog
    return (Shaders prog us)

--------------------------------------------------------------------------------
--  Raw OpenGL shader loading

-- | Load a shader from a string
loadShader' :: FilePath -> GLenum -> IO GLuint
loadShader' src shad = do
    shader <- glCreateShader shad
    srcBS  <- B.readFile src
    srcBS `B.useAsCString` 
        \c -> c `with` 
        \s -> glShaderSource shader 1 s nullPtr
    glCompileShader shader
    return shader

-- | From a vertex and fragment shader, make a program
makeProgram :: FilePath -> FilePath -> IO GLuint
makeProgram v f = do
    vert <- loadShader' v gl_VERTEX_SHADER
    frag <- loadShader' f gl_FRAGMENT_SHADER
    prog <- linkProgram vert frag
    glLinkProgram prog
    printLog prog gl_COMPILE_STATUS glGetProgramiv glGetProgramInfoLog
    glDeleteShader vert
    glDeleteShader frag
    return prog

-- | Link the vertex and fragment shaders together
linkProgram :: GLuint -> GLuint -> IO GLuint
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


