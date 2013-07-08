{-# OPTIONS_GHC -funbox-strict-fields #-}
module Shader where
import Graphics.Rendering.OpenGL.Raw

import qualified Data.ByteString as B

import Foreign.C
import Foreign

import Geometry
import Uniform
import Util

data Shader s = Shader
    { program  :: !GLuint
    , uniforms :: !(Uniforms s)
    }


--------------------------------------------------------------------------------
--  Raw OpenGL shader loading/

loadShaderFile :: FilePath -> GLenum -> IO GLuint
loadShaderFile src shad = do
    srcBS <- B.readFile src
    loadShader srcBS shad

-- | Load a shader from a string
loadShader :: B.ByteString -> GLenum -> IO GLuint
loadShader src shad = do
    shader <- glCreateShader shad
    src `B.useAsCString` 
        \c -> c `with` 
        \s -> glShaderSource shader 1 s nullPtr
    glCompileShader shader
    return shader

-- | From a vertex and fragment shader, make a program
makeProgram :: FilePath -> FilePath -> IO GLuint
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


