{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, RecordWildCards, BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Engine where
import qualified Data.Vector.Storable as S
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
import qualified OBJ
import qualified Model as M

{-
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

    -}

type Uniform = GLint

newUniform :: CString -> Program -> IO Uniform
newUniform name prog = fromIntegral
    `fmap` glGetUniformLocation prog name

--------------------------------------------------------------------------------
--  Models

data GLmodel = GLmodel
    { setModelOffset :: !(V3 GLfloat -> IO ())
    , setModelRotate :: !(Quaternion GLfloat -> IO ())
    , setModelScale  :: !(V3 GLfloat -> IO ())
    , drawModel      :: !(IO ())
    }

{-# INLINE loadModel #-}
loadModel :: FilePath -> FilePath -> FilePath -> IO GLmodel
loadModel model vshad fshad = do
    Right obj <- OBJ.readObj model
    fromModel (M.buildFromObj obj) vshad fshad

vertexAttribute, uvAttribute, normalAttribute :: GLuint
vertexAttribute = 0
uvAttribute     = 1
normalAttribute = 2

{-# INLINE fromModel #-}
fromModel :: M.Model -> FilePath -> FilePath -> IO GLmodel
fromModel (M.Model v n u f) = 
    newGLmodel (convertBy v3ToGLfloats v)
               (convertBy v3ToGLfloats n)
               (convertBy v2ToGLfloats u)
               (convertBy faceToGLushort f)
  where
    convertBy :: S.Storable b => (a -> V.Vector b) -> V.Vector a -> S.Vector b
    convertBy through xs = V.convert (V.concatMap through xs)

    v3ToGLfloats :: V3 GLfloat -> V.Vector GLfloat
    v3ToGLfloats   (V3 x y z) = V.map realToFrac (V.fromList [x,y,z])

    v2ToGLfloats :: V2 GLfloat -> V.Vector GLfloat
    v2ToGLfloats   (V2 x y) = V.map realToFrac (V.fromList [x,y])

    faceToGLushort :: M.Face -> V.Vector GLushort
    faceToGLushort (M.Verts i j k)                         = V.map fromIntegral (V.fromList [i,j,k])
    faceToGLushort (M.VertTex (i,_) (j,_) (k,_))           = V.map fromIntegral (V.fromList [i,j,k])
    faceToGLushort (M.VertNorm (i,_) (j,_) (k,_))          = V.map fromIntegral (V.fromList [i,j,k])
    faceToGLushort (M.VertTexNorm (i,_,_) (j,_,_) (k,_,_)) = V.map fromIntegral (V.fromList [i,j,k])

-- | 'newGLmodel' vertices normals uvs indices vertexShader fragmentShader
{-# INLINE newGLmodel #-}
newGLmodel :: S.Vector GLfloat 
           -> S.Vector GLfloat 
           -> S.Vector GLfloat 
           -> S.Vector GLushort 
           -> FilePath -> FilePath -> IO GLmodel
newGLmodel vert norm uv elems vshad fshad = do
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
    return GLmodel
        { setModelOffset = writeIORef moffset
        , setModelRotate = writeIORef mrotation
        , setModelScale  = writeIORef mscale
        , drawModel = do
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
            -- Normal attribute buffer
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
            -- Clean up
            glDisableVertexAttribArray vertexAttribute
            glDisableVertexAttribArray uvAttribute
            glUseProgram 0
        }
  where
    !size = 2*fromIntegral (S.length vert)

uniformFromVec :: GLint -> V3 GLfloat -> IO ()
uniformFromVec un (V3 x y z) = glUniform3f un x y z

uniformFromQuat :: GLint -> Quaternion GLfloat -> IO ()
uniformFromQuat un (Quaternion w (V3 x y z)) = glUniform4f un x y z w

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


