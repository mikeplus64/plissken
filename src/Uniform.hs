{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module GLSL where
import Graphics.Rendering.OpenGL.Raw
import Foreign.C
import Foreign

import Geometry hiding (vec2,vec3,vec4)
import Util ()

data Uniform a
data VertexAttrib a

data Mat4
data Mat3
data Mat2
data Vec2
data Vec3
data Vec4
data Float'

type Need t  = forall xs. CString -> Needs xs -> Needs (t -> xs)
data Proxy s = Proxy

mat4  :: Need Mat4
mat3  :: Need Mat3
mat2  :: Need Mat2
vec2  :: Need Vec2
vec3  :: Need Vec3
vec4  :: Need Vec4
float :: Need Float'
mat4  = More (Proxy::Proxy Mat4)
mat3  = More (Proxy::Proxy Mat3)
mat2  = More (Proxy::Proxy Mat2)
vec4  = More (Proxy::Proxy Vec4)
vec3  = More (Proxy::Proxy Vec3)
vec2  = More (Proxy::Proxy Vec2)
float = More (Proxy::Proxy Float')

class Result' a c | a -> c
instance Result' b c => Result' (a -> b) c
instance Result' b b

data Needs s where
    More :: !(f t) -> !CString -> !(Needs xs) -> Needs (t -> xs)
    OK   :: Needs (IO ())

needs :: (Needs (IO ()) -> Needs ys) -> Needs ys
needs f = f OK

data Uniforms s where
    Uc :: !GLint -> !(Uniforms s) -> Uniforms (t -> s)
    Uz :: Uniforms (IO ())

deriving instance Show (Uniforms s)
instance Show (Needs s) where
    show (More _ x xs) = "More " ++ show x ++ " (" ++ show xs ++ ")"
    show OK            = "OK"

class GetUniforms s where
    getUniforms :: Needs s -> GLuint -> IO (Uniforms s)
instance GetUniforms (IO ()) where
    {-# INLINE getUniforms #-}
    getUniforms _           _ = return Uz
instance GetUniforms s => GetUniforms (t -> s) where
    {-# INLINE getUniforms #-}
    getUniforms (More _ s xs) prog = do
        this <- glGetUniformLocation prog s
        rest <- getUniforms xs prog
        free s
        return (Uc this rest)

class Uploadable s r | s -> r where
    uploadUniforms' :: IO () -> Uniforms s -> (IO () -> IO ()) -> r

instance Uploadable (IO ()) (IO ()) where
    uploadUniforms' acc _ f = f acc

instance Uploadable b d => Uploadable (Mat4 -> b) (M -> d) where
    uploadUniforms' acc (Uc x xs) f mat = 
        uploadUniforms' (do acc; glUniformMatrix4fv x 1 0 . castPtr `appMatrixRaw` mat) xs f

instance Uploadable b d => Uploadable (Mat3 -> b) (M -> d) where
    uploadUniforms' acc (Uc x xs) f mat = 
        uploadUniforms' (do acc; glUniformMatrix3fv x 1 0 . castPtr `appMatrixRaw` mat) xs f

instance Uploadable b d => Uploadable (Mat2 -> b) (M -> d) where
    uploadUniforms' acc (Uc x xs) f mat = 
        uploadUniforms' (do acc; glUniformMatrix2fv x 1 0 . castPtr `appMatrixRaw` mat) xs f

instance Uploadable b d => Uploadable (Vec4 -> b) (V -> d) where
    uploadUniforms' acc (Uc x xs) f vec = 
        uploadUniforms' (do acc; glUniform4fv x 1 . castPtr `appVector` vec) xs f

instance Uploadable b d => Uploadable (Vec3 -> b) (V -> d) where
    uploadUniforms' acc (Uc x xs) f vec = 
        uploadUniforms' (do acc; glUniform4fv x 1 . castPtr `appVector` vec) xs f

instance Uploadable b d => Uploadable (Vec2 -> b) (V -> d) where
    uploadUniforms' acc (Uc x xs) f vec = 
        uploadUniforms' (do acc; glUniform4fv x 1 . castPtr `appVector` vec) xs f

instance Uploadable b d => Uploadable (Float' -> b) (F -> d) where
    uploadUniforms' acc (Uc x xs) f flt = 
        uploadUniforms' (do acc; glUniform1f x (realToFrac flt)) xs f

uploadUniforms :: Uploadable s r => Uniforms s -> (IO () -> IO ()) -> r
uploadUniforms = uploadUniforms' (return ())

testNeeds = needs 
    $ mat4 "MVP"
    . mat4 "M"
    . vec4 "diffuse"

