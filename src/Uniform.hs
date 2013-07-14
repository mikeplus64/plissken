{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Uniform where
import qualified Data.ByteString.Char8 as B
import Graphics.Rendering.OpenGL.Raw
import Foreign

import Geometry hiding (vec2,vec3,vec4,x,y,z,w)
import Util ()

data Mat4
data Mat3
data Mat2
data Vec2
data Vec3
data Vec4
data Float'

type Need t  = forall xs. B.ByteString -> Needs xs -> Needs (t -> xs)
data Proxy s = Proxy

umat4  :: Need Mat4
umat3  :: Need Mat3
umat2  :: Need Mat2
uvec2  :: Need Vec2
uvec3  :: Need Vec3
uvec4  :: Need Vec4
ufloat :: Need Float'
umat4  = More (Proxy::Proxy Mat4)
umat3  = More (Proxy::Proxy Mat3)
umat2  = More (Proxy::Proxy Mat2)
uvec4  = More (Proxy::Proxy Vec4)
uvec3  = More (Proxy::Proxy Vec3)
uvec2  = More (Proxy::Proxy Vec2)
ufloat = More (Proxy::Proxy Float')

class Result' a c | a -> c
instance Result' b c => Result' (a -> b) c
instance Result' b b

data Needs s where
    More :: !(f t) -> !B.ByteString -> !(Needs xs) -> Needs (t -> xs)
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

getUniformLoc :: GLuint -> B.ByteString -> IO GLint
getUniformLoc prog name = do
  loc <- name `B.useAsCString` glGetUniformLocation prog
  return loc
  if loc >= 0
    then return loc
    else if "gl_" `B.isPrefixOf` name
      then error $ show name ++ " cannot be used as a uniform in the program " ++ show prog ++ " as it starts with the reserved prefix 'gl_'" 
      else do
         putStrLn $ "Warning: uniform " ++ show name ++ "'s location is < 0"
         return loc

class GetUniforms s where
    getUniforms :: Needs s -> GLuint -> IO (Uniforms s)
instance GetUniforms (IO ()) where
    {-# INLINE getUniforms #-}
    getUniforms _           _ = return Uz
instance GetUniforms s => GetUniforms (t -> s) where
    {-# INLINE getUniforms #-}
    getUniforms (More _ s xs) prog = do
        this <- getUniformLoc prog s
        rest <- getUniforms xs prog
        return (Uc this rest)

class Uploadable s r | s -> r where
    uploadUniforms' :: IO () -> Uniforms s -> (IO () -> IO ()) -> r
instance Uploadable (IO ()) (IO ()) where
    {-# INLINE uploadUniforms' #-}
    uploadUniforms' acc _ f = f acc
instance Uploadable b d => Uploadable (Mat4 -> b) (M -> d) where
    {-# INLINE uploadUniforms' #-}
    uploadUniforms' acc (Uc x xs) f mat = 
        uploadUniforms' (do acc; glUniformMatrix4fv x 1 0 . castPtr `appMatrixRaw` mat) xs f
instance Uploadable b d => Uploadable (Mat3 -> b) (M -> d) where
    {-# INLINE uploadUniforms' #-}
    uploadUniforms' acc (Uc x xs) f mat = 
        uploadUniforms' (do acc; glUniformMatrix3fv x 1 0 . castPtr `appMatrixRaw` mat) xs f
instance Uploadable b d => Uploadable (Mat2 -> b) (M -> d) where
    {-# INLINE uploadUniforms' #-}
    uploadUniforms' acc (Uc x xs) f mat = 
        uploadUniforms' (do acc; glUniformMatrix2fv x 1 0 . castPtr `appMatrixRaw` mat) xs f
instance Uploadable b d => Uploadable (Vec4 -> b) (V -> d) where
    {-# INLINE uploadUniforms' #-}
    uploadUniforms' acc (Uc x xs) f vec = 
        uploadUniforms' (do acc; glUniform4fv x 1 . castPtr `appVector` vec) xs f
instance Uploadable b d => Uploadable (Vec3 -> b) (V -> d) where
    {-# INLINE uploadUniforms' #-}
    uploadUniforms' acc (Uc x xs) f vec = 
        uploadUniforms' (do acc; glUniform3fv x 1 . castPtr `appVector` vec) xs f
instance Uploadable b d => Uploadable (Vec2 -> b) (V -> d) where
    {-# INLINE uploadUniforms' #-}
    uploadUniforms' acc (Uc x xs) f vec = 
        uploadUniforms' (do acc; glUniform2fv x 1 . castPtr `appVector` vec) xs f
instance Uploadable b d => Uploadable (Float' -> b) (F -> d) where
    {-# INLINE uploadUniforms' #-}
    uploadUniforms' acc (Uc x xs) f flt = 
        uploadUniforms' (do acc; glUniform1f x (realToFrac flt)) xs f

uploadUniforms :: Uploadable s r => Uniforms s -> (IO () -> IO ()) -> r
uploadUniforms = uploadUniforms' (return ())

