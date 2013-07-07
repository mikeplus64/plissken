{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Uniform 
    (Uniforms(..), Upload, uploadUniforms', uploadUniforms, newUMat4, newUVec4, newUVec3, Mat4, Vec3, Vec4, Uz)
  where
import Graphics.Rendering.OpenGL.Raw
import Geometry
import Util ()
import Foreign
import Foreign.C

type Mat4 b = M -> b
type Vec4 b = V -> b
type Vec3 b = V -> b
type Uz     = IO ()

data Uniforms :: * -> * where
    UMat4 :: !GLint -> !(Uniforms s) -> Uniforms (M -> s)
    UVec4 :: !GLint -> !(Uniforms s) -> Uniforms (V -> s)
    UVec3 :: !GLint -> !(Uniforms s) -> Uniforms (V -> s)
    Uz   :: Uniforms (IO ())

deriving instance Show (Uniforms s)
deriving instance Eq (Uniforms s)

newUMat4 :: GLuint -> CString -> IO GLint
newUMat4 = glGetUniformLocation

newUVec4 :: GLuint -> CString -> IO GLint
newUVec4 = glGetUniformLocation

newUVec3 :: GLuint -> CString -> IO GLint
newUVec3 = glGetUniformLocation

class Upload s where
    uploadUniforms' :: IO () -> Uniforms s -> (IO () -> IO ()) -> s

instance Upload xs => Upload (M -> xs) where
    {-# INLINE uploadUniforms' #-}
    uploadUniforms' acc (UMat4 loc xs) f m4
      = uploadUniforms'
            (do acc; glUniformMatrix4fv loc 1 (fromIntegral gl_TRUE) . castPtr `appMatrixRaw` m4)
            xs
            f

instance Upload xs => Upload (V -> xs) where
    {-# INLINE uploadUniforms' #-}
    uploadUniforms' acc (UVec4 loc xs) f v4
      = uploadUniforms'
            (do acc; glUniform4fv loc 1 . castPtr `appVector` v4)
            xs
            f
    uploadUniforms' acc (UVec3 loc xs) f v3
      = uploadUniforms' 
            (do acc; glUniform3fv loc 1 . castPtr `appVector` v3)
            xs
            f

instance Upload (IO ()) where
    {-# INLINE uploadUniforms' #-}
    uploadUniforms' acc _ f = f acc

{-# INLINE uploadUniforms #-}
uploadUniforms :: Upload s => Uniforms s -> (IO () -> IO ()) -> s
uploadUniforms = uploadUniforms' (return ())

