{-# LANGUAGE ForeignFunctionInterface, MagicHash, FlexibleInstances, OverloadedStrings, StandaloneDeriving, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Util where
import System.IO.Unsafe
import Foreign.C
import Foreign hiding (unsafePerformIO)
import Data.String
import GHC.Base
import GHC.Ptr
import Control.Monad

import Graphics.Rendering.OpenGL.Raw

{-
import Numeric.LinearAlgebra
import Data.Packed.Development
import Data.Packed.Vector
-}
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Generic as G

foreign import ccall "stdio.h puts" 
    puts :: CString -> IO ()

foreign import ccall "stdio.h fputs" 
    fputs :: Ptr CFile -> CString -> IO ()

foreign import ccall "stdio.h stderr" 
    stderr :: Ptr CFile

putError :: CString -> IO ()
putError = fputs stderr

-- | Reverse function application
(&) :: a -> (a -> b) -> b
x & f = f x
infixl 1 &

orFail :: IO Bool -> String -> IO ()
orFail x err = do
    b <- x
    when b (error err)
infixl 0 `orFail`

while :: IO Bool -> IO a -> IO ()
while pred block = loop
  where
    loop = do
        ok <- pred
        when ok (do block; loop)

{-# NOINLINE unsafePackCString #-}
unsafePackCString :: String -> CString
unsafePackCString = unsafePerformIO . newCString

instance IsString CString where
    {-# INLINE fromString #-}
    fromString = unsafePackCString

-- Don't allocate a String to arrive at a CString; GHC internally uses 
-- CStrings to allocate Strings.
{-# RULES
  "fromString/CString"
  forall s. unsafePackCString (unpackCString# s) = Ptr s
  #-}

-- | A very unsafe but handy Num instance for pointers, to use them
-- like C pointers. The implementation coerces pointers, to and from
-- 'WordPtr', which has a Num instance.
instance Num (Ptr a) where
    fromInteger = wordPtrToPtr . fromIntegral
    x + y       = wordPtrToPtr (ptrToWordPtr x + ptrToWordPtr y)
    x * y       = wordPtrToPtr (ptrToWordPtr x * ptrToWordPtr y)
    x - y       = wordPtrToPtr (ptrToWordPtr x - ptrToWordPtr y)
    abs         = wordPtrToPtr . abs . ptrToWordPtr
    signum      = wordPtrToPtr . signum . ptrToWordPtr
    negate      = wordPtrToPtr . negate . ptrToWordPtr

{-
withMatrix :: Element e => (Ptr e -> IO a) -> Matrix e -> IO a
withMatrix f m = do
    let (fp, _, _) = V.unsafeToForeignPtr (V.convert (flatten m))
    withForeignPtr fp f

withVector :: Element e => (Ptr e -> IO a) -> Vector e -> IO a
withVector f v = do
    let (fp, _, _) = V.unsafeToForeignPtr (V.convert v)
    withForeignPtr fp f

instance Element GLfloat
-}

alloca' :: Storable a => (Ptr a -> IO b) -> IO a
alloca' f = alloca (\ptr -> f ptr >> peek ptr)

