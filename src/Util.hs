{-# LANGUAGE ForeignFunctionInterface, MagicHash, FlexibleInstances, OverloadedStrings, StandaloneDeriving, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies #-}
module Util where
import Foreign.C
import Foreign hiding (unsafePerformIO)
import Data.String
import Data.IORef
import GHC.Base
import GHC.Ptr
import Control.Monad
import Debug.Trace
import qualified Data.Vector.Mutable as V
import System.IO.Unsafe
import qualified System.IO as IO
import qualified Data.Text as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Array as T

warning :: String -> a -> a
warning s a = unsafePerformIO $ do
    IO.hPutStr IO.stderr ("warning: " ++ s)
    return a

tr :: String -> a -> a
tr = trace

tri :: (Functor m, Show a) => String -> m a -> m a
tri l = fmap (\x -> tr (l ++ show x) x)

foreign import ccall "stdio.h puts" 
    puts :: CString -> IO ()

foreign import ccall "stdio.h fputs" 
    fputs :: Ptr CFile -> CString -> IO ()

foreign import ccall "stdio.h stderr" 
    stderr :: Ptr CFile

putError :: CString -> IO ()
putError = fputs stderr

orFail :: IO Bool -> String -> IO ()
orFail x err = do
    b <- x
    when b (error err)
infixl 0 `orFail`

{-# INLINE while #-}
while :: IORef Bool -> IO a -> IO ()
while predicate block = loop
  where
    loop = do
        ok <- readIORef predicate
        when ok (block >> loop)

{-# INLINE while' #-}
while' :: a -> IORef Bool -> (a -> IO a) -> IO ()
while' z predicate f = loop z
  where
    loop s = do
        ok <- readIORef predicate
        when ok
          (f s >>= loop)

{-# INLINE for #-}
for :: (Ord b, Num b) => b -> b -> b -> (b -> IO a) -> IO ()
for z s lim f = go z
  where
    go i | i < lim   = f i >> go (i+s)
         | otherwise = return ()

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

alloca' :: Storable a => (Ptr a -> IO b) -> IO a
alloca' f = alloca (\ptr -> f ptr >> peek ptr)

forIOV :: V.IOVector a -> (a -> IO b) -> IO ()
forIOV v f = go 0
  where
    len  = V.length v
    go i = when (i < len) $ do 
        f =<< V.unsafeRead v i
        go (i+1)

updateAllIOV' :: V.IOVector a -> (a -> IO a) -> IO ()
updateAllIOV' v f = go 0
  where
    len  = V.length v
    go i = when (i < len) $ do 
        V.unsafeWrite v i 
            =<< f 
            =<< V.unsafeRead v i
        go (i + 1)


updateAllIOV :: V.IOVector a -> (a -> a) -> IO ()
updateAllIOV v f = go 0
  where
    len  = V.length v
    go i = when (i < len) $ do 
        V.unsafeWrite v i . f 
            =<< V.unsafeRead v i
        go (i + 1)


sign :: (Integral b, Num c) => b -> c
sign = fromIntegral . signum
