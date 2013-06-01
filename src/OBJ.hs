{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module OBJ where
import qualified Data.Attoparsec as A
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as V

import Control.Monad.State.Strict
import Control.Monad
import Control.Monad.ST
import Control.Lens
import Control.Applicative

import Graphics.Rendering.OpenGL.Raw
import Linear

import Util

type P = GLfloat

-- n indices
type I = Int
type I2 = (Int,Int)
type I3 = (Int,Int,Int)

-- | See http://en.wikipedia.org/wiki/Wavefront_.obj_file
-- This type implements a single line of an obj file.
--
-- Not defined in the 'Types' module because this type only matters here.
data ObjCommand
    = Comment !ByteString
    | MTLlib !ByteString
    | UseMTL !ByteString
    | Object !ByteString
    | Group !ByteString
    | S !Bool
    | V !P !P !P
    | VT !P !P
    | VN !P !P !P
    | FV !I !I !I
    | FVT !I2 !I2 !I2
    | FVN !I2 !I2 !I2
    | FVTN !I3 !I3 !I3
  deriving (Show, Eq)

type OBJ = [ObjCommand]

readObj :: FilePath -> IO (Either String OBJ)
readObj file = parseOnly parseObj `fmap` B.readFile file

parseObj :: Parser OBJ
parseObj = do
    objs <- (seperatedByLines . choice) 
        [ parseFace
        , parseVert
        , parseVertTex
        , parseVertNorm
        , parseSmooth
        , parseGroup
        , parseObject
        , parseComment
        , parseUseMTL
        , parseMTLlib 
        , fail "Unrecognised .obj command."]
    endOfInput <|> endOfLine >> endOfInput
    return objs
  where
    seperatedByLines f = f `sepBy` endOfLine

-- | Parse a floating point number.
float :: (Fractional a, Real a) => Parser a
float = fmap realToFrac double

-- | Parse something seperated by spaces.
spaced :: Parser a -> Parser [a]
spaced f = f `sepBy` space

parseFace :: Parser ObjCommand
parseFace = do
    string "f "
    fvtn <|> fvn <|> fvt <|> fv
  where
    -- parse multiple vertex indices, vertex indices+texture indices, vertex
    -- indices+vertex normal indices and vertex indices+texture indices+vertex
    -- normal indices
    fv    = do [x,y,z] <- spaced fv'                    ; return (FV x y z)
    fvt   = do [x,y,z] <- spaced fvt'                   ; return (FVT x y z)
    fvn   = do [x,y,z] <- spaced fvn'                   ; return (FVN x y z)
    fvtn  = do [x,y,z] <- spaced fvtn'                  ; return (FVTN x y z)
    -- individual face parsers
    fv'   = decimal
    fvt'  = do [x,y]   <- decimal `sepBy` char '/'      ; return (x,y)
    fvn'  = do [x,y]   <- decimal `sepBy` string "//"   ; return (x,y)
    fvtn' = do [x,y,z] <- decimal `sepBy` char '/'      ; return (x,y,z)

parseVert :: Parser ObjCommand
parseVert = do
    string "v "
    [x,y,z] <- spaced float
    return (V x y z)

parseVertTex :: Parser ObjCommand
parseVertTex = do
    string "vt "
    [x,y] <- spaced float
    return (VT x y)

parseVertNorm :: Parser ObjCommand
parseVertNorm = do
    string "vn "
    [x,y,z] <- spaced float
    return (VN x y z)

parseSmooth :: Parser ObjCommand
parseSmooth = do
    string "s "
    onOrOff <- string "on" 
           <|> string "1"
           <|> string "off"
           <|> string "0"
    return (S (onOrOff == "on" || onOrOff == "1"))

parseGroup :: Parser ObjCommand
parseGroup = do
    string "g "
    Group `fmap` A.takeTill isEndOfLine

parseObject :: Parser ObjCommand
parseObject = do
    string "o "
    Object `fmap` A.takeTill isEndOfLine

parseComment :: Parser ObjCommand
parseComment = do
    char '#'
    Comment `fmap` A.takeTill isEndOfLine

parseUseMTL :: Parser ObjCommand
parseUseMTL = do
    string "usemtl "
    UseMTL `fmap` A.takeTill isEndOfLine

parseMTLlib :: Parser ObjCommand
parseMTLlib = do
    string "mtllib "
    MTLlib `fmap` A.takeTill isEndOfLine

