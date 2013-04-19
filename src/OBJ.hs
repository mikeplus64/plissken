{-# OPTIONS_GHC -funbox-strict-fields #-}
import qualified Data.Attoparsec as A
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)

import qualified Data.Vector.Storable as VS

import Control.Applicative
import Control.Monad

import Graphics.Rendering.OpenGL.Raw
import Linear
import Types

type P = GLfloat

-- n indices
type I = Int
type I2 = (Int,Int)
type I3 = (Int,Int,Int)

-- | See http://en.wikipedia.org/wiki/Wavefront_.obj_file
-- This type implements a single line of an obj file.
data OBJ
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

data Loadable = Loadable
    { lVertices :: !(VS.Vector (V3 
    , lElements

readObj :: FilePath -> IO (Either String [OBJ])
readObj file = parseOnly parseObj `fmap` B.readFile file

loadObj :: [OBJ] -> IO ()
loadObj = do
    

parseObj :: Parser [OBJ]
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
    endOfLine >> endOfInput
    return objs
  where
    seperatedByLines f = f `sepBy` endOfLine

-- | Parse a floating point number.
float :: (Fractional a, Real a) => Parser a
float = fmap realToFrac double

-- | Parse something seperated by spaces.
spaced :: Parser a -> Parser [a]
spaced f = f `sepBy` space

parseFace :: Parser OBJ
parseFace = string "f " >> fvtn <|> fvn <|> fvt <|> fv
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

parseVert :: Parser OBJ
parseVert = do
    string "v "
    [x,y,z] <- spaced float
    return (V x y z)

parseVertTex :: Parser OBJ
parseVertTex = do
    string "vt "
    [x,y] <- spaced float
    return (VT x y)

parseVertNorm :: Parser OBJ
parseVertNorm = do
    string "vn "
    [x,y,z] <- spaced float
    return (VN x y z)

parseSmooth :: Parser OBJ
parseSmooth = do
    string "s "
    onOrOff <- string "on" 
           <|> string "1"
           <|> string "off"
           <|> string "0"
    return (S (onOrOff == "on" || onOrOff == "1"))

parseGroup :: Parser OBJ
parseGroup = do
    string "g "
    Group `fmap` A.takeTill isEndOfLine

parseObject :: Parser OBJ
parseObject = do
    string "o "
    Object `fmap` A.takeTill isEndOfLine

parseComment :: Parser OBJ
parseComment = do
    char '#'
    Comment `fmap` A.takeTill isEndOfLine

parseUseMTL :: Parser OBJ
parseUseMTL = do
    string "usemtl "
    UseMTL `fmap` A.takeTill isEndOfLine

parseMTLlib :: Parser OBJ
parseMTLlib = do
    string "mtllib "
    MTLlib `fmap` A.takeTill isEndOfLine

{-
-- This type uses (inefficient) singly-linked lists, because it is more
-- convenient as it is only intended as a "intermediate" step in loading
-- a model; no .obj data actually exists after a model is loaded into
-- OpenGL.
data ObjData = ObjData
    { lVertices      :: [Vec]
    , lVertNormals   :: [Vec]
    , lFaces         :: [Face]
    }

data NamedObject = NamedObject
    { objectName     :: String
    , objectData     :: ObjData
    }

data Obj = Obj
    { objFile        :: FilePath
    , objData        :: [NamedObject]
    }


obj3 :: B.ByteString -> Parser [a] -> String -> Parser (V3 a)
obj3 e f err = do
    element e
    coords <- f
    case coords of
        [x,y,z] -> return (V3 x y z)
        _       -> fail err



-- | Each parser corresponds with the .obj element the function parses.
objV, objVN, objVP :: Parser (V3 GLfloat)
objV  = obj3 "v"  parseFloats "Only 3 coordinate vertices are supported."
objVN = obj3 "vn" parseFloats "Vertex normals must be 3 numbers."
objVP = obj3 "vp" parseFloats "Parameter spaces vertices must have 3 coordinates."

-- | Parse a texture coordinate
objT :: Parser (V2 GLfloat)
objT = do
    element "vt"
    coords <- parseFloats
    case coords of
        [x,y] -> return (V2 x y)
        _     -> fail "Only 2 coordinate texture coordinates are supported."

-- | Parser the "f" element.
objF :: Parser Face
objF = vtns <|> vns <|> vts <|> vs
  where
    -- Parse multiple vertices, vertices+normals, vertices+texture, 
    -- vertices+textures+normals
    vs   = do [x,y,z] <- spaced v   ; return (V (V3 x y z))
    vns  = do [x,y,z] <- spaced vn  ; return (VN (V3 x y z))
    vts  = do [x,y,z] <- spaced vt  ; return (VT (V3 x y z))
    vtns = do [x,y,z] <- spaced vtn ; return (VTN (V3 x y z))
    -- Individual vertex, vertex+normal, vertex+texture, 
    -- vertex+texture+normal parsers
    v    = float
    vn   = do [x,y] <- float `sepBy` string "//" ; return (x, y)
    vt   = do [x,y] <- float `sepBy` char '/'    ; return (x, y)
    vtn  = do [x,y,z] <- float `sepBy` char '/'  ; return (x, y, z)

objG 
    

element :: B.ByteString -> Parser ()
element e = string e >> void space
-}
