{-# LANGUAGE OverloadedStrings #-}
module Save 
    ( Savable(..)
    , Key(..), Type(..), Value(..)
    , Config
    , mkConfig
    , conf
    , textT, natT, listT, arrayT, pairT
    , text, nat, list, pair
    , lookupConf
    , parseConfig
    , readConfig
    , formatConfig
    , saveConfig
    , intOf
    ) where
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A
import Control.Monad.Writer
import Control.Applicative
import Data.List

class Savable a where
    save :: a -> Config
    load :: Config -> a

type Config = M.Map Key Value

mkConfig :: Writer Config () -> Config
mkConfig = execWriter

conf :: T.Text -> Value -> Writer Config ()
conf t v = tell $ 
    M.singleton (Key (T.split (== '.') t) (valType v)) v

data Type 
    = TN -- ^ natural number
    | TT -- ^ text
    | TL !Type -- ^ homogenous list
    | THL (V.Vector Type) -- ^ heterogenous list
  deriving (Show,Read,Eq,Ord)

type Category = [T.Text]

textT, natT :: Category -> Key
textT c = Key c TT
natT  c = Key c TN

listT :: Category -> Type -> Key
listT c x = Key c (TL x)

arrayT :: Category -> [Type] -> Key
arrayT c x = Key c (THL (V.fromList x))

pairT :: Category -> Type -> Type -> Key
pairT c x y = Key c (THL (V.fromList [x,y]))

pair :: Value -> Value -> Value
pair v t = L (V.fromList [v,t])

text :: T.Text -> Value
text = T

nat :: Real a => a -> Value
nat v = N (realToFrac v)

list :: [Value] -> Value
list = L . V.fromList

data Key  = Key Category !Type
  deriving (Eq,Ord)

instance Show Key where
    show (Key ks _) = intercalate "." (map T.unpack ks)

data Value 
    = N !A.Number
    | T !T.Text
    | L !(V.Vector Value)
  deriving (Eq,Ord)
instance Show Value where
    show (N n)  = show n
    show (T t)  = show t
    show (L vs) = "[" ++ intercalate " " (V.toList (V.map show vs)) ++ "]"

valType :: Value -> Type
valType (N _)  = TN
valType (T _)  = TT
valType (L vs) = 
    if V.all (\v -> Just v == vs V.!? 0) vs
    then case vs V.!? 0 of
        Just v -> TL (valType v)
        _      -> THL V.empty
        
    else THL (V.map valType vs)

okType :: Value -> Type -> Bool
okType (N _) TN       = True
okType (T _) TT       = True
okType (L v) (TL t)   = case v V.!? 0 of
    Just h -> valType h == t
    _      -> False
okType (L l) (THL ts) = V.and (V.zipWith (\v t -> valType v == t) l ts)
okType _     _        = False

lookupConf :: Key -> Config -> Maybe Value
lookupConf key@(Key _ type_) conf = do
    value <- M.lookup key conf
    guard (okType value type_)
    return value

parseValue :: A.Parser Value
parseValue = text' <|> number <|> list <|> nil
  where
    chars   = mapM A.char

    number  = N <$> A.number

    text'   = T <$> do
        A.char '"'
        inside <- A.manyTill 
            (chars "\\\"" <|> fmap return A.anyChar)
            (A.char '"')
        return (T.pack (concat inside))

    nil  = do
        A.char '['
        A.skipSpace
        A.char ']'
        return (L V.empty)
    list = do
        A.char '['
        vals <- A.manyTill 
            (A.skipSpace *> parseValue <* A.skipSpace)
            (A.char ']') 
        return (L (V.fromList vals))

parseKey :: A.Parser [T.Text]
parseKey = (A.skipSpace *> A.takeTill (`elem` " .") <* A.skipSpace) 
 `A.sepBy1` A.char '.'

parseKeyValue :: A.Parser ([T.Text],Value)
parseKeyValue = do
    keys <- parseKey
    A.skipSpace
    A.char '='
    A.skipSpace
    val <- parseValue
    return (keys,val)

parseKeyValues :: M.Map [T.Text] Type -> A.Parser Config
parseKeyValues types = M.fromList <$> A.many1' correctlyTypedKV
  where
    correctlyTypedKV = do
        (keys,val) <- parseKeyValue
        typ <- case M.lookup keys types of
                Just typ' -> do
                    guard (okType val typ')
                    return typ'
                _         -> return (valType val)
        A.endOfLine
        return (Key keys typ, val)

parseConfig :: T.Text -> Config
parseConfig t = case A.parseOnly (parseKeyValues M.empty) (t <> "\n") of
    Right c -> c
    Left e  -> error e

readConfig :: FilePath -> IO Config
readConfig path = parseConfig `fmap` T.readFile path

saveConfig :: FilePath -> Config -> IO ()
saveConfig path = T.writeFile path . formatConfig

formatConfig :: Config -> T.Text
formatConfig m = execWriter $ flip M.traverseWithKey m $ \(Key cat _) val -> do
    str (T.intercalate "." cat)
    str " = "
    str (T.pack (show val))
    ln
  where
    str = tell
    ln  = tell "\n"

intOf :: Integral a => a -> Maybe Value -> a
intOf _ (Just (N v)) = fromIntegral (floor v)
intOf x _            = x
