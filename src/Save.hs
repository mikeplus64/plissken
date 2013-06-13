module Save where
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Attoparsec.Text as A
import Control.Monad.State
import Control.Applicative
import Data.List

class Savable a where
    save :: a -> Config
    load :: Config -> a

type Config = M.Map Key Value

data Type 
    = TN -- ^ natural number
    | TT -- ^ text
    | TL !Type -- ^ homogenous list
    | THL (V.Vector Type) -- ^ heterogenous list
  deriving (Show,Read,Eq,Ord)

data Key  = Key [T.Text] !Type
  deriving (Eq,Ord)

typed :: T.Text -> Type -> Key
typed = Key . T.split (== '.')

(=:) :: Key -> Value -> (Key,Value)
(=:) = (,)

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
    show (L vs) = show (V.toList vs)

valType :: Value -> Type
valType (N _)  = TN
valType (T _)  = TT
valType (L vs) = 
    if V.all (\v -> Just v == vs V.!? 0) vs
    then TL (valType (V.head vs))
    else THL (V.map valType vs)

okType :: Value -> Type -> Bool
okType (N _) TN       = True
okType (T _) TT       = True
okType (L v) (TL t)   = case v V.!? 0 of
    Just h -> valType h == t
    _      -> False
okType (L l) (THL ts) = V.and (V.zipWith (\v t -> valType v == t) l ts)
okType _     _        = False

readValue :: Key -> Config -> Maybe Value
readValue key@(Key _ type_) conf = do
    value <- M.lookup key conf
    guard (okType value type_)
    return value

parseValue :: A.Parser Value
parseValue = text <|> number <|> list
  where
    chars   = mapM A.char

    number  = N <$> A.number

    text    = T <$> do
        A.char '"'
        inside <- A.manyTill 
            (chars "\\\"" <|> fmap return A.anyChar)
            (A.char '"')
        return (T.pack (concat inside))

    primitive = text <|> number

    list = do
        A.char '['
        vals <- A.manyTill 
            (A.skipSpace *> primitive <* A.skipSpace)
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
                    guard (valType val == typ')
                    return typ'
                _         -> return (valType val)
        A.endOfLine
        return (Key keys typ, val)

readConfig :: FilePath -> IO Config
readConfig path = do
    cfg <- T.readFile path
    case A.parseOnly (parseKeyValues M.empty) cfg of
        Right c -> return c
        Left e  -> error e


