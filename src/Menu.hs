{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, NamedFieldPuns, RecordWildCards, BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Menu where
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as I
import Data.Foldable (fold)
import Data.IORef
import Data.String
import Data.Char
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Control.Applicative
import Control.Lens hiding (simple, element)
import Geometry (V, vec4)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Foreign.C
import Graphics.UI.GLFW (Key(..))
import Controls
import Util()

data Element
    = Button  { _press :: !(IO ()) }
    | Field   { _press :: !(IO ())
              , _query :: !(IO ByteString)
              }
    | TextBox { _enter :: !(Maybe Char -> IO ())
              , _press :: !(IO ())
              , _query :: !(IO ByteString)
              }
    | ToMenu  { _target :: ![String] }
    | GoBack
    | NilE

instance Show Element where
    show Button{}    = "Button"
    show Field{}     = "Field"
    show (ToMenu s)  = "ToMenu " ++ show s
    show GoBack      = "GoBack"
    show NilE        = "NilE"
    show TextBox{}   = "TextBox"

-- | RGBA colour
type Colour = V

data Label
    = Plain !CString
    | Updated !(IO ByteString)

{-# INLINE withLabel #-}
withLabel :: Label -> (CString -> IO ()) -> IO ()
withLabel (Plain str)   f = f str
withLabel (Updated iob) f = iob >>= flip B.useAsCString f

instance IsString Label where
    fromString s = Plain (fromString s)

instance Show Label where
    show (Plain s) = "Plain " ++ show s
    show _         = "Updated"

data Item = Item
    { _label         :: !Label
    , _ielement      :: !Element
    , _fontColour    :: !Colour
    , _bgColour      :: !Colour
    , _hovered       :: !Colour
    , _pressed       :: !Colour
    } deriving (Show)

data MenuS = MenuS
    { _menu          :: ![String]
    , _selection     :: !Int
    } deriving (Show)

startingMenuS :: MenuS
startingMenuS = MenuS [] 0

makeLenses ''Element
makeLenses ''Item
makeLenses ''MenuS

type Menu   = M.Map [String] (I.IntMap Item)
type InMenu = StateT MenuS (Reader Menu)

type WIO w  = WriterT w IO
type MkMenu = WIO Menu
type Items  = [Item]

runMenu :: IORef MenuS -> Menu -> InMenu a -> IO a
runMenu ms' m f = do
    ms <- readIORef ms'
    case runReader (runStateT f ms) m of
      (a,s) -> do
        writeIORef ms' $! s
        return a

item' :: Label -> Element -> Item
item' l f = Item
    { _label        = l
    , _ielement     = f
    , _fontColour   = vec4 1 1 1 1
    , _hovered      = vec4 0.3 0.3 0.3 1
    , _bgColour     = vec4 0.9 0.9 0.9 1
    , _pressed      = vec4 1 1 1 1
    }

showed :: Show a => Label -> a -> (a -> a) -> (a -> IO ()) -> WIO Items ()
showed l z f cc = do
    elm <- liftIO (newShow z f cc)
    tell [item' l elm]

dynLabel :: IO (Maybe B.ByteString) -> WIO Items ()
dynLabel f = tell [item' (Updated (fold <$> f)) NilE]

newMenu :: MkMenu () -> IO Menu
newMenu = execWriterT

subMenu :: [String] -> WIO Items () -> MkMenu ()
subMenu s
    = lift . execWriterT
  >=> tell . M.singleton s . I.fromList . zip [0..]

newBool :: (Bool -> IO ()) -> IO Element
newBool f = do
    ref <- newIORef False
    return Field
        { _press = do modifyIORef' ref not; readIORef ref >>= f
        , _query = B.pack . show <$> readIORef ref
        }

newShow :: Show a => a -> (a -> a) -> (a -> IO ()) -> IO Element
newShow z f cc = do
    ref <- newIORef z
    return Field
        { _press = do modifyIORef' ref f; readIORef ref >>= cc
        , _query = B.pack . show <$> readIORef ref
        }

newTextBox :: (ByteString -> IO ()) -> (ByteString -> IO ()) -> IO Element
newTextBox update pressed = do
    ref <- newIORef ""
    return TextBox
        { _enter = \c' -> do
               case c' of
                   Just c -> modifyIORef' ref (`B.snoc` toLower c)
                   _      -> modifyIORef' ref (^._init)
               bs <- readIORef ref
               update bs
        , _press = readIORef ref >>= pressed
        , _query = readIORef ref
        }

button :: Label -> IO () -> WIO Items ()
button l f = tell [item' l (Button f)]

textBox :: Label -> (ByteString -> IO ()) -> WIO Items ()
textBox l f = do
    tb <- liftIO (newTextBox (\_ -> return ()) f)
    tell [item' l tb]

textBox' :: Label -> (ByteString -> IO ()) -> (ByteString -> IO ()) -> WIO Items ()
textBox' l update press = do
    tb <- liftIO (newTextBox update press)
    tell [item' l tb]

toggle :: Label -> (Bool -> IO ()) -> WIO Items ()
toggle l f = do
    e <- liftIO (newBool f)
    tell [item' l e]

link :: Label -> [String] -> WIO Items ()
link l s = tell [item' l (ToMenu s)]

back :: WIO Items ()
back = tell [item' "back" GoBack]

topMenu :: WIO Items () -> MkMenu ()
topMenu = subMenu []

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

backOne :: InMenu ()
backOne = do
    menu %= safeInit
    selection .= 0

selectedItem :: InMenu (Maybe Item)
selectedItem = do
    pos <- use menu
    sel <- use selection
    asks $ \m -> do
        sub <- M.lookup pos m
        I.lookup sel sub

select :: (Int -> Int) -> InMenu ()
select f = do
    path <- use menu
    sel' <- use selection
    here <- view (at path)
    case do
        (sel, _) <- I.lookupLE (f sel') =<< here
        return sel
      of
        Just s -> selection .= s
        _      -> return ()

runHere :: InMenu (IO ())
runHere = do
    Just itm <- selectedItem
    case _ielement itm of
        Button p      -> return p
        Field  p _    -> return p
        ToMenu path   -> do
            menu      .= path
            selection .= 0
            return (return ())
        GoBack        -> backOne >> return (return ())
        TextBox _ p _ -> return p
        _             -> return (return ())

runEvent :: Event -> InMenu (IO ())
runEvent ev = case ev of
    Select     -> runHere
    Back       -> internally backOne
    MenuUp     -> internally (select succ)
    MenuDown   -> internally (select pred)
    _          -> return (return ())
  where
    internally f = f >> return (return ())

runKey :: Key -> InMenu (IO ())
runKey k = do
    Just itm <- selectedItem
    case itm^.ielement of
        TextBox f _ _ -> case k of
            CharKey c    -> return $! f (Just c)
            KeyBackspace -> return $! f Nothing
            _            -> return (return ())
        _ -> return (return ())
