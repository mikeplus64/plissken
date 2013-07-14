{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, NamedFieldPuns, RecordWildCards, BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Menu where
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as I
import qualified Data.Text as T
import Data.Dynamic
import Data.Dynamic.Lens
import Data.Maybe
import Data.IORef
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Control.Applicative
import Control.Lens hiding (simple, element)
import Geometry (V, vec4)
import Controls

data Element
  = Button          { _press       :: !(IO ()) }
  | Field           { _enter       :: !(T.Text -> IO ()) }
  | Toggle          { _toggle      :: !(Bool -> IO ()) }
  | ToMenu          { _targetMenu  :: [T.Text] }

instance Show Element where
    show (Button _)  = "Button"
    show (Field _)   = "Field"
    show (Toggle _)  = "Toggle"
    show (ToMenu t)  = "ToMenu " ++ show t

-- | RGBA colour
type Colour = V

data Item = Item
    { _label         :: !T.Text
    , _element       :: Element
    , _fontColour    :: !Colour
    , _bgColour      :: !Colour
    , _hovered       :: !Colour
    , _pressed       :: !Colour
    } deriving (Show)

data MenuS = MenuS
    { _menu          :: [T.Text]
    , _selection     :: !Int
    , _elementData   :: !(M.Map [T.Text] (I.IntMap Dynamic))
    } deriving (Show)

startingMenuS :: MenuS
startingMenuS = MenuS [] 0 M.empty

makeLenses ''Element
makeLenses ''Item
makeLenses ''MenuS
type Menu = M.Map [T.Text] (I.IntMap Item)
type InMenu = StateT MenuS (Reader Menu)

runMenu :: IORef MenuS -> Menu -> InMenu a -> IO a
runMenu ms' m f = do
    ms <- readIORef ms'
    case runReader (runStateT f ms) m of
      (a,s) -> do
        print s
        writeIORef ms' $! s
        return a

item :: T.Text -> Element -> Writer [Item] ()
item l f = (tell . return) Item
    { _label        = l
    , _element      = f
    , _fontColour   = vec4 1 1 1 1
    , _hovered      = vec4 0.3 0.3 0.3 1
    , _bgColour     = vec4 0.9 0.9 0.9 1
    , _pressed      = vec4 1 1 1 1
    }

button :: T.Text -> IO () -> Writer [Item] ()
button l f = item l (Button f)

backto :: [T.Text] -> Writer [Item] ()
backto = item "back" . ToMenu

newMenu :: Writer Menu () -> IO (Menu, IORef MenuS)
newMenu w = do
    r <- newIORef (MenuS [] 0 M.empty)
    return (execWriter w, r)

newMenu' :: Writer Menu () -> Menu
newMenu' = execWriter

subMenu :: [T.Text] -> Writer [Item] () -> Writer Menu ()
subMenu s
    = tell . M.singleton s
           . I.fromList
           . zip [0..]
           . execWriter

topMenu :: Writer [Item] () -> Writer Menu ()
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

overHere :: (Show a, Typeable a) => a -> (a -> a) -> InMenu ()
overHere x f = do
    stuff <- dataHere
    case stuff of
        Just d -> enterHere (f d)
        _      -> enterHere x

enterHere :: Typeable a => a -> InMenu ()
enterHere t = do
    pos <- use menu
    sel <- use selection
    elementData %= 
      M.alter 
        (Just . maybe (I.singleton sel (toDyn t)) 
                      (I.insert sel (toDyn t)))
        pos

dataHere :: Typeable a => InMenu (Maybe a)
dataHere = do
    pos <- use menu
    sel <- use selection
    preuse (elementData.ix pos.ix sel._Dynamic)

deleteDataHere :: InMenu ()
deleteDataHere = do
    pos <- use menu
    sel <- use selection
    elementData.at pos._Just.at sel .= Nothing

runHere :: InMenu (IO ())
runHere = do
    Just Item{_element} <- selectedItem
    case _element of
        Button f    -> return f
        Field f     -> do
          datum <- maybe "" (T.pack . reverse) <$> dataHere
          return $! f datum
        Toggle f    -> do
          datum <- not . fromMaybe False <$> dataHere
          return $! f datum
        ToMenu path -> do
          menu      .= path
          selection .= 0
          return (return ())

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

runEvent :: Event -> InMenu (IO ())
runEvent ev = case ev of
    Select     -> runHere
    Back       -> internally backOne
    MenuUp     -> internally (select succ)
    MenuDown   -> internally (select pred)
    _          -> return (return ())
  where
    internally f = f >> return (return ())
