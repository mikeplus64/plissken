{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, NamedFieldPuns, RecordWildCards, BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Menu where
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as I
import qualified Data.Text as T
import qualified Data.Foldable as F
import Data.Dynamic
import Data.Dynamic.Lens
import Data.Maybe
import Data.IORef
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Applicative
import Control.Concurrent
import Control.Lens hiding (simple, element)
import Geometry (V3)

data Element
  = Button          { _press       :: !(IO ()) }
  | Field           { _enter       :: !(T.Text -> IO ()) }
  | Toggle          { _toggle      :: !(Bool -> IO ()) }
  | ToMenu          { _targetMenu  :: [T.Text] }
  | CloseButton

instance Show Element where
    show (Button _) = "Button"
    show (Field _)    = "Field"
    show (Toggle _)       = "Toggle"
    show (ToMenu t)       = "ToMenu " ++ show t
    show CloseButton      = "CloseButton"

type Colour = V3

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

type Menu = M.Map [T.Text] (I.IntMap Item)

type InMenu = StateT MenuS (ReaderT Menu IO)

runMenu :: InMenu a -> MenuS -> Menu -> IO a
runMenu f = runReaderT . evalStateT f

data MenuEvent
    = Select !Int
    | SelectUp
    | SelectDown

    | Empty
    | Insert !Char

    | Enter
    | Exit
    | Resume
    | Restart
    | BackMenu
    | Kill
  deriving (Show,Read,Eq)

makeLenses ''Element
makeLenses ''Item
makeLenses ''MenuS

item :: T.Text -> Element -> Item
item l f = Item
    { _label        = l
    , _element      = f
    , _fontColour   = 0.0
    , _hovered      = 0.8
    , _bgColour     = 0.9
    , _pressed      = 1.0
    }

close :: Item
close = Item
    { _label        = "Close"
    , _element      = CloseButton
    , _fontColour   = 0.0
    , _hovered      = 0.8
    , _bgColour     = 0.9
    , _pressed      = 1.0
    }

(=:) :: k -> a -> (k, a)
(=:) = (,)

subm :: [T.Text] -> [Item] -> ([T.Text], I.IntMap Item)
subm texts items = (texts, I.fromList (zip [0..] items))

mkMenu :: [([T.Text], I.IntMap Item)] -> Menu
mkMenu = M.fromList

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
        Button f -> return f
        Field f    -> f <$> maybe "" (T.pack . reverse) <$> dataHere
        Toggle f       -> f <$> not.fromMaybe False <$> dataHere
        ToMenu path    -> do
            menu .= path
            selection .= 0
            return (return ())
        _              -> return (return ())

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

data GameMenu = GM
    { menuEvents  :: !(Chan MenuEvent)
    , menuResults :: !(Chan (IO ()))
    , menuRender  :: !(IORef (IO ()))
    , menuIsOpen  :: !(IORef (Maybe Bool))
    , menuThread  :: !ThreadId
    }

{-# INLINE sendMenu #-}
sendMenu :: MenuEvent -> GameMenu -> IO ()
sendMenu e gm = writeChan (menuEvents gm) e

whenOpen :: GameMenu -> (GameMenu -> IO ()) -> IO ()
whenOpen gm f = do
    isOpen <- readIORef (menuIsOpen gm)
    case isOpen of
        Just True -> f gm
        _         -> return ()

{-# INLINE drawMenu #-}
-- | Silently "fails" if there is no menu to draw
drawMenu :: GameMenu -> IO ()
drawMenu gm = join (readIORef (menuRender gm))

{-# INLINE newMenu #-}
newMenu :: Menu -> (Int -> Bool -> Item -> Maybe Dynamic -> IO ()) -> IO GameMenu
newMenu m r = do
    mevs  <- newChan
    mres  <- newChan
    frame <- newIORef (return ())
    open  <- newIORef (Just True)
    pid   <- forkIO (renderMenu r mevs mres frame open m)
    forkIO (forever (join (readChan mres)))
    return (GM mevs mres frame open pid)

{-# INLINE renderMenu #-}
renderMenu
    :: (Int -> Bool -> Item -> Maybe Dynamic -> IO ())  -- ^ Used to render individual items, given the item's index, and whether it is selected
    -> Chan MenuEvent                  -- ^ Chan of events
    -> Chan (IO ())                    -- ^ Chan of results from pressing buttons, or performing actions in the menu generally
    -> IORef (IO ())                   -- ^ Action to render the current menu with
    -> IORef (Maybe Bool)
    -> Menu
    -> IO ()
renderMenu render events results rendering open
    = runMenu go originalMenuS
  where
    originalMenuS = MenuS [] 0 M.empty

    waitForResume :: InMenu ()
    waitForResume = do
        event <- liftIO (readChan events)
        case event of
            Resume -> return ()
            _      -> waitForResume

    toggleOpen = liftIO (modifyIORef' open (fmap not))

    go :: InMenu ()
    go = do
        here    <- use menu
        sel     <- use selection
        datas   <- use elementData
        submenu <- view (at here)
        liftIO $! writeIORef rendering $! F.for_ submenu $! imapM_ (\ !sel' !item' ->
            render 
                sel' 
                (sel' == sel) 
                item'
                $! M.lookup here datas >>= I.lookup sel')

        event   <- liftIO (readChan events)
        case event of
            Kill        -> liftIO (writeIORef open Nothing)
            Exit        -> toggleOpen >> waitForResume >> toggleOpen >> go
            Resume      -> error "Not currently exited."
            Restart     -> put originalMenuS >> go
            BackMenu    -> backOne >> go
            SelectUp    -> select pred >> go
            SelectDown  -> select succ >> go
            Select i    -> select (const i) >> go
            Insert t    -> overHere [t] (t:) >> go
            Empty       -> deleteDataHere >> go
            Enter       -> do
                !action <- runHere
                liftIO (writeChan results action)
                go

