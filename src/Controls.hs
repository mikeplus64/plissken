{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Controls where
import qualified Data.Map.Strict     as M
import qualified Data.HashMap.Strict as H

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (Key(..), MouseButton(..))



import Control.Concurrent
import Control.Applicative
import Data.IORef

import Data.Int
import Data.Text (Text, append)

import Text.Read (readMaybe)

import Game
import Menu

buildScheme :: Config -> Config -> Maybe Scheme
buildScheme scheme defs = do
    return undefined
  where
    contKey :: Text -> Key
    contKey c = Key ["controls",c] TT

    lookupKeyEvent :: Text -> Maybe (Key, Event)
    lookupKeyEvent m' = do
        k' <- M.lookup (contKey m') scheme <|> M.lookup (contKey m') defs
        k  <- readMaybe k'
        m  <- readMaybe m'
        return (k, m)


data Scheme = Scheme
    { _schemeName    :: !String
    , _controls      :: !(Map Key Event)
    }

data Event
    = ToggleMenu
    | SelectionDown
    | SelectionUp
    | SelectionHelp
    | Select
    | Back
    | TurnUp
    | TurnDown
    | TurnLeft
    | TurnRight
    | AbsX !Int8
    | AbsY !Int8
    | AbsZ !Int8
  deriving (Show,Read,Eq,Ord,Enum)

{-
type MouseWheel = Int

data MB = Down !MouseButton 
        | Up !MouseButton

data Mouse = Mouse
    { mousePos    :: !(IORef (Int,Int))
    , mouseWheel  :: !(IORef MouseWheel)
    , mouseButton :: !(IORef MB)
    }

data InputStream = IS
    { mouse :: !Mouse
    , keys  :: !(Chan Key)
    }

mouseButtonCallback :: InputStream -> MouseButton -> Bool -> IO ()
mouseButtonCallback IS{ mouse = Mouse{mouseButton} } button down
  = writeIORef mouseButton $! 
      if down then Down button else Up button

mousePositionCallback :: InputStream -> Int -> Int -> IO ()
mousePositionCallback IS{ mouse = Mouse{mousePos} } x y 
  = writeIORef mousePos (x, y)

mouseWheelCallback :: InputStream -> Int -> IO ()
mouseWheelCallback IS{ mouse = Mouse{mouseWheel} }
  = writeIORef mouseWheel
-}
