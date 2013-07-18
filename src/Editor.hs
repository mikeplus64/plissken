{-# LANGUAGE OverloadedStrings, TemplateHaskell, BangPatterns, RankNTypes #-}
module Editor where
import Control.Monad.State.Strict
import Control.Applicative
import Control.Lens

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B
import Data.IORef

import Geometry
import Game
import Controls
import Util(sign)

import Debug.Trace

data Editor = Editor
    { _cursor     :: !V
    , _currBlock  :: !Block
    , _stageName  :: !B.ByteString
    , _editorOpen :: !Bool
    , _editStage  :: !Stage
    , _editHelp   :: !(Maybe B.ByteString)
    , _savePath   :: !FilePath
    } deriving (Show,Read,Eq)
makeLenses ''Editor

type Edit = State Editor

newEditor :: IO (IORef Editor)
newEditor = newIORef =<< initEditor

edit :: IORef Editor -> Edit a -> IO a
edit editor' e = do
    editor0 <- readIORef editor'
    case runState e editor0 of
        (a, editor1) -> do
            writeIORef editor' $! editor1
            return a

edit_ :: IORef Editor -> Edit a -> IO ()
edit_ editor' e = do
    editor0 <- readIORef editor'
    case execState e editor0 of
        editor1 -> writeIORef editor' $! editor1

placeBlock ::  Edit ()
placeBlock = do
    target <- use cursor
    block  <- use currBlock
    editStage %= M.insert target block

offsetCursor :: V -> Edit ()
offsetCursor offset'
    = cursor %= wrapAdd offset'

setCursor :: V -> Edit ()
setCursor c = cursor .= wrap c

initEditor :: IO Editor
initEditor = return $! Editor (vec3 4 4 4) Brick "level" False M.empty Nothing ""

saveStage :: Editor -> FilePath -> IO ()
saveStage e path = writeFile path . show $ e^.editStage

loadStage :: FilePath -> IO Stage
loadStage path = do
    content <- B.readFile path
    return $! read $! B.unpack content

editorStageToGame :: IORef GameS -> IORef Editor -> IO ()
editorStageToGame game' editor' = do
    estage <- view editStage <$> readIORef editor'
    modifyIORef' game' (stage .~ estage)

gameStageToEditor :: IORef GameS -> IORef Editor -> IO ()
gameStageToEditor game' editor' = do
    gstage <- view stage <$> readIORef game'
    modifyIORef editor' (editStage .~ gstage)

place4x4 :: Edit ()
place4x4 = do
    c     <- use cursor
    block <- use currBlock
    editStage %= M.union
        (gen4x4
            (floor $! c @> 0)
            (floor $! c @> 1)
            (floor $! c @> 2)
            block)

deleteThis :: Edit ()
deleteThis = do
    c <- use cursor
    editStage %= M.delete c

delete4x4 :: Edit ()
delete4x4 = do
    c <- use cursor
    editStage %= flip M.difference
        (gen4x4
           (floor $! c @> 0)
           (floor $! c @> 1)
           (floor $! c @> 2)
           Brick)

notifyE :: Show a => Lens' Editor a -> Edit ()
notifyE llens = do
    !d <- B.pack . show <$> use llens
    editHelp .= Just d
    
runEditEvent :: Event -> Edit ()
runEditEvent event = case event of
    Select     -> placeBlock
    Flamethrow -> place4x4
    MenuUp     -> currBlock %= blockAfter >> notifyE currBlock
    MenuDown   -> currBlock %= blockBefore >> notifyE currBlock
    AbsX i     -> offsetCursor (vec3 (sign i) 0 0)
    AbsY i     -> offsetCursor (vec3 0 (sign i) 0)
    AbsZ i     -> offsetCursor (vec3 0 0 (sign i))
    Delete     -> deleteThis
    Delete4x4  -> delete4x4
    _          -> return ()
