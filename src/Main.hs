{-# LANGUAGE OverloadedStrings, BangPatterns #-}
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.IORef
import Data.Function
import Data.Monoid

import Control.Monad
import Control.Concurrent

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (Key(..), MouseButton(..))
import Graphics.Rendering.OpenGL.Raw

import Numeric.LinearAlgebra
import Control.Lens

import System.Environment
import System.Posix.Signals

import OBJ
import Menu
import Engine
import Util
import Geometry
import Uniform

mainMenu :: IORef Bool -> Menu
mainMenu open = mkMenu
    [ subm []
        [ item "options" (ToMenu ["options"])
        , item "quit" (Button (void (quit open)))
        ]
    , subm ["options"]
        [ item "player" (ToMenu ["options","player"])
        ]
    , subm ["options","player"]
        [ item "player" (ToMenu ["options","player"])
        ]
    ]

quit :: IORef Bool -> IO Bool
quit open = do
    writeIORef open False
    return True

main :: IO ()
main = do
    open <- newIORef True
    GLFW.initialize
    GLFW.openWindow GLFW.defaultDisplayOptions
        { GLFW.displayOptions_windowIsResizable = False
        }
    GLFW.enableKeyRepeat
    GLFW.enableAutoPoll
    GLFW.setWindowCloseCallback (quit open)
    GLFW.setWindowTitle "PLISSKEN"
    GLFW.setWindowSizeCallback $ \_ _ -> do
        glViewport 0 0 512 512
        GLFW.setWindowDimensions 512 512
    GLFW.setWindowDimensions 512 512

    bevelCube <- loadModel "data/models/bevelcube.obj" "data/models/basic.vert" "data/models/background.frag" $ \prog -> do
        mvp <- newUVec4 prog "mvp"
        return (UMat4 mvp Uz)

    mainmenu <- newMenu (mainMenu open) $ \ !sel !down !item !dyn -> do
        putStr (show sel)
        putChar (if down then '+' else ' ')
        T.putStr (_label item)
        T.putStr " -> "
        print dyn

    camera'     <- newIORef (vec3 2 2 2)
    -- target'     <- newIORef (vec3 0 0 0)
    up'         <- newIORef (vec3 0 1 0)
    model'      <- newIORef (vec3 0 0 0)
    view'       <- newIORef (lookAt (- vec3 2 2 2) (vec3 0 0 0) (vec3 0 1 0))
    projection' <- newIORef (perspective 90 1 0.001 100)
    mvp         <- newIORef 0

    let target'    = model'
        updateView = do
            camera <- readIORef camera'
            target <- readIORef target'
            up     <- readIORef up'
            writeIORef view' $! lookAt camera target up

        updateMVP  = do
            model  <- readIORef model'
            view   <- readIORef view'
            proj   <- readIORef projection'
            writeIORef mvp $! proj `mXm` view `mXm` translation model

    updateView
    updateMVP

    GLFW.setKeyCallback $ \ key keyPress -> when keyPress $ do
        case key of 
            CharKey c -> case c of
                'I' -> model' `modifyIORef'` (+ vec3 0 0.1 0)
                'K' -> model' `modifyIORef'` (+ vec3 0 (-0.1) 0)
                'L' -> model' `modifyIORef'` (+ vec3 0.1 0 0)
                'J' -> model' `modifyIORef'` (+ vec3 (-0.1) 0 0)
                'U' -> model' `modifyIORef'` (+ vec3 0 0 (-0.1))
                'O' -> model' `modifyIORef'` (+ vec3 0 0 0.1)
                _   -> return ()
            _       -> return ()
        updateView
        updateMVP

    GLFW.setMousePositionCallback $ \ mx my -> do
        return ()


    {-
        when keyPress $ print () >> case key of
            KeyUp        -> modifyIORef' centre' (_z +~ 0.1)
            KeyDown      -> modifyIORef' centre' (_z -~ 0.1)
            KeyRight     -> modifyIORef' centre' (_x +~ 0.1)
            KeyLeft      -> modifyIORef' centre' (_x -~ 0.1)
            KeySpace     -> modifyIORef' centre' (_y +~ 0.1)
            CharKey 'c'  -> modifyIORef' centre' (_y -~ 0.1)
            _            -> return ()
        when keyPress $ whenOpen men $ case key of
            KeyUp        -> sendMenu SelectUp
            KeyDown      -> sendMenu SelectDown
            KeyLeft      -> sendMenu BackMenu
            KeyRight     -> sendMenu Enter
            KeyEnter     -> sendMenu Enter
            CharKey c    -> sendMenu (Insert c)
            KeyBackspace -> sendMenu Empty
            _            -> const (return ()
            )
    -}

    glClearColor 0.5 0.6 0.7 1
    glEnable gl_DEPTH_TEST
    glDepthFunc gl_LESS
    glEnable gl_CULL_FACE

    while open (do
        clear 

        drawModel bevelCube =<< readIORef mvp


        -- Through experimentation I found GLFW.swapBuffers would take almost 
        -- exactly 1.666e-2 seconds to complete. This is 1/60 seconds! 
        -- ie, glfw is automatically capping the framerate to 60fps.
        -- To remove the cap use GLFW.setWindowBufferSwapInterval 0. (somewhat
        -- dangerous, as it will stress your GPU and CPU heavily)
        GLFW.swapBuffers)

    GLFW.terminate

