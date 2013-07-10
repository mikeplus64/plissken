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

import Data.Vector.Storable ((//), (!))
import qualified Data.Foldable as F
import Data.Time

import Control.Lens hiding ((%=))

import OBJ
import Menu
import Engine
import Util hiding ((%=))
import Geometry
import Uniform
import Shader
import Game
import Paths

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

(%=) :: IORef a -> (a -> a) -> IO ()
(%=) = modifyIORef'

view', proj, viewProj :: M
viewProj = mXm proj view'
view'    = lookAt (vec3 10 10 10) (vec3 0 (-0.25) 0) (vec3 0 1 0)
proj     = perspective 30 1 0.001 100

main :: IO ()
main = do
    open <- newIORef True
    GLFW.initialize
    GLFW.openWindow GLFW.defaultDisplayOptions
        { GLFW.displayOptions_windowIsResizable = False
        , GLFW.displayOptions_numFsaaSamples    = Just 8
        }
    GLFW.enableAutoPoll
    GLFW.setWindowTitle "PLISSKEN"
    GLFW.setWindowSizeCallback $ \_ _ -> do
        glViewport 0 0 1024 1024
        GLFW.setWindowDimensions 1024 1024
    GLFW.setWindowCloseCallback (quit open)
    GLFW.setWindowDimensions 1024 1024


    mainmenu <- newMenu (mainMenu open) $ \ !sel !down !item !dyn -> do
        putStr (show sel)
        putChar (if down then '+' else ' ')
        T.putStr (_label item)
        T.putStr " -> "
        print dyn

    model'      <- newIORef (vec3 0 0 0)
    light'      <- newIORef (vec3 4 2 2)

    game' <- newGame $ newStage $
        [ (vec3 x y z, Brick)
        | x <- [0,19]
        , y <- [0,19]
        , z <- [0,19]
        ] ++
        [ (vec3 13 13 10, Good Grape)
        , (vec3 16 12 13, Good Grape)
        , (vec3 5  6  8, Good Grape)
        , (vec3 5  2 1, Good Grape)
        , (vec3 3  4  5, Good Grape)
        , (vec3 14 15 2, Bad Orange)
        ]

    GLFW.setKeyCallback $ \ key keyPress -> when keyPress $
        case key of
            CharKey c -> case c of
                'I' -> model' %= (+ vec3 0 0.1 0)
                'K' -> model' %= (+ vec3 0 (-0.1) 0)
                'L' -> model' %= (+ vec3 0.1 0 0)
                'J' -> model' %= (+ vec3 (-0.1) 0 0)
                'U' -> model' %= (+ vec3 0 0 (-0.1))
                'O' -> model' %= (+ vec3 0 0 0.1)

                'D' -> update game' (turnFlip toX)
                'A' -> update game' (turnFlip (-toX))
                'W' -> update game' (turnFlip toY)
                'S' -> update game' (turnFlip (-toY))
                'Q' -> update game' (turnFlip toZ)
                'E' -> update game' (turnFlip (-toZ))

                _   -> return ()
            _       -> return ()

    GLFW.setMousePositionCallback $ \ _ _ -> return ()

    glClearColor 0 0 0 1

    shroom      <- loadModel (Paths.model "shroom.obj")
    backcube    <- loadModel (Paths.model "invcube.obj")
    block       <- loadModel (Paths.model "block.obj")

    smoothBlock <- makeShaders (Paths.shader "smooth.vert") (Paths.shader "smooth.frag")
        $ umat4 "VP" 
        . umat4 "V"
        . umat4 "P"
        . ufloat "scale"
        . uvec3 "w_offset"
        . uvec3 "w_light_pos"
        . uvec4 "light_col"
        . uvec4 "diffuse"

    background <- makeShaders (Paths.shader "flat.vert") (Paths.shader "flat.frag")
        $ umat4 "VP" 
        . umat4 "V"
        . umat4 "P"
        . ufloat "scale"
        . uvec3 "w_offset"
        . uvec3 "w_light_pos"
        . uvec4 "light_col"
        . uvec4 "diffuse"

    putStrLn "loading models"

    i' <- newIORef 0
    start <- getCurrentTime
    logic <- forkIO $ while' start open $ \lastUpdate -> do
        threadDelay 50000 -- pause for 0.05 seconds
        now <- getCurrentTime
        if diffUTCTime now lastUpdate >= 0.2
          then do
            tick game'
            getCurrentTime
          else return lastUpdate

    glEnable gl_DEPTH_TEST
    glDepthFunc gl_LESS

    {-
    glEnable gl_CULL_FACE
    glCullFace gl_BACK

    glEnable gl_SMOOTH
    glEnable gl_BLEND
    glBlendFunc gl_ONE gl_ONE_MINUS_SRC_ALPHA
    -}


    while open $ do
        clear 

        model <- readIORef model'
        light <- readIORef light'
        i     <- readIORef i'
        game  <- readIORef game'

        let globalLight = vec4 0.95 0.8 0.7 400
            drawSmooth :: GLmodel -> V -> V -> IO ()
            drawSmooth m pos diffuse  =
              drawModel m smoothBlock
                viewProj
                view'
                proj
                0.09
                (0.2 `scale` pos)
                light
                globalLight
                diffuse

        F.for_ (game^?snake.body._head) $ \ (Ent pos _) ->
            drawSmooth block pos (vec4 0.3 1 0.3 1)

        F.for_ (game^.snake.body._tail) $ \ (Ent pos _) ->
            drawSmooth block pos (vec4 0.7 0.9 0.8 1)

        forStage (_stage game) $ \ pos b -> case b of
          Brick  -> drawSmooth block pos (vec4 0.6 0.5 0.5 1)
          Wood   -> drawSmooth block pos (vec4 0.1174 0.0745 0.04705 1)
          Water  -> drawSmooth block pos (vec4 0.2 1 0.4 0.4)
          Good _ -> drawSmooth shroom pos (vec4 0.2 0.2 1 1)
          Bad _  -> drawSmooth shroom pos (vec4 1 0.2 0.2 1)

        writeIORef light' $! vec3 (1.5 * sin (0.01 * i)) 6 (1.5 * cos (0.01 * i))

        i' %= succ
        
        -- Through experimentation I found GLFW.swapBuffers would take almost 
        -- exactly 1.666e-2 seconds to complete. This is 1/60 seconds! 
        -- ie, glfw is automatically capping the framerate to 60fps.
        -- To remove the cap use GLFW.setWindowBufferSwapInterval 0. (somewhat
        -- dangerous, as it will stress your GPU and CPU heavily)
        GLFW.swapBuffers

    writeIORef open False
    killThread logic
    GLFW.terminate

