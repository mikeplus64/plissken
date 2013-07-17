{-# LANGUAGE OverloadedStrings, LambdaCase, BangPatterns, ScopedTypeVariables, RankNTypes #-}
module Main where
import Data.IORef

import Control.Monad
import Control.Monad.State.Strict
import Control.Concurrent

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (Key(..))
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.FTGL

import qualified Data.Vector.Storable as S
import qualified Data.ByteString as B
import Data.Foldable (for_)
import Data.Typeable
import Data.Dynamic
import Data.Time
import Data.Maybe

import Foreign
import Foreign.C

import Control.Lens

import Menu
import Engine
import Util
import Geometry
import Uniform
import Shader
import Game
import Save
import Controls
import qualified Paths

import Text.Read (readMaybe)


quit :: IORef Bool -> IO Bool
quit open = do
    writeIORef open False
    return True

view', viewMenu, proj, viewProj :: M
viewProj = mXm proj view'
view'    = lookAt (vec3 10 10 10) (vec3 0 (-0.25) 0) (vec3 0 1 0)
viewMenu = lookAt (vec3 0 0 20) (vec3 0 0 13) (vec3 0 1 0)
vmproj   = mXm proj viewMenu
proj     = perspective 30 1 0.001 100

initGLFW :: Int -> Int -> IO (IORef Bool)
initGLFW width height = do
    open    <- newIORef True
    hasInit <- GLFW.initialize
    isOpen  <- GLFW.openWindow GLFW.defaultDisplayOptions
        { GLFW.displayOptions_windowIsResizable = False
        , GLFW.displayOptions_numFsaaSamples    = Just 8
        }
    unless (hasInit && isOpen) (error "Could not initialize GLFW")
    GLFW.enableAutoPoll
    GLFW.setWindowTitle "PLISSKEN"
    GLFW.setWindowSizeCallback $ \_ _ -> do
        glViewport 0 0 (fromIntegral width) (fromIntegral height)
        GLFW.setWindowDimensions width height
    GLFW.setWindowCloseCallback (quit open)
    GLFW.setWindowDimensions width height
    return open

{-# INLINE keyCallback #-}
keyCallback :: IORef GameS -> IORef MenuS -> Menu -> Scheme -> GLFW.Key -> Bool -> IO ()
keyCallback g' m' m s = callback
  where
    control = withControls s
    sign    = fromIntegral . signum
    callback k down = when down $ control k $ \event -> do
      case event of
        ToggleMenu -> update g' (gameIsPaused %= not)
        _          -> do
          paused <- isPaused g'
          if paused
            then id =<< runMenu m' m (runEvent event)
            else update g' $! case event of
              AbsX 0     -> turnFlip (vec3 1 0 0)
              AbsY 0     -> turnFlip (vec3 0 1 0)
              AbsZ 0     -> turnFlip (vec3 0 0 1)
              AbsX i     -> turnFlip (vec3 (sign i) 0 0)
              AbsY i     -> turnFlip (vec3 0 (sign i) 0)
              AbsZ i     -> turnFlip (vec3 0 0 (sign i))
              Flamethrow -> flamethrow
              _          -> return ()

{-# INLINE mainMenu #-}
mainMenu :: (Game () -> IO ()) -> IO Menu
mainMenu upd = newMenu $ do
    topMenu $ do
        button' "start" $ gameIsPaused %= not
        link   "difficulty" ["difficulty"]
    subMenu ["difficulty"] $ do
        toggle' "die on wall collisions?"  (difficulty.dieAtWall .=)
        toggle' "die from self collision?" (difficulty.selfCollisions .=)
        showed' "game tick delta" 0.15 addToSpeed (difficulty.speed .=)
        back "back"
  where
    toggle' s f = toggle s (upd . f)
    button' s f = button s (upd f)
    showed' s z f cc = showed s z f (upd . cc)

main :: IO ()
main = do
    open   <- initGLFW 1024 1024

    model' <- newIORef (vec3 0 0 0)
    light' <- newIORef (vec3 4 2 2)

    putStrLn "reading user config"
    !config <- readConfig (Paths.config "config")
    let !controls  = buildScheme config

    putStrLn "starting game"
    game' <- newGame $ newStage $
        [ (vec3 x y z, Brick)
        | x <- [0, 9]
        , y <- [0, 9]
        , z <- [0,2,9]
        ]

    putStrLn "init menu system"
    loadedMenu <- mainMenu (update game')
    menuS      <- newIORef startingMenuS

    putStrLn "set control scheme"
    GLFW.setKeyCallback (keyCallback game' menuS loadedMenu controls)

    putStrLn "loading models"
    shroom       <- loadModel (Paths.model "shroom.obj")
    grapes       <- loadModel (Paths.model "grapes.obj")
    apple        <- loadModel (Paths.model "apple.obj")
    orange       <- loadModel (Paths.model "orange.obj")
    block        <- loadModel (Paths.model "block.obj")
    square       <- loadModel (Paths.model "menu.obj")

    putStrLn "compiling shaders"
    smoothBlock <- makeShaders (Paths.shader "smooth.vert") (Paths.shader "smooth.frag")
        $ umat4 "VP" 
        . umat4 "V"
        . umat4 "P"
        . ufloat "scale"
        . uvec3 "w_offset"
        . uvec3 "w_light_pos"
        . uvec4 "light_col"
        . uvec4 "diffuse"
    flat <- makeShaders (Paths.shader "flat.vert") (Paths.shader "flat.frag")
        $ umat4 "VP"
        . umat4 "V"
        . umat4 "P"
        . ufloat "scale"
        . uvec3 "w_offset"
        . uvec3 "w_light_pos"
        . uvec4 "light_col"
        . uvec4 "diffuse"

    putStrLn "loading fonts"
    terminus <- createBitmapFont (Paths.fonts "TerminusBold.ttf")
    setFontFaceSize terminus 24 72

    putStrLn "init game"
    update game' restart

    putStrLn "starting game timer loop"
    lock  <- newEmptyMVar
    timer <- forkIO $ forever $ do
        d <- gameField game' (difficulty.speed)
        threadDelay (floor (1000000*d))
        putMVar lock ()

    putStrLn "starting game logic loop"
    start <- getCurrentTime
    logic <- forkIO $ forever $ do
        takeMVar lock
        tick game'

    putStrLn "main loop"
    i' <- newIORef 0 -- frame counter
    while open $ do
        clear

        glEnable gl_DEPTH_TEST
        glDepthFunc gl_LESS

        glEnable gl_CULL_FACE
        glCullFace gl_BACK

        glEnable gl_BLEND
        glBlendFunc gl_ONE gl_ONE_MINUS_SRC_ALPHA

        modifyIORef' i' succ

        light <- readIORef light'
        di    <- readIORef i'
        game  <- readIORef game'

        let globalLight = vec4 0.95 0.8 0.7 400
            drawSmooth :: GLmodel -> V -> F -> V -> IO ()
            drawSmooth m pos scaler diffuse =
              drawModel m smoothBlock
                viewProj
                view'
                proj
                scaler
                pos
                light
                globalLight
                diffuse

            stageM :: GLmodel -> V -> V -> IO ()
            stageM m pos = drawSmooth m (0.4 `scale` pos) 0.2

        -- Draw the snake's head in bright green, and its
        -- tail as a duller green.
        for_ (game^?snake.body._head) $ \ (Ent pos _) ->
            stageM block pos (vec4 0.3 1 0.3 1)

        for_ (game^.snake.body._tail) $ \ (Ent pos _) ->
            stageM block pos (vec4 0.7 0.9 0.8 1)

        -- Remember to draw blocks semi-transparent.
        forStage (_stage game) $ \ pos b ->
            let snakeHeadPos = game^?snake.body._head.position
                colourMult   = case snakeHeadPos of
                    Just headPos
                        | linedUp headPos pos
                        -> vec4 1.5 1.5 1.5 1
                    _   -> vec4 0.6 0.6 0.6 0.8
                fruitMult    = case snakeHeadPos of
                    Just headPos
                        | linedUp headPos pos
                        -> vec4 1.5 1.5 1.5 1
                    _   -> vec4 0.6 0.6 0.6 1
            in case b of
                Brick       -> stageM block  pos (colourMult * vec4 0.1 0.1 0.1 1.0)
                Wood        -> stageM block  pos (colourMult * vec4 0.1174 0.0745 0.04705 1.0)
                Water       -> stageM block  pos (colourMult * vec4 0.2 0.2 0.4 0.3)
                Bad _       -> stageM shroom pos (colourMult * vec4 1 0.2 0.2 1.7)
                Good Apple  -> stageM apple  pos (fruitMult * vec4 0.4470 0.7294 3.6036e-3 1)
                Good Orange -> stageM orange pos (fruitMult * vec4 0.91764706 0.4 9.009009e-3 1)
                Good Grape  -> stageM grapes pos (fruitMult * vec4 1.0 0.25882354 0.12432432 1)

        -- draw the score
        renderText terminus 0.75 0.9 "score:"
        withCString (show (_score game))
            $ renderText terminus 0.9 0.9

        -- draw the "message"
        for_ (_message game) (`withCString` renderText terminus (-0.25) (-0.5))

        -- draw the menu
        when (_gameIsPaused game) $ do
            ms <- readIORef menuS
            let sel = 1.4 * fromIntegral (ms^.selection)

            drawModel block smoothBlock
                vmproj
                viewMenu
                proj
                2.7
                (vec3 0 0 9)
                (vec3 0 sel 0.5)
                (vec4 0.04 0.05 0.1 1.0 * globalLight)
                (vec4 0.1 0.1 0.1 1)

            ifor_ (loadedMenu^.ix (ms^.menu)) $ \ y itm -> do
                -- when this menu item is selected, have a little arrow next to it
                when (ms^.selection == y) $
                    renderText terminus (-0.12) (fromIntegral y*0.175) ">"
                case itm^.ielement of
                    Field _ getValue -> do
                        val <- getValue
                        renderText terminus 0.75 (fromIntegral y*0.175) "="
                        B.useAsCString val $
                            renderText terminus 0.8 (fromIntegral y*0.175)
                    _ -> return ()

                -- finally, the label
                renderLabel terminus 0 (fromIntegral y*0.175) (itm^.label)

        writeIORef light' $! vec3 (1.5 * sin (0.01 * di)) 6 (1.5 * cos (0.01 * di))
        
        -- Through experimentation I found GLFW.swapBuffers would take almost 
        -- exactly 1.666e-2 seconds to complete. This is 1/60 seconds! 
        -- ie, glfw is automatically capping the framerate to 60fps.
        -- To remove the cap use GLFW.setWindowBufferSwapInterval 0. (somewhat
        -- dangerous, as it will stress your GPU and CPU heavily)
        GLFW.swapBuffers

    writeIORef open False
    killThread logic
    killThread timer
    GLFW.terminate
