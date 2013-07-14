{-# LANGUAGE OverloadedStrings, LambdaCase, BangPatterns #-}
import Data.IORef

import Control.Monad
import Control.Concurrent

import qualified Graphics.UI.GLFW as GLFW
import Graphics.UI.GLFW (Key(..))
import Graphics.Rendering.OpenGL.Raw

import Numeric.LinearAlgebra

import Data.Foldable (for_)
import Data.Time

import Control.Lens hiding ((%=))

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

import qualified Data.Vector.Storable as S

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

view', proj, viewProj :: M
viewProj = mXm proj view'
view'    = lookAt (vec3 10 10 10) (vec3 0 (-0.25) 0) (vec3 0 1 0)
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
keyCallback :: IORef GameS -> Scheme -> GLFW.Key -> Bool -> IO ()
keyCallback g s = callback
  where
    control      = withControls s
    sign         = fromIntegral . signum
    callback k b = when b . update g . control k $ \case
        AbsX 0 -> turnFlip (vec3 1 0 0)
        AbsY 0 -> turnFlip (vec3 0 1 0)
        AbsZ 0 -> turnFlip (vec3 0 0 1)
        AbsX i -> turn (vec3 (sign i) 0 0)
        AbsY i -> turn (vec3 0 (sign i) 0)
        AbsZ i -> turn (vec3 0 0 (sign i))
        _      -> return ()

main :: IO ()
main = do
    open   <- initGLFW 1024 1024

    model' <- newIORef (vec3 0 0 0)
    light' <- newIORef (vec3 4 2 2)

    game' <- newGame $ newStage $
        [ (vec3 x y z, Brick)
        | x <- [0, 9]
        , y <- [0, 9]
        , z <- [0,2,9]
        ]

    !config <- readConfig (Paths.config "config")
    let !controls = buildScheme config
    GLFW.setKeyCallback (keyCallback game' controls)

    putStrLn "loading models"
    shroom      <- loadModel (Paths.model "shroom.obj")
    bar         <- loadModel (Paths.model "bar.obj")
    block       <- loadModel (Paths.model "block.obj")

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
        . uvec4 "diffuse"

    putStrLn "init game"
    i' <- newIORef 0
    start <- getCurrentTime
    logic <- forkIO $ while' start open $ \lastUpdate -> do
        now <- getCurrentTime
        threadDelay 50000
        if diffUTCTime now lastUpdate >= 0.15
          then do
            tick game'
            getCurrentTime
          else return lastUpdate

    glEnable gl_DEPTH_TEST
    glDepthFunc gl_LESS

    glEnable gl_CULL_FACE
    glCullFace gl_BACK

    glClearColor 0 0 0 1

    glEnable gl_BLEND
    glBlendFunc gl_ONE gl_ONE_MINUS_SRC_ALPHA

    update game' (addFruit goodFruits)

    while open $ do
        modifyIORef' i' succ

        clear 

        model <- readIORef model'
        light <- readIORef light'
        i     <- readIORef i'
        game  <- readIORef game'

        let globalLight = vec4 0.95 0.8 0.7 400
            drawSmooth :: GLmodel -> V -> F -> V -> IO ()
            drawSmooth m pos scale diffuse =
              drawModel m smoothBlock
                viewProj
                view'
                proj
                scale
                pos
                light
                globalLight
                diffuse

            stage :: GLmodel -> V -> V -> IO ()
            stage m pos diffuse = drawSmooth m (0.4 `scale` pos) 0.2 diffuse

        -- Draw the snake's head in bright green, and its
        -- tail as a duller green.
        for_ (game^?snake.body._head) $ \ (Ent pos _) -> do
            stage block pos (vec4 0.3 1 0.3 1)
        for_ (game^.snake.body._tail) $ \ (Ent pos _) ->
            stage block pos (vec4 0.7 0.9 0.8 1)

        -- Remember to draw blocks semi-transparent.
        forStage (_stage game) $ \ pos b ->
            let colourMult = case game^?snake.body._head of
                  Just (Ent headPos _) | linedUp headPos pos
                                       -> vec4 1.5 1.5 1.5 1
                  _                    -> vec4 0.5 0.5 0.5 0.5
            in case b of
              Brick  -> stage block pos (colourMult * vec4 0.1 0.1 0.1 1.0)
              Wood   -> stage block pos (colourMult * vec4 0.1174 0.0745 0.04705 0.1)
              Water  -> stage block pos (colourMult * vec4 0.2 1 0.4 0.4)
              Good _ -> stage shroom pos (colourMult * vec4 0.2 0.2 1 1)
              Bad _  -> stage shroom pos (colourMult * vec4 1 0.2 0.2 1)

        for_ (game^?snake.body._head) $ \ (Ent pos _) -> do
            drawModel bar flat
              viewProj
              view'
              proj
              0.2
              (0.4 `scale` pos)
              (vec4 0.01 0.01 0.01 0.001)

        writeIORef light' $! vec3 (1.5 * sin (0.01 * i)) 6 (1.5 * cos (0.01 * i))

        -- Through experimentation I found GLFW.swapBuffers would take almost 
        -- exactly 1.666e-2 seconds to complete. This is 1/60 seconds! 
        -- ie, glfw is automatically capping the framerate to 60fps.
        -- To remove the cap use GLFW.setWindowBufferSwapInterval 0. (somewhat
        -- dangerous, as it will stress your GPU and CPU heavily)
        GLFW.swapBuffers

    writeIORef open False
    killThread logic
    GLFW.terminate
