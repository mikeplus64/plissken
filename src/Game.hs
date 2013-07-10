{-# LANGUAGE TemplateHaskell, BangPatterns, FlexibleInstances, RecordWildCards #-}
module Game where
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Sequence.Lens
import Data.Sequence (ViewL(..), ViewR(..), (|>), (<|), Seq)

import qualified Data.Foldable as F
import Data.Foldable (for_)

import Control.Monad.State.Strict
import Control.Arrow
import Data.Maybe

import Control.Lens hiding ((<|), (|>))
import Prelude hiding (head, last)
import Data.IORef

import Geometry hiding ((<|), (|>), step)
import Menu

type Game = StateT GameS Maybe

toX, toY, toZ :: V
toX = vec3 1 0 0
toY = vec3 0 1 0
toZ = vec3 0 0 1

(!.) :: s -> State s a -> s
(!.) = flip execState

infixr 0 !.

type Pos = V
type Vel = V

type Positioned = M.Map Pos
type OrderedPos = []

data Entity = Ent
    { _position     :: !Pos
    , _velocity     :: !Vel
    } deriving (Show, Read, Eq)

type Body = S.Seq Entity

data Flame = Flame
    { _lifeLeft     :: !Int
    , _fent         :: !Entity
    } deriving (Show, Read, Eq)

data Snake = Snake
    { _body         :: !(Seq Entity)
    , _flame        :: !(Seq Flame)
    } deriving (Show, Read, Eq)

data Fruit
    = Grape
    | Apple
    | Orange
  deriving (Eq, Show, Read, Ord, Enum)

data Block
    = Brick
    | Wood
    | Water
    | Good !Fruit
    | Bad !Fruit
  deriving (Eq, Show, Read, Ord)

type Stage = Positioned Block

data GameS
    = Game  { _stage         :: !Stage
            , _stageBounds   :: !(Int,Int)
            , _snake         :: !Snake
            , _score         :: !Int
            , _gameTicks     :: !Integer
            , _message       :: !(Maybe String) 
            , _gameIsPaused  :: !Bool
            } deriving (Show,Read,Eq)

makeLenses ''Flame
makeLenses ''Entity
makeLenses ''GameS
makeLenses ''Snake

endGame :: Game ()
endGame = lift Nothing

pushEnt :: Entity -> Entity
pushEnt (Ent p v) = Ent (cmap (`mod'` 20) (p+v)) v
  where
    mod' x y = fromIntegral ((floor x :: Int) `mod` y)

pushBody :: Body -> Body
pushBody b 
  = case S.viewl b of
      snakeHead :< _ ->
        case S.viewr b of
          before :> _ -> pushEnt snakeHead <| before 
          _           -> S.empty
      _ -> S.empty

selfImmolated :: Snake -> Bool
selfImmolated (Snake b f) 
  = F.any (\(Flame _ e) -> e `F.elem` b) f

stepSnakeAlone :: Snake -> Maybe Snake
stepSnakeAlone !orig
  | selfImmolated new = Nothing
  | otherwise         = Just new
  where 
    !new = orig 
         & body %~ pushBody
         & flame.mapped.fent %~ pushEnt

collided :: Snake -> Stage -> [(Pos, Block)]
collided (Snake body' _) level 
  = [] !. for_ body' $ \(Ent pos _) -> 
        case M.lookup pos level of
            Just b -> modify ((pos,b):)
            _      -> return ()

turn :: Vel -> Game ()
turn v = snake.body._head.velocity .= v

end :: String -> Game ()
end s = do
    message .= Just s
    lift Nothing

{-# INLINE stepGame #-}
stepGame :: Game ()
stepGame = do
    False         <- use gameIsPaused
    Just newSnake <- uses snake stepSnakeAlone
    collisions    <- uses stage (collided newSnake)
    snake        .= newSnake

    for_ collisions $ \ (pos, block) -> case block of
        -- if it's OK fruit, add a block to the end of the snake
        -- delete the block in the stage too
        Good _ -> do
            stage.at pos .= Nothing
            Just oldLast <- preuse (snake.body._last)
            score        += 1
            snake.body   %= (|> oldLast)

        -- remove the last segment of the snake if the fruit was off
        -- delete the block in the stage too
        Bad _ -> do
            stage.at pos .= Nothing
            snake.body   %= \b -> case S.viewr b of
                before :> _ -> before
                _           -> b

        Water -> return ()

        -- if it's anything else, colliding with it is deadly.
        _     -> end "Killed by a head-on collision!"

    level    <- use stage
    oldFlame <- use (snake.flame)
    snake.flame .= S.empty

    -- delete blocks depending on whether they've been flamed and their type
    -- and incrementally rebuild snake.flame
    for_ oldFlame $ \ f@(Flame _ (Ent _ pos)) ->
        case M.lookup pos level of
            Just block -> case block of
                Wood  -> stage.at pos .= Nothing
                Water -> stage.at pos .= Nothing
                Brick -> return ()
                _     -> do
                    stage.at pos .= Nothing
                    snake.flame %= (|> f)
            _ -> snake.flame %= (|> f)

    gameTicks += 1


newStage :: [(Pos,Block)] -> Stage
newStage = M.fromList

newGame :: Stage -> IO (IORef GameS)
newGame _stage = do
    let _snake = Snake (S.singleton (Ent (vec3 4 0 0) (vec3 1 0 0))) S.empty
        _score = 0
        _gameTicks = 0
        _message = Nothing
        _gameIsPaused = False
        _stageBounds = (19,19)
    newIORef Game{..}

update :: IORef GameS -> Game a -> IO ()
update s' f = do
    s <- readIORef s'
    case execStateT (stepGame >> f) s of
        Just x -> do
          print x
          writeIORef s' $! x
        _      -> putStrLn "no update?"

tick :: IORef GameS -> IO ()
tick s' = do
    s <- readIORef s'
    case execStateT stepGame s of
        Just x -> do
          print x
          writeIORef s' $! x
        _      -> putStrLn "no update?"

forStage :: Stage -> (V -> Block -> IO ()) -> IO ()
forStage s f = void (M.traverseWithKey f s)

turnFlip :: V -> Game ()
turnFlip new = do
    now <- use (snake.body._head.velocity)
    if now == new
      then snake.body._head.velocity .= -new
      else snake.body._head.velocity .= new

