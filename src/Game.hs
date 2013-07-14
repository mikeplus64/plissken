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

import System.Random
import Geometry hiding ((<|), (|>), step)

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
            , _stageBounds   :: !Int
            , _snake         :: !Snake
            , _score         :: !Int
            , _gameTicks     :: !Integer
            , _message       :: !(Maybe String) 
            , _gameIsPaused  :: !Bool
            , _stdGen        :: !StdGen
            } deriving (Show,Read)

makeLenses ''Flame
makeLenses ''Entity
makeLenses ''GameS
makeLenses ''Snake

rand :: (Int,Int) -> Game Int
rand bounds = do
  gen' <- use stdGen
  case randomR bounds gen' of
    (i,gen) -> do
      stdGen .= gen
      return i

intToBlock :: Int -> Block
intToBlock i = case i of
  0 -> Brick
  1 -> Wood
  2 -> Water
  3 -> Good Grape
  4 -> Good Apple
  5 -> Good Orange
  6 -> Bad Grape
  7 -> Bad Apple
  8 -> Bad Orange
  _ -> error "intToBlock: invalid i"

-- | the number returned is an integer, but is stored floating point
randPos :: Game Pos
randPos = do
    x <- fromIntegral `fmap` rand (0,9)
    y <- fromIntegral `fmap` rand (0,9)
    z <- fromIntegral `fmap` rand (0,9)
    return (vec3 x y z)

addFruit :: (Int,Int) -> Game ()
addFruit fruitType = do
    pos <- randPos 
    stg <- use stage
    if M.member pos stg
      then addFruit fruitType -- simply try again
      else do
        block' <- intToBlock `fmap` rand fruitType
        stage  %= M.insert pos block'

goodFruits,badFruits,anyFruits :: (Int,Int)
goodFruits = (3,5)
badFruits  = (6,8)
anyFruits  = (3,8)
 
endGame :: Game ()
endGame = lift Nothing

pushEnt :: Entity -> Entity
pushEnt (Ent p v) = Ent (cmap (`mod'` 10) (p+v)) v
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
            addFruit goodFruits

        -- remove the last segment of the snake if the fruit was off
        -- delete the block in the stage too
        Bad _ -> do
            stage.at pos .= Nothing
            snake.body   %= \b -> case S.viewr b of
                before :> _ -> before
                _           -> b
            addFruit anyFruits

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
    let _snake = Snake (S.singleton (Ent (vec3 4 4 4) (vec3 1 0 0))) S.empty
        _score = 0
        _gameTicks = 0
        _message = Nothing
        _gameIsPaused = False
        _stageBounds = 10
    _stdGen <- newStdGen
    newIORef Game{..}

update :: IORef GameS -> Game a -> IO ()
update s' f = do
    s <- readIORef s'
    case execStateT f s of
        Just x -> writeIORef s' $! x
        _      -> putStrLn "no update?"

tick :: IORef GameS -> IO ()
tick s' = do
    s <- readIORef s'
    case execStateT stepGame s of
        Just x -> writeIORef s' $! x
        _      -> putStrLn "no update?"

forStage :: Stage -> (V -> Block -> IO ()) -> IO ()
forStage s f = void (M.traverseWithKey f s)

turnFlip :: V -> Game ()
turnFlip new = do
    now <- use (snake.body._head.velocity)
    if now == new
      then snake.body._head.velocity .= -new
      else snake.body._head.velocity .= new

linedUp :: V -> V -> Bool
linedUp x y = atIndex x 0 == atIndex y 0 && atIndex x 1 == atIndex y 1
           || atIndex x 1 == atIndex y 1 && atIndex x 2 == atIndex y 2
           || atIndex x 2 == atIndex y 2 && atIndex x 0 == atIndex y 0

isPaused :: IORef GameS -> IO Bool
isPaused = fmap _gameIsPaused . readIORef
