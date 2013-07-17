{-# LANGUAGE TemplateHaskell, BangPatterns, FlexibleInstances, RecordWildCards, RankNTypes #-}
module Game where
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Sequence.Lens
import Data.Sequence (ViewL(..), ViewR(..), (|>), (<|), Seq)

import qualified Data.Foldable as F
import qualified Data.Traversable as F
import Data.Foldable (for_)

import Control.Monad.State.Strict
import Control.Arrow
import Data.Maybe

import Control.Lens hiding ((<|), (|>))
import Prelude hiding (head, last)
import Data.IORef
import Data.Time

import System.Random
import System.IO.Unsafe (unsafePerformIO)
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

data GameS = Game  
    { _stage         :: !Stage
    , _stageBounds   :: !Int
    , _snakeIsAlive  :: !Bool
    , _snake         :: !Snake
    , _score         :: !Integer
    , _gameTicks     :: !Integer
    , _message       :: !(Maybe String) 
    , _gameIsPaused  :: !Bool
    , _difficulty    :: !Difficulty
    , _stdGen        :: !StdGen
    } deriving (Show)

data Difficulty = Difficulty
    { _dieAtWall      :: !Bool
    , _selfCollisions :: !Bool
    , _speed          :: !NominalDiffTime
    } deriving (Show)

makeLenses ''Difficulty

easy :: Difficulty
easy = Difficulty False True 0.30

medium :: Difficulty
medium = Difficulty False False 0.15

hard :: Difficulty
hard = Difficulty True False 0.10

addToSpeed :: NominalDiffTime -> NominalDiffTime
addToSpeed x = fromIntegral (floor (100*x+5) `mod` 100 :: Int) / 100

makeLenses ''Flame
makeLenses ''Entity
makeLenses ''GameS
makeLenses ''Snake


tick :: IORef GameS -> IO ()
tick s' = do
    s <- readIORef s'
    case execStateT stepGame s of
        Just x
            | _snakeIsAlive x -> writeIORef s' $! x
            | otherwise       ->
                update s' $ do
                    restart
                    difficulty   .= x^.difficulty
                    message      .= x^.message
                    gameIsPaused .= True
        _ -> return ()

rand :: (Int,Int) -> Game Int
rand bounds = do
  gen' <- use stdGen
  case randomR bounds gen' of
    (i,gen) -> do
      stdGen .= gen
      return i

randf :: (Int,Int) -> Game F
randf bounds = fromIntegral `fmap` rand bounds

addB, subB :: Int -> Int -> Int
addB x y = mod (x + y) 10
subB x y = mod (x - y) 10

gen4x4 :: Int -> Int -> Int -> Block -> Stage
gen4x4 x' y' z' b = M.fromList
    [ (vec3 (fromIntegral x) (fromIntegral y) (fromIntegral z), b)
    | x <- [x',addB x' 1]
    , y <- [y',addB y' 1]
    , z <- [z',addB z' 1]
    ]

generateLevel :: Int -> Game ()
generateLevel blockCount = do
    () <- replicateM_ (blockCount `div` 2) $! do
        x' <- rand (0,9)
        y' <- rand (0,9)
        z' <- rand (0,9)
        b  <- intToBlock `fmap` rand (0,1)
        stage %= M.union (gen4x4 x' y' z' b) 
    stage %= M.filterWithKey (\k _ -> not (linedUp (vec3 4 4 4) k))
    return ()

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
    x <- randf (0,9)
    y <- randf (0,9)
    z <- randf (0,9)
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

selfColliding :: Snake -> Bool
selfColliding (Snake bdy _)
  = F.any (\(Ent pos _) -> headPos == Just (fr pos)) (bdy^._tail)
  where
    headPos = fmap fr (bdy^?_head.position)
    fr :: V -> V
    fr = cmap (fromIntegral . (floor :: Float -> Int))

turn :: Vel -> Game ()
turn v = snake.body._head.velocity .= v
 
    
end :: String -> Game ()
end s = do
    message      .= Just s
    snakeIsAlive .= False
    gameIsPaused .= True

restart :: Game GameS
restart = do
    now <- get
    gen <- use stdGen
    stg <- use stage
    put (initialGame gen stg)
    stage .= M.empty
    generateLevel 15
    return now

{-# INLINE stepGame #-}
stepGame :: Game ()
stepGame = do
    False         <- use gameIsPaused
    message .= Nothing

    
    Just newSnake <- uses snake stepSnakeAlone
    selfCollideTest <- use (difficulty.selfCollisions)
    when (selfCollideTest && selfColliding newSnake) $
        end "Self collision!"
    collisions    <- uses stage (collided newSnake)
    snake        .= newSnake

    for_ collisions $ \ (pos, block) -> case block of
        -- if it's OK fruit, add a block to the end of the snake
        -- delete the block in the stage too
        Good _ -> do
            stage.at pos .= Nothing
            -- this is fairly safe, as we know that to get here at all
            -- the snake should not be null.
            Just oldLast <- preuse (snake.body._last)
            score        += 100
            snake.body   %= (|> oldLast)
            addFruit goodFruits

        -- remove the last segment of the snake if the fruit was off
        -- delete the block in the stage too
        Bad _ -> do
            stage.at pos .= Nothing
            snake.body   %= \b -> case S.viewr b of
                before :> _ -> before
                _           -> b
            bdy <- use (snake.body)
            addFruit anyFruits

        Water -> return ()

        -- if it's anything else, colliding with it is deadly.
        _     -> do
            diesFromWalls <- use (difficulty.dieAtWall)
            if diesFromWalls
               then end "Deadly collision!"
               else lift Nothing

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

    snake' <- use snake

    when (S.null (_body snake')) $ end "Ran out of snake!"
    when (selfImmolated snake')  $ end "Cooked to death!"

    gameTicks += 1

flamethrow :: Game ()
flamethrow = do
    Just ent <- preuse (snake.body._head)
    snake.flame %= (|> Flame 5 (pushEnt ent))

newStage :: [(Pos,Block)] -> Stage
newStage = M.fromList

initialGame :: StdGen -> Stage -> GameS
initialGame _stdGen _stage =
    let _snake = Snake (S.singleton (Ent (vec3 4 4 4) (vec3 1 0 0))) S.empty
        _score = 0
        _gameTicks = 0
        _message = Nothing
        _gameIsPaused = False
        _stageBounds = 10
        _snakeIsAlive = True
        _difficulty  = medium
    in Game{..}

newGame :: Stage -> IO (IORef GameS)
newGame _stage = do
    _stdGen <- newStdGen
    newIORef (initialGame _stdGen _stage)

update :: IORef s -> StateT s Maybe a -> IO ()
update s' f = do
    s <- readIORef s'
    case execStateT f s of
        Just x -> writeIORef s' $! x
        _      -> return ()

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

{-# INLINE gameField #-}
gameField :: IORef GameS -> Lens' GameS a -> IO a
gameField ref getter = view getter `fmap` readIORef ref
