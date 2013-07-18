{-# LANGUAGE TemplateHaskell, BangPatterns, FlexibleInstances, RecordWildCards, RankNTypes, OverloadedStrings, NamedFieldPuns #-}
module Game where
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Sequence.Lens
import Data.Sequence (ViewL(..), ViewR(..), (|>), (<|), Seq)

import qualified Data.Foldable as F
import qualified Data.Traversable as F
import Data.Foldable (for_)
import Data.Maybe
import Data.Ord
import Data.IORef
import Data.Time
import Data.List
import Data.Char (toUpper)

import qualified Data.ByteString.Char8 as B

import Control.Monad.State.Strict
import Control.Applicative
import Control.Arrow

import Control.Lens hiding ((<|), (|>))
import Prelude hiding (head, last)

import System.Random
import Geometry hiding ((<|), (|>), step)

import Foreign.C

import Controls
import Util(sign)

type Game = StateT GameS Maybe

toX, toY, toZ :: V
toX = vec3 1 0 0
toY = vec3 0 1 0
toZ = vec3 0 0 1

{-# INLINE wrapAdd #-}
wrapAdd :: V -> V -> V
wrapAdd x y = wrap (x+y)

{-# INLINE wrap #-}
wrap :: V -> V
wrap = cmap $ \a -> fromIntegral (floor a `mod` 10 :: Int)

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
    , _message       :: !(Maybe B.ByteString)
    , _gameIsPaused  :: !Bool
    , _gameStuck     :: !Bool
    , _gameEditMode  :: !Bool
    , _gameStarted   :: !Bool
    , _scores        :: ![Score]
    , _difficulty    :: !Difficulty
    , _playerName    :: !B.ByteString
    , _stdGen        :: !StdGen
    } deriving (Show)

data Score = Score
    { _initials      :: !B.ByteString
    , _scored        :: !Integer
    , _gameTicksUsed :: !Integer  
    , _easiness      :: !Difficulty
    , _overallRating :: !Double  
    } deriving (Show,Eq,Ord,Read)

data Difficulty = Difficulty
    { _dieAtWall      :: !Bool
    , _selfCollisions :: !Bool
    , _speed          :: !NominalDiffTime
    } deriving (Show,Eq,Ord,Read)

instance Read NominalDiffTime where
    readsPrec _ [] = []
    readsPrec _ s  =
        let double   = takeWhile (/= 's') s
            leftOver = drop (length double+1) s
        in [(realToFrac (read double::Double), leftOver)]

makeLenses ''Difficulty
makeLenses ''Score
makeLenses ''Flame
makeLenses ''Entity
makeLenses ''GameS
makeLenses ''Snake

dieAtWallPenalty :: Bool -> Double
dieAtWallPenalty True  = 0.1
dieAtWallPenalty False = 1.0

selfCollisionPenalty :: Bool -> Double
selfCollisionPenalty True  = 1.5
selfCollisionPenalty False = 1.0

rateEase :: Difficulty -> Double
rateEase (Difficulty d s p) = (dieAtWallPenalty d * selfCollisionPenalty s)/realToFrac p

easy :: Difficulty
easy = Difficulty False True 0.30

medium :: Difficulty
medium = Difficulty False False 0.15

hard :: Difficulty
hard = Difficulty True False 0.10

addToSpeed :: NominalDiffTime -> NominalDiffTime
addToSpeed x = fromIntegral (floor (100*x+5) `mod` 100 :: Int) / 100


tick :: IORef GameS -> IO ()
tick s' = do
    s <- readIORef s'
    case execStateT stepGame s of
        Just x -> do
            writeIORef s' $! x
            unless (_snakeIsAlive x)
                (update_ s' restart)

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
    addFruit goodFruits

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
    _ -> error "intToBlock: invalid int"
intFromBlock :: Block -> Int
intFromBlock b = case b of
    Brick       -> 0
    Wood        -> 1
    Water       -> 2
    Good Grape  -> 3
    Good Apple  -> 4
    Good Orange -> 5
    Bad Grape   -> 6
    Bad Apple   -> 7
    Bad Orange  -> 8
blockAfter, blockBefore :: Block -> Block
blockAfter  = intToBlock . (`mod` 9) . succ . intFromBlock
blockBefore = intToBlock . (`mod` 9) . pred . intFromBlock

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
 
pushEnt :: Entity -> Entity
pushEnt (Ent p v) = Ent (wrapAdd p v) v

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
turn v = do
    gameStuck .= False
    snake.body._head.velocity .= v

end :: B.ByteString -> Game ()
end s = do
    inTop10 <- amInTopTen
    when inTop10 addScore
    message      .= Just s
    snakeIsAlive .= False
    gameIsPaused .= True

restart :: Game ()
restart = do
    !inTop10 <- amInTopTen
    ()       <- when inTop10 addScore

    !now <- get
    !gen <- use stdGen
    put (initialGame gen M.empty)
    generateLevel 15

    playerName   .= now^.playerName
    difficulty   .= now^.difficulty
    scores       .= now^.scores
    gameIsPaused .= True
    gameStarted  .= False

{-# INLINE stepGame #-}
stepGame :: Game ()
stepGame = do
    False <- use gameIsPaused
    False <- use gameEditMode
    False <- use gameStuck
    True  <- use gameStarted

    message .= Nothing
    
    oldSnake        <- use snake
    Just newSnake   <- uses snake stepSnakeAlone

    gameTicks += 1

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
               else do
                    score -= 60
                    snake .= oldSnake
                    gameStuck .= True

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
        _gameEditMode = False
        _scores = []
        _playerName = "MAL"
        _gameStuck = False
        _gameStarted = False
    in Game{..}

newGame :: Stage -> IO (IORef GameS)
newGame _stage = do
    _stdGen <- newStdGen
    newIORef (initialGame _stdGen _stage)

{-# INLINE update_ #-}
update_ :: IORef s -> StateT s Maybe a -> IO ()
update_ s' f = do
    s <- readIORef s'
    case execStateT f s of
        Just x -> writeIORef s' $! x
        _      -> return ()

{-# INLINE update #-}
update :: IORef s -> StateT s Maybe a -> IO (Maybe a)
update s' f = do
    s <- readIORef s'
    case runStateT f s of
        Just (a,x) -> do
            writeIORef s' $! x
            return (Just a)
        _      -> return Nothing

forStage :: Stage -> (V -> Block -> IO ()) -> IO ()
forStage s f = void (M.traverseWithKey f s)

turnFlip :: V -> Game ()
turnFlip new = do
    gameStuck .= False
    now <- use (snake.body._head.velocity)
    if now == new
      then snake.body._head.velocity .= -new
      else snake.body._head.velocity .= new

linedUp :: V -> V -> Bool
linedUp x y = atIndex x 0 == atIndex y 0 && atIndex x 1 == atIndex y 1
           || atIndex x 1 == atIndex y 1 && atIndex x 2 == atIndex y 2
           || atIndex x 2 == atIndex y 2 && atIndex x 0 == atIndex y 0
           
menuIsOpen :: IORef GameS -> IO Bool
menuIsOpen game' = menuIsOpen' <$> readIORef game'

{-# INLINE menuIsOpen' #-}
menuIsOpen' :: GameS -> Bool
menuIsOpen' Game{_gameIsPaused,_gameEditMode} = not _gameEditMode && _gameIsPaused

{-# INLINE gameField #-}
gameField :: IORef GameS -> Lens' GameS a -> IO a
gameField ref getter = view getter `fmap` readIORef ref

scoreOf :: Game Score
scoreOf = do
    !_initials      <- B.map toUpper . B.take 3 <$> use playerName
    !_easiness      <- use difficulty
    !_gameTicksUsed <- use gameTicks
    !_scored        <- use score
    let !_overallRating = rateEase _easiness * (fromIntegral _scored * fromIntegral _gameTicksUsed ** 0.25)
    return Score{..}

addScore :: Game ()
addScore = do
    s <- scoreOf
    scores %= (s:)
    scores %= topTen

formatScores :: [Score] -> B.ByteString
formatScores = B.unlines . map formatScore
  where
    formatScore = B.pack . show

readScores :: B.ByteString -> [Score]
readScores = map readScore . B.lines
  where
    readScore = read . B.unpack
  
saveScores :: FilePath -> GameS -> IO ()
saveScores p g = B.writeFile p (formatScores (_scores g))

loadScores :: FilePath -> IO [Score]
loadScores p = readScores `fmap` B.readFile p

topTen :: [Score] -> [Score]
topTen = take 10 . sortBy (flip (comparing _overallRating))

amInTopTen :: Game Bool
amInTopTen = do
    !myScore <- scoreOf
    !scores' <- use scores
    return $! length scores' < 10
           || any (\s0 -> _overallRating myScore >= _overallRating s0) scores'

scoreAt :: Int -> Game (Maybe Score)
scoreAt i = do
    ss <- use scores
    return (ss^?ix i)

arcadeFormatScore :: Score -> B.ByteString
arcadeFormatScore s = B.concat [ _initials s, " | ", B.pack . show $ _overallRating s ]

resumeOrStart :: Game ()
resumeOrStart = do
    gameIsPaused .= False
    gameStarted  .= True
    gameEditMode .= False
    message      .= Nothing

openMenu :: Game ()
openMenu = do
    gameIsPaused .= True
    gameEditMode .= False
    message      .= Just "paused"

{-# INLINE runGameEvent #-}
runGameEvent :: Event -> Game ()
runGameEvent event = case event of
    AbsX 0     -> turnFlip (vec3 1 0 0)
    AbsY 0     -> turnFlip (vec3 0 1 0)
    AbsZ 0     -> turnFlip (vec3 0 0 1)
    AbsX i     -> turnFlip (vec3 (sign i) 0 0)
    AbsY i     -> turnFlip (vec3 0 (sign i) 0)
    AbsZ i     -> turnFlip (vec3 0 0 (sign i))
    Flamethrow -> flamethrow
    EndGame    -> restart
    _          -> return ()
