{-# LANGUAGE TemplateHaskell, BangPatterns, FlexibleInstances #-}
module Game where
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Sequence.Lens
import Data.Sequence (ViewL(..), ViewR(..), (|>), (<|), Seq)

import qualified Data.Foldable as F
import Data.Foldable (for_)

import Control.Monad.State
import Control.Arrow
import Data.Maybe

import Control.Lens hiding ((<|), (|>))
import Prelude hiding (head, last)

import Physics
import Menu

(!.) :: s -> State s a -> s
(!.) = flip execState

infixr 0 !.

type Pos = V
type Vel = V

type Positioned = M.Map Pos
type OrderedPos = []

x, y, z :: V
x = vec3 1 0 0
y = vec3 0 1 0
z = vec3 0 0 1

data Entity = Ent
    { _position     :: !Pos
    , _velocity     :: !Vel
    } deriving (Show, Eq)

type Body = S.Seq Entity

data Flame = Flame
    { _lifeLeft     :: !Int
    , _fent         :: !Entity
    } deriving (Show, Eq)

data Snake = Snake
    { _body         :: !(Seq Entity)
    , _flame        :: !(Seq Flame)
    } deriving (Show, Eq)

data Fruit
    = Grape
    | Apple
    | Orange
  deriving (Eq, Show, Ord, Enum)

data Block
    = Brick
    | Wood
    | Water
    | OK !Fruit
    | Off !Fruit
  deriving (Eq, Show, Ord)

type Stage = Positioned Block

data GameS
    = Game  { _stage         :: !Stage
            , _snake         :: !Snake
            , _score         :: !Int
            , _gameTicks     :: !Integer
            , _message       :: !(Maybe String) 
            , _gameIsPaused  :: !Bool
            }

makeLenses ''Flame
makeLenses ''Entity
makeLenses ''GameS
makeLenses ''Snake

type Game = StateT GameS Maybe

endGame :: Game ()
endGame = lift Nothing

pushEnt :: Entity -> Entity
pushEnt (Ent p v) = Ent (p+v) v

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
         & body  %~ pushBody
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

step :: Game ()
step = do
    False         <- view gameIsPaused
    Just newSnake <- uses snake stepSnakeAlone
    collisions    <- uses stage (collided newSnake)

    for_ collisions $ \ (pos, block) -> case block of
        -- if it's OK fruit, add a block to the end of the snake
        -- delete the block in the stage too
        OK  _ -> do
            stage.at pos .= Nothing
            Just oldLast <- preuse (snake.body._last)
            snake        .= newSnake
            score        += 1
            snake.body   %= (|> oldLast)

        -- remove the last segment of the snake if the fruit was off
        -- delete the block in the stage too
        Off _ -> do
            stage.at pos .= Nothing
            snake        .= newSnake
            snake.body   %= \b -> case S.viewr b of
                before :> _ -> before
                _           -> b

        Water -> snake .= newSnake

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
