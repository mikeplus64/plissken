{-# LANGUAGE BangPatterns, TypeOperators, TypeFamilies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Types where
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector
import Data.Word
import Data.IORef
import Data.Fixed

type Radius = Double

data RGBA  = RGBA !Word8 !Word8 !Word8 !Word8
  deriving (Eq, Ord, Show, Read)

data Vec3 = Vec3 !Double !Double !Double
  deriving (Eq, Ord, Show, Read)

data Danger
    = Deflect
    | Lava
  deriving (Eq, Ord, Show, Read)

data Block 
    = Empty
    | Wall
    | Lamp !RGBA
    | Mist !RGBA !Double
    | Danger !Danger
    | Spawn
  deriving (Eq, Ord, Show, Read)

-- | See http://en.wikipedia.org/wiki/Spherical_coordinate_system
-- Same as that without a radial distance.
-- Angles are in radians.
-- X ~ left/right
-- Y ~ out/in
-- Z ~ down/up
data Direction = Direction
    { polar   :: !Double 
    , azimuth :: !Double
    } deriving (Eq, Ord, Show, Read)

data MagDir = MD !Direction !Double
  deriving (Eq, Ord, Show, Read)

data Body = Body 
    { position      :: !Vec3
    , facing        :: !Direction
    } deriving (Eq, Ord, Show, Read)

data Snake = Snake
    { direction     :: !Direction
    , alive         :: !(Array V (Z:.Int) Body)
    , dead          :: !(Array V (Z:.Int) Body)
    }

data Level = Level
    { name   :: String
    , blocks :: !(Array V (Z:.Int:.Int) Block)
    }

data World = World
    { snake   :: !(IORef Snake)
    , level   :: !(IORef Level)
    }

-- update :: Double -> World -> IO ()
