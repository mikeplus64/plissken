import Data.Array.Repa as R
import Data.Word
import Data.Fixed
import Types

tau :: Double
tau = 2*pi

health :: Snake -> Int
health (Snake _ b d) = size (extent b) + 1

isAlive :: Snake -> Bool
isAlive (Snake _ b d) = size (extent b) > 0

isDead :: Snake -> Bool
isDead = not . isAlive

toX, toY, toZ :: Direction
toX = Direction (pi/2) 0
toY = Direction 0 0
toZ = Direction 0 (pi/2)

fromX, fromY, fromZ :: Direction
fromX = Direction (3*pi/2) 0
fromY = Direction pi 0
fromZ = Direction 0 pi

-- | "Normalize" a direction; ensure its angles are positive, and less than a full revolution
normalize :: Direction -> Direction
normalize (Direction p a) = Direction (mod' p tau) (mod' a tau)

push :: Vec3 -> Direction -> Double -> Vec3
push (Vec3 x y z) dir mag = Vec3 (x+ox) (y+oy) (z+oz)
  where
    -- new positions relative to the origin
    oxz = sqrt (mag^2 - oy^2)
    ox  = oxz * cos (polar dir)
    oy  = mag * sin (azimuth dir)
    oz  = oxz * sin (polar dir)

slide :: Vec3 -> Direction -> Double -> Double -> Double -> Vec3
slide v@(Vec3 x y z) dir mag time now
    = Vec3 (x+nt*lx) (y+nt*ly) (z+nt*lz)
  where
    nt            = now/time
    Vec3 lx ly lz = push v dir mag

slideBody :: Body -> Double -> Double -> Double -> Body
slideBody (Body v d) mag time now = Body (slide v d mag time now) d

