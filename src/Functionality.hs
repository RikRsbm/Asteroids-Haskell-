module Functionality where

import Model
    ( Movable(steer, location),
      IsRound(..),
      Direction(Right, Left),
      Bullet(Bullet),
      Steen(Steen),
      Player(..),
      GameState( bullets, score, player) )
import General ( addVec, addVecToPt )
import Constants
    ( playerRadius,
      bulletSpeed,
      inputAccelPlayer,
      inputSteerPlayer,
      halfWidth,
      halfWidthFloat,
      halfHeight,
      halfHeightFloat )
import Graphics.Gloss.Data.Vector (mulSV, normalizeV, magV)
import System.Random (Random(randomR), mkStdGen)





boost :: Player -> Player
boost p@(Player { pVelocity = vec }) 
    = p { pVelocity = vec `addVec` mulSV inputAccelPlayer (lookDirection p) }

pCheckBounds :: Player -> Player -- checks whether player goes out of bounds. if so, reset player within bounds
pCheckBounds p@(Player { pLocation = (x, y), pVelocity = (dx, dy) }) 
    = p { pLocation = (x', y'), pVelocity = (dx', dy') }
  where
    (x', dx') | x >   width  = (  width , 0 )
              | x < - width  = (- width , 0 ) 
              | otherwise    = (  x     , dx)
    (y', dy') | y >   height = (  height, 0 )
              | y < - height = (- height, 0 )
              | otherwise    = (  y     , dy)
    width  = halfWidthFloat - playerRadius
    height = halfHeightFloat - playerRadius

pColliding :: Player -> Steen -> Bool
pColliding p s  
    | magV (x - a, y - b) < radius s + playerRadius / 2 = True -- /2 so that you actually have to touch the stone if you are sideways
    | otherwise                                         = False
  where 
    (x, y) = location p
    (a, b) = location s

bColliding :: Steen -> Bullet -> Bool 
bColliding b s  
    | magV (x - p, y - q) < radius s + radius b = True
    | otherwise                                 = False
  where 
    (x, y) = location b
    (p, q) = location s

shootBullet :: GameState -> GameState
shootBullet gstate = gstate { bullets = bul : bullets gstate, score = score gstate - 1 }
  where 
    bul = Bullet loc vec
    loc = location p `addVecToPt` (playerRadius `mulSV` lookDirection p) -- make sure bullet starts at point of player
    vec = bulletSpeed `mulSV` lookDirection p 
    p = player gstate

checkWithinBounds :: (IsRound a, Movable a) => a -> Bool
checkWithinBounds m = x < width  && x > - width &&
                      y < height && y > - height
  where 
    r = radius m
    (x, y) = location m
    width  = halfWidthFloat  + r + 1 -- +1 so spawned stones don't immediately despawn
    height = halfHeightFloat + r + 1

randomSteen :: Int -> GameState -> Maybe Steen
randomSteen seed gstate 
    | steenOdds == 0 = Just (Steen (x, y) (dx, dy) r 1)
    | otherwise      = Nothing
  where
    gen = mkStdGen seed
    (steenOdds  , gen1) = randomR (0  :: Int            , 100                ) gen 
    (radius     , gen2) = randomR (15 :: Int            , 40                 ) gen1
    (randomX    , gen3) = randomR (- halfWidth  - radius, halfWidth  + radius) gen2
    (randomY    , gen4) = randomR (- halfHeight - radius, halfHeight + radius) gen3
    (bigVelocity, gen5) = randomR (10  :: Int           , 20                 ) gen4
    (side       , gen6) = randomR (0  :: Int            , 3                  ) gen5

    (x, y) = case side of -- pick a side
               0 -> (fromIntegral randomX, - halfHeightFloat - r)
               1 -> (fromIntegral randomX,   halfHeightFloat + r)
               2 -> (- halfWidthFloat - r, fromIntegral randomY)
               _ -> (halfWidthFloat   + r, fromIntegral randomY)

    (dx, dy) = v `mulSV` normalizeV (a - x, b - y)
    (a, b) = location (player gstate)
    v = fromIntegral bigVelocity / 10
    r = fromIntegral radius

checkMovementKeyPressed :: (Char, Bool) -> Player -> Player
checkMovementKeyPressed ('w', True) p = boost p
checkMovementKeyPressed ('a', True) p = steer p Model.Left inputSteerPlayer 
checkMovementKeyPressed ('d', True) p = steer p Model.Right inputSteerPlayer 
checkMovementKeyPressed _ gstate = gstate