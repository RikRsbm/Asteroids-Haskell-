{-# LANGUAGE InstanceSigs #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss ( Point, Vector )
import Graphics.Gloss.Interface.IO.Game ( Key(Char) )
import Data.Set ( empty, Set ) 
import Graphics.Gloss.Data.Vector (magV, mulSV, rotateV, normalizeV)
import System.Random (mkStdGen, Random (randomR))



data GameState = GameState {
                   player :: Player
                 , stenen :: [Steen]
                 , bullets :: [Bullet]
                 , elapsedTime :: Float
                 , keysPressed :: Set Key
                 , started :: Bool
                 , paused :: Bool
                 , gameOver :: Bool
                 , score :: Int
                 }

initialState :: GameState
initialState = GameState (Player (0, 0) 
                                 (0, 0) 
                                 (0, lookDirectionVecMagnitude)
                         ) 
                         []
                         []
                         0 
                         empty
                         False
                         False
                         False
                         0




data Player = Player { 
                pLocation :: Point -- location of player
              , pVelocity :: Vector -- velocity of player
              , lookDirection :: Vector -- direction that player is looking in, it's a vector of magnitude inputAccelPlayer
              } 

data Steen = Steen { 
               sLocation :: Point -- location of stone
             , sVelocity :: Vector -- velocity of stone
             , sRadius :: Float -- radius of stone
             , level :: Int -- level of stone 
             } 

data Bullet = Bullet {
                bLocation :: Point -- location of bullet
              , bVelocity :: Vector -- velocity of bullet
              }


data Direction = Left | Right deriving Eq

class IsRound a where
    radius :: a -> Float

instance IsRound Steen where
    radius :: Steen -> Float
    radius = sRadius

instance IsRound Bullet where
    radius :: Bullet -> Float
    radius b = bulletRadius

class Movable a where -- things on screen that can move
    location :: a -> Point
    velocity :: a -> Vector
    glide :: a -> a -- how they should glide through space every step
    steer :: a -> Direction -> Float -> a -- steers them by certain number of degrees

instance Movable Player where
    location :: Player -> Point
    location = pLocation

    velocity :: Player -> Vector
    velocity = pVelocity

    glide :: Player -> Player
    glide p@(Player { pLocation = (x, y), pVelocity = vec@(dx, dy) }) -- updates player position and velocity
        = p { pLocation = (x + dx, y + dy), pVelocity = newVec }
      where 
        newVec | magV vec < autoDecelPlayer = (0, 0) -- if player (almost) stands  still
               | otherwise                  = vec `subVec` mulSV autoDecelPlayer (normalizeV vec) -- decelleration

    steer :: Player -> Direction -> Float -> Player
    steer p d angle = p { lookDirection = rotateV angle' (lookDirection p) } -- steer 'angle' degrees in direction 'd'
      where 
        angle' | d == Model.Left = angle
               | otherwise = -angle

instance Movable Steen where
    location :: Steen -> Point
    location = sLocation

    velocity :: Steen -> Vector
    velocity = sVelocity

    glide :: Steen -> Steen
    glide s@(Steen { sLocation = (x, y), sVelocity = (dx, dy) }) 
        = s { sLocation = (x + dx, y + dy) }

    steer :: Steen -> Direction -> Float -> Steen -- not used yet
    steer s d angle = s { sVelocity = rotateV angle' (velocity s) } -- steer 'angle' degrees in direction 'd'
      where 
        angle' | d == Model.Left = angle
               | otherwise = -angle

instance Movable Bullet where
    location :: Bullet -> Point
    location = bLocation

    velocity :: Bullet -> Vector
    velocity = bVelocity

    glide :: Bullet -> Bullet
    glide b@(Bullet { bLocation = (x, y), bVelocity = (dx, dy) }) 
        = b { bLocation = (x + dx, y + dy) }

    steer :: Bullet -> Direction -> Float -> Bullet -- not used yet
    steer b d angle = b { bVelocity = rotateV angle' (bVelocity b) } -- steer 'angle' degrees in direction 'd'
      where 
        angle' | d == Model.Left = angle
               | otherwise = -angle


boost :: Player -> Player
boost p@(Player { pVelocity = vec }) 
    = p { pVelocity = vec `addVec` mulSV inputAccelPlayer (lookDirection p) }

addVec :: Vector -> Vector -> Vector
addVec (dx, dy) (dx', dy') = (dx + dx',
                              dy + dy')

subVec :: Vector -> Vector -> Vector
subVec (dx, dy) (dx', dy') = (dx - dx',
                              dy - dy')
                        
addVecToPt :: Point -> Vector -> Point
addVecToPt (x, y) (x', y') = (x + x', 
                              y + y')

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
    | steenOdds == 0 && started gstate = Just (Steen (x, y) (dx, dy) r 1)
    | otherwise                        = Nothing
  where
    gen = mkStdGen seed
    (steenOdds  , gen1) = randomR (0  :: Int            , 160                ) gen 
    (radius     , gen2) = randomR (15 :: Int            , 40                 ) gen1
    (randomX    , gen3) = randomR (- halfWidth  - radius, halfWidth  + radius) gen2
    (randomY    , gen4) = randomR (- halfHeight - radius, halfHeight + radius) gen3
    (bigVelocity, gen5) = randomR (10  :: Int           , 20                 ) gen4
    (side       , gen6) = randomR (0  :: Int            , 4                  ) gen5

    (x, y) = case side of -- pick a side
               0 -> (fromIntegral randomX, - halfHeightFloat - r)
               1 -> (fromIntegral randomX,   halfHeightFloat + r)
               2 -> (- halfWidthFloat - r, fromIntegral randomY)
               _ -> (halfWidthFloat   + r, fromIntegral randomY)

    (dx, dy) = v `mulSV` normalizeV (a - x, b - y)
    (a, b) = location (player gstate)
    v = fromIntegral bigVelocity / 10
    r = fromIntegral radius



addMaybe :: Maybe a -> [a] -> [a]
addMaybe Nothing xs = xs
addMaybe (Just x) xs = x : xs



checkMovementKeyPressed :: Key -> Player -> Player
checkMovementKeyPressed (Char 'w') p = boost p
checkMovementKeyPressed (Char 'a') p = steer p Model.Left inputSteerPlayer 
checkMovementKeyPressed (Char 'd') p = steer p Model.Right inputSteerPlayer 
checkMovementKeyPressed _ gstate = gstate






lookDirectionVecMagnitude :: Float -- magnitude of lookDirection vector. leave this at 1. 
lookDirectionVecMagnitude = 1      -- This way we don't have to normalize the vector everytime we use it.
                                   -- If we change this, make sure that the vector gets normalized every time we use it

playerRadius :: Float -- distance between center and end of rocketship
playerRadius = 15

bulletRadius :: Float
bulletRadius = 4

bulletSpeed :: Float
bulletSpeed = 6

steenScoreMultiplier :: Int
steenScoreMultiplier = 3

-- for editing steen values, edit them in the "randomSteen" function (too much variables to put here)

autoDecelPlayer :: Float -- amount of automatic decelleration per step
autoDecelPlayer = 0.04

inputAccelPlayer :: Float -- amount of acceleration per 'w' press
inputAccelPlayer = 0.15

inputSteerPlayer :: Float -- angle that player steers per 'a' or 'd' press
inputSteerPlayer = pi / 40

screenWidth :: Int
screenWidth = 1000

halfWidth :: Int
halfWidth = screenWidth `div` 2

halfWidthFloat :: Float
halfWidthFloat = fromIntegral halfWidth

screenHeight :: Int
screenHeight = 600

halfHeight :: Int
halfHeight = screenHeight `div` 2

halfHeightFloat :: Float
halfHeightFloat = fromIntegral halfHeight



{-

to do:

1. file met highscores
2. uiteindelijk melding als je highscore verbreekt
3. highscore in beeld zetten

-}