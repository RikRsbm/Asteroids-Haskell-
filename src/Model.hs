{-# LANGUAGE InstanceSigs #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Set 
import Graphics.Gloss.Data.Vector (magV, mulSV, rotateV, normalizeV)



data GameState = GameState {
                   player :: Player
                 , elapsedTime :: Float
                 , keysPressed :: Set Key
                 }

initialState :: GameState
initialState = GameState (Player (0, 0) 
                                 (0, 0) 
                                 (0, lookDirectionVecMagnitude)
                         ) 
                         0 
                         empty




data Player = Player { 
                pLocation :: Point -- location of player
              , pVelocity :: Vector -- velocity of player
              , lookDirection :: Vector -- direction that player is looking in, it's a vector of magnitude inputAccelPlayer
              } 

data Steen = Steen { 
               location :: Point -- location of stone
             , velocity :: Vector -- velocity of stone
             , level :: Int -- level of stone 
             } 

data Direction = Left | Right deriving Eq




class Movable a where -- things on screen that can move
    getPosition :: a -> Point -- gets their position
    glide :: a -> a -- how they should glide through space every step
    steer :: a -> Direction -> Float -> a -- steers them by certain number of degrees

instance Movable Player where
    getPosition :: Player -> Point
    getPosition = pLocation

    glide :: Player -> Player
    glide p@(Player { pLocation = (x, y), pVelocity = vec@(dx, dy) }) -- updates player position and velocity
        = checkBounds (p { pLocation = (x + dx, y + dy), pVelocity = newVec } )
      where 
        newVec | magV vec < autoDecelPlayer = (0, 0) -- if player (almost) stands  still
               | otherwise                  = vec `subVec` mulSV autoDecelPlayer (normalizeV vec) -- decelleration

    steer :: Player -> Direction -> Float -> Player
    steer p d angle = p { lookDirection = rotateV angle' (lookDirection p) } -- steer 'angle' degrees in direction 'd'
      where 
        angle' | d == Model.Left = angle
               | otherwise = -angle

instance Movable Steen where
    getPosition :: Steen -> Point
    getPosition = location

    glide :: Steen -> Steen
    glide s@(Steen { location = (x, y), velocity = (dx, dy) }) 
        = s { location = (x + dx, y + dy) }

    steer :: Steen -> Direction -> Float -> Steen
    steer = undefined




boost :: Player -> Player
boost p@(Player { pVelocity = vec }) 
    = p { pVelocity = vec `addVec` mulSV inputAccelPlayer (lookDirection p) }

addVec :: Vector -> Vector -> Vector
addVec (dx, dy) (dx', dy') = (dx + dx',
                              dy + dy')

subVec :: Vector -> Vector -> Vector
subVec (dx, dy) (dx', dy') = (dx - dx',
                              dy - dy')

checkBounds :: Player -> Player -- checks whether player goes out of bounds. if so, reset player within bounds
checkBounds p@(Player { pLocation = (x, y), pVelocity = (dx, dy) }) 
    = p { pLocation = (x', y'), pVelocity = (dx', dy') }
  where
    (x', dx') | x >   width  = (  width , 0 )
              | x < - width  = (- width , 0 ) 
              | otherwise    = (  x     , dx)
    (y', dy') | y >   height = (  height, 0 )
              | y < - height = (- height, 0 )
              | otherwise    = (  y     , dy)
    width  = fromIntegral screenWidth  / 2 - playerSize
    height = fromIntegral screenHeight / 2 - playerSize




lookDirectionVecMagnitude :: Float -- magnitude of lookDirection vector. leave this at 1. 
lookDirectionVecMagnitude = 1      -- This way we don't have to normalize the vector everytime we use it.
                                   -- If we change this, make sure that the vector gets normalized every time we use it

playerSize :: Float -- distance between center and end of rocketship
playerSize = 15

autoDecelPlayer :: Float -- amount of automatic decelleration per step
autoDecelPlayer = 0.04

inputAccelPlayer :: Float -- amount of acceleration per 'w' press
inputAccelPlayer = 0.15

inputSteerPlayer :: Float -- angle that player steers per 'a' or 'd' press
inputSteerPlayer = pi / 40

screenWidth :: Int
screenWidth = 1000

screenHeight :: Int
screenHeight = 600