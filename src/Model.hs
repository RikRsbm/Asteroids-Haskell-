{-# LANGUAGE InstanceSigs #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Constants
    ( lookDirectionVecMagnitude, bulletRadius, autoDecelPlayer )
import Graphics.Gloss ( Point, Vector )
import Graphics.Gloss.Interface.IO.Game ( Key(Char) )
import Data.Set ( empty, Set ) 
import Graphics.Gloss.Data.Vector (magV, mulSV, rotateV, normalizeV)
import General ( subVec )






data GameState = GameState {
                   player :: Player
                 , stenen :: [Steen]
                 , bullets :: [Bullet]
                 , elapsedTime :: Float
                 , keysPressed :: Set Key
                 , firstStep :: Bool
                 , started :: Bool
                 , paused :: Bool
                 , gameOver :: Bool
                 , score :: Int
                 , highscore :: Int
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
                         True
                         False
                         False
                         False
                         0
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






