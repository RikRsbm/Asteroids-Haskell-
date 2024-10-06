-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random ( randomIO )
import GHC.IO.Encoding (BufferCodec(getState))
import Data.Set ( delete, insert ) 






step :: Float -> GameState -> IO GameState
step secs gstate 
    | gameOver gstate || paused gstate = return gstate 
    | any (pColliding (player gstate)) (stenen gstate) = return $ gstate { gameOver = True }
    | otherwise = update gstate secs

update :: GameState -> Float -> IO GameState
update gstate secs 
     = do r <- randomIO
          let l = length (stenen gstate)
          let newStenen = checkBulletSteenCollisions (stenen gstate)
          return $ gstate 
                  { 
                    player = pCheckBounds (glide (foldr checkMovementKeyPressed (player gstate) (keysPressed gstate)))
                  , stenen = addMaybe (randomSteen r gstate) (map glide (filter checkWithinBounds newStenen))
                  , bullets = map glide (filter checkWithinBounds (bullets gstate))
                  , score = score gstate + steenScoreMultiplier * (l - length newStenen)
                  , elapsedTime = elapsedTime gstate + secs  
                  }
  where 
    checkBulletSteenCollisions = filter (\steen -> not (any (bColliding steen) (bullets gstate)))  
    

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate) 

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'r') Down _ _) GameState { gameOver = True} = initialState
inputKey k@(EventKey (Char 'w') Down _ _) gstate@(GameState { started = False }) -- if w is pressed for the first time, start the game and call inputkey again to move forward
    = inputKey k (gstate { started = True })
inputKey (EventKey (SpecialKey KeyEnter) Down _ _) gstate = shootBullet gstate
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate = gstate { paused = not (paused gstate) }
inputKey (EventKey k Down _ _) gstate = gstate { keysPressed = insert k (keysPressed gstate)} -- for other keys
inputKey (EventKey k Up _ _)   gstate = gstate { keysPressed = delete k (keysPressed gstate)}
inputKey _ gstate = gstate -- other key events (and events in general)

