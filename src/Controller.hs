-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model ( Movable(glide), GameState(..), initialState )
import Functionality
    ( pCheckBounds,
      pColliding,
      bColliding,
      shootBullet,
      checkWithinBounds,
      randomSteen,
      checkMovementKeyPressed )
import Constants ( steenScoreMultiplier, highscorePath )
import General ( addMaybe )
import Graphics.Gloss.Interface.IO.Game
    ( Key(SpecialKey, Char),
      KeyState(Up, Down),
      SpecialKey(KeyEsc, KeyEnter),
      Event(EventKey) )
import System.Random ( randomIO )
import Data.Set ( delete, insert ) 
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad (when)






step :: Float -> GameState -> IO GameState
step secs gstate 
    | firstStep gstate = readHighscore gstate
    | gameOver gstate || paused gstate = return gstate 
    | any (pColliding (player gstate)) (stenen gstate) = finishGame gstate            
    | otherwise = update gstate secs

readHighscore :: GameState -> IO GameState
readHighscore gstate 
    = do text <- readFile highscorePath
         let oldHs = readMaybe (takeWhile (/= '\n') text)
         return $ gstate { firstStep = False, highscore = fromMaybe 0 oldHs }

finishGame :: GameState -> IO GameState
finishGame gstate 
    = do when (score gstate > highscore gstate)
           $ writeFile highscorePath (show (score gstate))
         return $ gstate { gameOver = True}

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
inputKey (EventKey (SpecialKey KeyEnter) Down _ _) gstate@(GameState { started = True, paused = False, gameOver = False }) 
    = shootBullet gstate
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(GameState { started = True, gameOver = False}) 
    = gstate { paused = not (paused gstate) }
inputKey (EventKey k Down _ _) gstate = gstate { keysPressed = insert k (keysPressed gstate)} -- for other keys
inputKey (EventKey k Up _ _)   gstate = gstate { keysPressed = delete k (keysPressed gstate)}
inputKey _ gstate = gstate -- other key events (and events in general)

