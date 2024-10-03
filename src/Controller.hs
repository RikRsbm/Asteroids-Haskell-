-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import GHC.IO.Encoding (BufferCodec(getState))
import Data.Set ( delete, insert ) 

-- | Handle one iteration of the game
-- step :: Float -> GameState -> IO GameState
-- step secs gstate
--   | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
--   = -- We show a new random number
--     do randomNumber <- randomIO
--        let newNumber = abs randomNumber `mod` 10
--        return $ GameState (ShowANumber newNumber) 0
--   | otherwise
--   = -- Just update the elapsed time
--     return $ gstate { elapsedTime = elapsedTime gstate + secs }

step :: Float -> GameState -> IO GameState -- hier ook toevoegen dat ie checkt of er een key is gepressed
step secs gstate = do
    let gstate' = foldr checkKeyPressed gstate (keysPressed gstate) -- does every action that belongs to the currently pressed keys
    return $ gstate' { player = glide (player gstate'), elapsedTime = elapsedTime gstate' + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate) 

inputKey :: Event -> GameState -> GameState
inputKey (EventKey k Down _ _) gstate = gstate { keysPressed = insert k (keysPressed gstate)}
inputKey (EventKey k Up _ _)   gstate = gstate { keysPressed = delete k (keysPressed gstate)}
inputKey _ gstate = gstate -- other key events (and events in general)

checkKeyPressed :: Key -> GameState -> GameState
checkKeyPressed (Char 'w') gstate = gstate { player = boost (player gstate) }
checkKeyPressed (Char 'a') gstate = gstate { player = steer (player gstate) Model.Left inputSteerPlayer }
checkKeyPressed (Char 'd') gstate = gstate { player = steer (player gstate) Model.Right inputSteerPlayer }
checkKeyPressed _ gstate = gstate

