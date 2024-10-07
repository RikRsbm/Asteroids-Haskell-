module Main where

import Controller ( input, step )
import Constants ( screenWidth, screenHeight )
import Model ( initialState )
import View ( view )





import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Stenen" (screenWidth, screenHeight) (0, 0)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function