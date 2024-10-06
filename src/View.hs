-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Graphics.Gloss.Data.Vector (rotateV, normalizeV)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures (steenPics ++ bulletPics ++ playerPics ++ scorePics ++ x ++ y ++ z)
  where
    playerPics = viewPlayer (player gstate)
    steenPics = map viewSteen (stenen gstate)
    bulletPics = map viewBullet (bullets gstate)
    scorePics = viewScore (score gstate)
    x | gameOver gstate = viewGameOver
      | otherwise       = []
    y | started gstate  = []
      | otherwise       = viewNotStarted    
    z | paused gstate   = viewPaused
      | otherwise       = []


viewPlayer :: Player -> [Picture]
viewPlayer p = [lineLeft, lineRight, lineBack]
  where 
    (x, y) = location p
    d@(dx, dy) = lookDirection p
    (dx', dy') = rotateV (pi / 2) d

    lineBack  = color green (line [(x - playerRadius * dx - playerRadius * dx' / 2, y - playerRadius * dy - playerRadius * dy' / 2),
                                   (x - playerRadius * dx + playerRadius * dx' / 2, y - playerRadius * dy + playerRadius * dy' / 2)])
    lineLeft  = color green (line [(x - playerRadius * dx + playerRadius * dx' / 2, y - playerRadius * dy + playerRadius * dy' / 2),
                                   (x + playerRadius * dx                         , y + playerRadius * dy                         )])
    lineRight = color green (line [(x - playerRadius * dx - playerRadius * dx' / 2, y - playerRadius * dy - playerRadius * dy' / 2),
                                   (x + playerRadius * dx                         , y + playerRadius * dy                         )])

viewSteen :: Steen -> Picture
viewSteen s = translate x y (color white (circle (radius s))) 
  where (x, y) = location s

viewBullet :: Bullet -> Picture
viewBullet b = translate x y (color red (circle bulletRadius))
  where (x, y) = location b

viewScore :: Int -> [Picture]
viewScore s = [translate 250 250 (scale 0.25 0.25 (color blue (text ("Score: " ++ show s))))]

viewNotStarted :: [Picture]
viewNotStarted = [translate (-450) 200 (scale 0.4 0.4 (color blue (text "W to boost")))
                , translate (-450) 140 (scale 0.4 0.4 (color blue (text "A, D to steer")))
                , translate (-450) 80 (scale 0.4 0.4 (color blue (text "Enter to shoot")))
                , translate (-450) 20 (scale 0.4 0.4 (color blue (text "Esc to pause")))
                , translate (-150) (-150) (scale 0.4 0.4 (color blue (text "W to start")))]

viewPaused :: [Picture]
viewPaused = [translate (-300) (-150) (scale 0.4 0.4 (color blue (text "paused, Esc to resume")))]

viewGameOver :: [Picture]
viewGameOver = [translate (-160) (-150) (scale 0.4 0.4 (color blue (text "R to restart")))]
