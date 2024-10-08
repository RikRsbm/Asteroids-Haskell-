-- | This module defines how to turn
--   the game state into a picture
module View where

import Model
    ( Movable(location),
      IsRound(radius),
      Bullet,
      Steen,
      Player(lookDirection),
      GameState(player, stenen, bullets, score, highscore, status), Status (PreStart, Paused, GameOver) )
import General ( addMaybe )
import Constants
    ( playerRadius,
      bulletRadius,
      bigTextScale,
      smallTextScale,
      statusY,
      explanationX,
      playerBackLineRatio )
import Graphics.Gloss.Data.Vector (rotateV)
import Graphics.Gloss
    ( blue,
      green,
      red,
      white,
      circle,
      color,
      line,
      pictures,
      scale,
      text,
      translate,
      Color,
      Picture (Blank),
      Point )





view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures pics
  where
    pics = steenPics ++ bulletPics ++ [playerPic, scorePic, highscorePic, statusPic]

    playerPic = viewPlayer (player gstate)
    steenPics = map viewSteen (stenen gstate)
    bulletPics = map viewBullet (bullets gstate)
    scorePic = viewScore (score gstate)
    highscorePic = viewHighscore (highscore gstate)
    statusPic = viewStatus (status gstate)


viewPlayer :: Player -> Picture
viewPlayer p = pictures [lineLeft, lineRight, lineBack]
  where 
    (x, y) = location p
    d@(dx, dy) = lookDirection p
    (dx', dy') = rotateV (pi / 2) d

    lineBack  = color green (line [(x - pDx - pDx', y - pDy - pDy'),
                                   (x - pDx + pDx', y - pDy + pDy')])
    lineLeft  = color green (line [(x - pDx + pDx', y - pDy + pDy'),
                                   (x + pDx       , y + pDy       )])
    lineRight = color green (line [(x - pDx - pDx', y - pDy - pDy'),
                                   (x + pDx       , y + pDy       )])
    pDx = playerRadius * dx
    pDy = playerRadius * dy
    pDx' = playerRadius * dx' * playerBackLineRatio
    pDy' = playerRadius * dy' * playerBackLineRatio

viewSteen :: Steen -> Picture
viewSteen s = translate x y (color white (circle (radius s))) 
  where (x, y) = location s

viewBullet :: Bullet -> Picture
viewBullet b = translate x y (color red (circle bulletRadius))
  where (x, y) = location b

viewScore :: Int -> Picture
viewScore s = viewText (250, 250) smallTextScale blue ("Score: " ++ show s)

viewHighscore :: Int -> Picture
viewHighscore s = translate 190 210 (scale smallTextScale smallTextScale (color blue (text ("Highscore: " ++ show s))))

viewStatus :: Status -> Picture
viewStatus PreStart = pictures [viewText (explanationX, 200) bigTextScale blue "W to boost"
                              , viewText (explanationX, 140) bigTextScale blue "A, D to steer"
                              , viewText (explanationX, 80) bigTextScale blue "Enter to shoot"
                              , viewText (explanationX, 20) bigTextScale blue "Esc to pause"
                              , viewText (-150, statusY) bigTextScale blue "W to start"]
viewStatus Paused = viewText (-300, statusY) bigTextScale blue "paused, Esc to resume"
viewStatus GameOver = viewText (-160, statusY) bigTextScale blue "R to restart"
viewStatus _ = Blank

viewText :: Point -> Float -> Color -> String -> Picture
viewText (x, y) s c txt = translate x y (scale s s (color c (text txt)))

