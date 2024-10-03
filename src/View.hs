-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Graphics.Gloss.Data.Vector (rotateV, normalizeV)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = Pictures pics
  where -- this is all just for the player
    (x, y) = pLocation (player gstate)
    d@(dx, dy) = lookDirection (player gstate)
    pics = [lineLeft, lineRight, lineBack]
    -- lineLong = color green (line [(x - playerSize * dx, y - playerSize * dy),
    --                               (x + playerSize * dx, y + playerSize * dy)])
    lineBack  = color green (line [(x - playerSize * dx - playerSize * dx' / 2, y - playerSize * dy - playerSize * dy' / 2),
                                   (x - playerSize * dx + playerSize * dx' / 2, y - playerSize * dy + playerSize * dy' / 2)])
    lineLeft  = color green (line [(x - playerSize * dx + playerSize * dx' / 2, y - playerSize * dy + playerSize * dy' / 2),
                                   (x + playerSize * dx                       , y + playerSize * dy                       )])
    lineRight = color green (line [(x - playerSize * dx - playerSize * dx' / 2, y - playerSize * dy - playerSize * dy' / 2),
                                   (x + playerSize * dx                       , y + playerSize * dy                       )])
    (dx', dy') = rotateV (pi / 2) d

-- viewPure :: GameState -> Picture
-- viewPure gstate = case infoToShow gstate of
--   ShowNothing   -> blank
--   ShowANumber n -> color green (text (show n))
--   ShowAChar   c -> color green (text [c])

-- viewPure :: GameState -> Picture
-- viewPure gstate = color green (circle (sizeOfBall gstate))