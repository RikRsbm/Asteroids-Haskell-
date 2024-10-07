module General where

import Graphics.Gloss (Vector, Point)







addVec :: Vector -> Vector -> Vector
addVec (dx, dy) (dx', dy') = (dx + dx',
                              dy + dy')

subVec :: Vector -> Vector -> Vector
subVec (dx, dy) (dx', dy') = (dx - dx',
                              dy - dy')
                        
addVecToPt :: Point -> Vector -> Point
addVecToPt (x, y) (x', y') = (x + x', 
                              y + y')



addMaybe :: Maybe a -> [a] -> [a]
addMaybe Nothing xs = xs
addMaybe (Just x) xs = x : xs