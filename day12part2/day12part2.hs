module Main where

import Linear.Vector
import Linear.V2

readInstruction :: String -> (Char, Int)
readInstruction (c:cs) = (c, read cs :: Int)

rotateWaypoint :: V2 Int -> (Char, Int) -> V2 Int
rotateWaypoint (V2 x y) (_, 180) = V2 (-x) (-y)
rotateWaypoint (V2 x y) ('L', 90) = V2 (-y) x
rotateWaypoint wp ('L', 270) = rotateWaypoint wp ('R', 90)
rotateWaypoint (V2 x y) ('R', 90) = V2 y (-x)
rotateWaypoint wp ('R', 270) = rotateWaypoint wp ('L', 90)
rotateWaypoint _ _ = error "unexpected"

moveWaypoint :: V2 Int -> (Char, Int) -> V2 Int
moveWaypoint (V2 x y) (ac, amt) = case ac of
  'E' -> V2 (x + amt) y
  'N' -> V2 x (y + amt)
  'W' -> V2 (x - amt) y
  'S' -> V2 x (y - amt)
  _ -> error "unexpected"

sail :: [(Char, Int)] -> V2 Int
sail = sail_ (V2 10 1)
  where
    sail_ :: V2 Int -> [(Char, Int)] -> V2 Int
    sail_ _ [] = V2 0 0
    sail_ wp ((ac, amt):as)
      | ac == 'F' = amt *^ wp ^+^ sail_ wp as
      | ac == 'L' || ac == 'R' = sail_ (rotateWaypoint wp (ac, amt)) as
      | otherwise = sail_ (moveWaypoint wp (ac, amt)) as

manhattan :: V2 Int -> Int
manhattan (V2 x y) = abs x + abs y

main :: IO ()
main = do
  inpt <- map readInstruction . lines <$> getContents
  let destination = sail inpt

  print (manhattan destination)

