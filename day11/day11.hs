module Main where

import Data.Array
import Data.List
import Data.Maybe
import Control.Arrow

type WaitingArea = Array (Int, Int) Char

getAdjacent :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getAdjacent (x, y) (mx, my) = 
  delete (x, y) [(x + i, y + j) | i <- [-1, 0, 1], 
                                  0 <= x + i, x + i <= mx, 
                                  j <- [-1, 0, 1], 
                                  0 <= y + j, y + j <= my]


isWithinBounds :: (Int, Int) -> (Int, Int) -> Bool
isWithinBounds (mx, my) (x, y) = 0 <= x && x <= mx && 0 <= y && y <= my


getSeatsInDirection :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
getSeatsInDirection bs (x, y) (dx, dy) = 
  takeWhile (isWithinBounds bs) [(x + i * dx, y + i * dy) | i <- [1..]]


findSeatInDirection :: WaitingArea -> (Int, Int) -> (Int, Int) -> Maybe Char
findSeatInDirection st pos d = if null seats then Nothing else find ('.' /=) (map (st !) seats)
  where 
    ((_, _), bs) = bounds st
    seats = getSeatsInDirection bs pos d


getAdjacent2 :: WaitingArea -> (Int, Int) -> [Char]
getAdjacent2 st pos = mapMaybe (findSeatInDirection st pos) directions
  where 
    directions = delete (0, 0) [(i, j) | i <- [-1, 0, 1], j <- [-1, 0, 1]]
  

transition2 :: WaitingArea -> (Int, Int) -> Maybe Char
transition2 st (x, y)
  | st ! (x, y) == 'L' = if all ('L' ==) adjacents then Just '#' else Nothing
  | st ! (x, y) == '#' = if length (filter ('#'==) adjacents) >= 5 then Just 'L' else Nothing
  | otherwise = Nothing
    where
      adjacents = getAdjacent2 st (x, y)


transition :: WaitingArea -> (Int, Int) -> Maybe Char
transition st (x, y)
  | st ! (x, y) == 'L' = if all (\c -> (c == 'L'|| c == '.')) adjacents then Just '#' else Nothing
  | st ! (x, y) == '#' = if length (filter ('#'==) adjacents) >= 4 then Just 'L' else Nothing
  | otherwise = Nothing
    where ((_, _), (mx, my)) = bounds st
          adjacents = [st ! coord | coord <- getAdjacent (x, y) (mx, my)]


nextState :: (WaitingArea -> (Int, Int) -> Maybe Char) -> WaitingArea -> WaitingArea
nextState tFn st = st // transitions
  where 
    ixs = indices st
    maybeTransitions = zip ixs (map (tFn st) ixs)
    transitions = map (id *** fromJust) $ filter (\(_, mt) -> isJust mt) maybeTransitions


findFixedPoint :: WaitingArea -> (WaitingArea -> WaitingArea) -> WaitingArea
findFixedPoint initSt stFn = fixedPoint 
  where
    states = iterate stFn initSt
    (_, fixedPoint) = head (dropWhile (uncurry (/=)) (zip states (tail states)))

main :: IO ()
main = do
  inpt <- lines <$> getContents
  let my = length (head inpt) - 1
      mx = length inpt - 1
      initState = listArray ((0, 0), (mx, my)) (concat inpt)
      fixedPoint1 = findFixedPoint initState (nextState transition)
      fixedPoint2 = findFixedPoint initState (nextState transition2)

  print (length (filter ('#'==) (elems fixedPoint1)))
  print (length (filter ('#'==) (elems fixedPoint2)))

