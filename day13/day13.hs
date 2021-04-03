module Main where

import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read
import Control.Arrow

nearestGEMultiple :: Integer -> Integer -> Integer
nearestGEMultiple i j = if i `mod` j == 0 then i else ((i `div` j) + 1) * j

extEuc :: Integer -> Integer -> (Integer, Integer, Integer)
extEuc a 0 = (a, 1, 0)
extEuc a b = let (d, x', y') = extEuc b (a `mod` b)
  in
    (d, y', x' - (a `div` b) * y')

smoothenInput :: (Integer, Integer) -> (Integer, Integer)
smoothenInput (pos, line) = (pos `mod` line, line)

merge :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
merge (a1, n1) (a2, n2) = (x `mod` (n1 * n2) , n1 * n2)
  where
    (_, m1, _) = extEuc n1 n2
    x = a1 + (a2 - a1) * m1 * n1

main :: IO ()
main = do
  l1:l2:_ <- lines <$> getContents
  let 
    dep = read l1 :: Integer
    sndcmp = \(_, a) (_, b) -> compare a b
    buses = mapMaybe (readMaybe :: String -> Maybe Integer) $ splitOn "," l2
    ps = zip buses (map (\j -> nearestGEMultiple dep j - dep) buses)
    (bus, dt) = minimumBy sndcmp ps

    allBuses = map (readMaybe :: String -> Maybe Integer) $ splitOn "," l2
    pps2 = zip [(0 :: Integer)..] allBuses 
    ps2 = map (id *** fromJust) $ filter (\(_, mb) -> isJust mb) pps2
    sps2 = map smoothenInput ps2
    so = foldl merge (0, 1) sps2

  print (bus * dt)
  print (snd so - fst so)
