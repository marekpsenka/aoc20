module Main where

import Data.List

groupEntries :: [String] -> [[String]]
groupEntries = f [] []
  where
    f _ gs [] = gs
    f g gs [e] = if not (null e) then gs ++ [g ++ words e] else gs
    f g gs (e:es) =
      if not (null e)
        then f (g ++ words e) gs es
        else f [] (gs ++ [g]) es

allUnion :: (Eq a) => [[a]] -> [a]
allUnion = foldl union []

allIntersection :: (Eq a) => [[a]] -> [a]
allIntersection l = foldl intersect (allUnion l) l

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  let grouped = groupEntries inputLines
  print (sum ( map (length . allUnion) grouped))
  print (sum ( map (length . allIntersection) grouped))
