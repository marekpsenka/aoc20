module Main where

import Data.List

getPairs :: [Int] -> [(Int, Int)]
getPairs [] = []
getPairs [_] = []
getPairs (x:xs) = [(x, y) | y <- xs] ++ getPairs xs

getTriples :: [Int] -> [(Int, Int, Int)]
getTriples xs = [(x, y, z) | x <- xs, y <- xs, z <- xs]

do2AddTo2020 :: (Int, Int) -> Bool
do2AddTo2020 (x, y) = x + y == 2020

do3AddTo2020 :: (Int, Int, Int) -> Bool
do3AddTo2020 (x, y, z) = x + y + z == 2020

findAnswer2 :: [Int] -> Int
findAnswer2 x = case find do2AddTo2020 (getPairs x) of
  Nothing -> error "oops"
  Just (y, z) -> y * z

findAnswer3 :: [Int] -> Int
findAnswer3 w = case find do3AddTo2020 (getTriples w) of
  Nothing -> error "oops"
  Just (x, y, z) -> x * y * z


main :: IO ()
main = do
  inpt <- getContents
  let numbers = map (read::String->Int) (lines inpt)
  print (findAnswer2 numbers)
  print (findAnswer3 numbers)
