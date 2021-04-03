module Main where

import Data.List

connect :: [Int] -> (Int, Int, Int)
connect rs = connect_ rs (1, 0, 0)
  where 
    connect_ [r] (d1, d2, d3) = (d1, d2, d3 + 1)
    connect_ (r:rs) (d1, d2, d3) = case head rs - r of
      1 -> connect_ rs (d1 + 1, d2, d3)
      2 -> connect_ rs (d1, d2 + 1, d3)
      3 -> connect_ rs (d1, d2, d3 + 1)
      _ -> error "unexpected"
 
calculateArr :: [Int] -> Int
calculateArr [] = 1
calculateArr (1:1:1:1:ds) = 7 * calculateArr ds
calculateArr (1:1:1:ds) = 4 * calculateArr ds
calculateArr (1:1:ds) = 2 * calculateArr ds
calculateArr (_:ds) = calculateArr ds

main :: IO ()
main = do
  inpt <- lines <$> getContents
  let ratings = sort $ map (read :: String -> Int) inpt
      (d1, d2, d3) = connect ratings
      diffs = (1:) $ zipWith (-) (tail ratings) ratings
  print (d1 * d3) 

  print (calculateArr diffs)

