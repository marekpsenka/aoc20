module Main where

import Control.Arrow
import Data.List

splitPass :: String -> (String, String)
splitPass = splitAt 7

charCodedBinary :: Char -> Char -> String -> Int
charCodedBinary _ _ [] = 0
charCodedBinary zc oc (c:cs) = a + charCodedBinary zc oc cs
  where
    a = if c == zc then 0 else 2^length cs 

toRowNumber :: String -> Int
toRowNumber = charCodedBinary 'F' 'B'

toColumnNumber :: String -> Int
toColumnNumber = charCodedBinary 'L' 'R'

getId :: (Int, Int) -> Int
getId (row, column) = row * 8 + column

findGap :: [Int] -> Int
findGap [] = error "cannot find gap in empty list"
findGap [_] = error "cannot find gap in list containing one element"
findGap (x:xs) = if head xs == x + 1 then findGap xs else x + 1

main :: IO ()
main = do
  passes <- lines <$> getContents
  let splitPasses = map splitPass passes
      passToId = getId . (toRowNumber *** toColumnNumber)
      ids = map passToId splitPasses

  print (maximum ids)
  print (findGap (sort ids))
