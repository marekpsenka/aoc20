module Main where

import Data.List.Split

data DbEntry = DbEntry {
  occMin :: Int,
  occMax :: Int,
  char :: Char,
  pwd :: String
} deriving (Show)

readDbEntry :: String -> DbEntry
readDbEntry s = 
  let
     rule:pw:_ = splitOn ": " s
     limits:char:_ = splitOn " " rule
     occMin:occMax:_ = splitOn "-" limits
  in
    DbEntry {occMin = read occMin :: Int, 
             occMax = read occMax :: Int, 
             char =  char !! 0, pwd=pw}

inRange :: (Ord a) => (a, a) -> a -> Bool
inRange (k, l) x = k <= x && x <= l

countOcc :: (Eq a) => [a] -> a -> Int
countOcc xs x = length $ filter (==x) xs

isValid :: DbEntry -> Bool
isValid e = inRange (occMin e, occMax e) (countOcc (pwd e) (char e))

isValid2 :: DbEntry -> Bool
isValid2 e = (((pwd e) !! ((occMin e) - 1)) == (char e)) /=
             (((pwd e) !! ((occMax e) - 1)) == (char e)) 

main :: IO ()
main = do
  inpt <- lines <$> getContents
  let entries = map readDbEntry inpt
  print (length $ filter id (map isValid entries))
  print (length $ filter id (map isValid2 entries))
