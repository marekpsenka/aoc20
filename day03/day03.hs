module Main where

markTreeByInt :: Char -> Int
markTreeByInt c = case c of 
  '.' -> 0
  '#' -> 1
  _ -> error "wtf"

seeForest :: Int -> [String] -> (Int, Int) -> Char
seeForest p forest (x, y)
  | y >= length forest = error "y index out of range"
  | otherwise = (forest !! y) !! (x `mod` p)

getCoords :: Int -> (Int, Int) -> [(Int, Int)]
getCoords ymax (dx, dy) = [(dx * m, dy * m) | m <- [0..(ymax `div` dy)]]

countTreesOnSlope :: Int -> [String] -> (Int, Int) -> Int
countTreesOnSlope p forest d = 
  sum (map (markTreeByInt . seeForest p forest) (getCoords ((length forest) - 1) d))

main :: IO ()
main = do
  forest <- lines <$> getContents
  let mapPeriod = length (head forest)
      slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

  print (countTreesOnSlope mapPeriod forest (3, 1))
  print (product (map (countTreesOnSlope mapPeriod forest) slopes))
