import Data.List
import Data.List.Split
import qualified Data.Map as Map

type Spoken = Map.Map Int Int

sayNumber :: [Int] -> Int
sayNumber (x:xs) = case elemIndex x xs of
  Just i -> i + 1
  Nothing -> 0

turn :: [Int] -> [Int]
turn x = (sayNumber x):x

getNth :: Int -> [Int] -> Int
getNth n start = head $ iterate turn start !! (n - length start)

turn2 :: (Int, Int, Spoken) -> (Int, Int, Spoken)
turn2 (l, i, sp) = case Map.lookup l sp of 
  Just il -> (i - il, i + 1, Map.insert l i sp)
  Nothing -> (0, i + 1, Map.insert l i sp)

initSpoken :: [Int] -> Spoken
initSpoken xs = Map.fromList (zip xs [1..])

getNth2 :: Int -> [Int] -> Int
getNth2 n start = nth
  where
    (nth, _, _) = iterate turn2 (last start, length start, initSpoken (init start)) !! (n - length start)

main :: IO ()
main = do
  inpt <- splitOn "," <$> getLine
  let startingNumbers = map (read :: String -> Int) inpt
  print (getNth 2020 (reverse startingNumbers))
  print (getNth2 30000000 startingNumbers)
