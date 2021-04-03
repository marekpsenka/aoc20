module Main where

import Data.List
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Maybe

readIntegerWithSign :: String -> Int
readIntegerWithSign (c:cs) = case c of
  '+' -> read cs :: Int
  '-' -> read (c:cs) :: Int
  _ -> error "Cannot parse integer"

readInstruction :: String -> (String, Int)
readInstruction s = (take 3 s, readIntegerWithSign (drop 4 s) :: Int)

executeTracked :: [(String, Int)] -> Int -> Int -> Set.Set Int -> Int
executeTracked ins acc ip tr = 
  if ip `Set.member` tr 
    then acc 
  else case ins !! ip of
    ("acc", val) -> executeTracked ins (acc + val) (ip + 1) (Set.insert ip tr)
    ("jmp", val) -> executeTracked ins acc (ip + val) (Set.insert ip tr)
    ("nop", val) -> executeTracked ins acc (ip + 1) (Set.insert ip tr)

executeTracked2 :: [(String, Int)] -> Int -> Int -> Set.Set Int -> Maybe Int
executeTracked2 ins acc ip tr
  | ip `Set.member` tr = Nothing 
  | ip == length ins = Just acc
  | otherwise = case ins !! ip of
    ("acc", val) -> executeTracked2 ins (acc + val) (ip + 1) (Set.insert ip tr)
    ("jmp", val) -> executeTracked2 ins acc (ip + val) (Set.insert ip tr)
    ("nop", val) -> executeTracked2 ins acc (ip + 1) (Set.insert ip tr)

checkHalt :: Vector.Vector (String, Int) -> Int -> Maybe Int
checkHalt ins i = case ins Vector.! i of 
  ("jmp", val) -> executeTracked2 (Vector.toList (ins Vector.// [(i, ("nop", val))])) 0 0 Set.empty
  ("nop", val) -> executeTracked2 (Vector.toList (ins Vector.// [(i, ("jmp", val))])) 0 0 Set.empty
  ("acc", _) -> Nothing
  _ -> error "unexpected"

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  let ins = map readInstruction inputLines
  print (executeTracked ins 0 0 Set.empty)
  let v = Vector.fromList ins
      halts = map (checkHalt v) [0..(length ins - 1)]
      maybeAcc = find isJust halts
  print maybeAcc

