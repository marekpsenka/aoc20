module Main where
  
import qualified Data.Set as Set

makeSetOfSums :: [Int] -> Set.Set Int
makeSetOfSums [] = Set.empty 
makeSetOfSums [i] = Set.empty
makeSetOfSums (i:is) = sums i is `Set.union` makeSetOfSums is
  where sums i is = Set.fromList (map (i +) is)

findDiscrepancy :: [Int] -> Maybe Int
findDiscrepancy xs
  | length xs < 26 = Nothing
  | candidate `Set.member` makeSetOfSums (take 25 xs) = findDiscrepancy (tail xs)
  | otherwise = Just candidate
    where
      candidate = xs !! 25

containsSequence :: [Int] -> [Int] -> Int -> Maybe [Int]
containsSequence [] _ _ = Nothing
containsSequence (x:xs) ys tgt
  | x + sum ys < tgt = containsSequence xs (ys ++ [x]) tgt
  | x + sum ys == tgt = Just ys
  | otherwise = Nothing

findSequence :: [Int] -> Int -> Maybe Int
findSequence [] _ = Nothing 
findSequence (x:xs) tgt = case containsSequence (x:xs) [] tgt of
  Nothing -> findSequence xs tgt
  Just seq -> Just (maximum seq + minimum seq)

main :: IO ()
main = do
  inpt <- lines <$> getContents
  let numbers = map (read :: String -> Int) inpt
      Just discrepancy = findDiscrepancy numbers
  print discrepancy
  print (findSequence numbers discrepancy)
