module Main where


data Course = E | N | W | S deriving (Enum, Show, Read)


readInstruction :: String -> (Char, Int)
readInstruction (c:cs) = (c, read cs :: Int)


tsum :: (Int, Int) -> (Int, Int) -> (Int, Int)
tsum (x, y) (z, w) = (x + z, y + w)


changeCourse :: Course -> (Char, Int) -> Course
changeCourse c (lr, deg) = case lr of 
  'L' -> toEnum $ (fromEnum c + (deg `div` 90)) `mod` 4
  'R' -> toEnum $ ((fromEnum c - (deg `div` 90)) + 4) `mod` 4


move :: Course -> Int -> (Int, Int)
move c amt = case c of
  E -> (amt, 0)
  N -> (0, amt)
  W -> (-amt, 0)
  S -> (0, -amt)


sail :: [(Char, Int)] -> (Int, Int)
sail = sail_ E
  where
    sail_ _ [] = (0, 0)
    sail_ c ((cr, amt):is)
      | cr == 'L' || cr == 'R' = sail_ (changeCourse c (cr, amt)) is
      | cr == 'F' = (move c amt) `tsum` sail_ c is
      | otherwise = (move (read [cr] :: Course) amt) `tsum` sail_ c is


manhattan :: (Int, Int) -> Int
manhattan (x, y) = abs x + abs y

main :: IO ()
main = do
  inpt <- map readInstruction . lines <$> getContents
  let pos = sail inpt
      dist = manhattan pos

  print dist

