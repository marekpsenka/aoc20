module Main where

import Data.List
import Data.Maybe
import Text.Read
import Data.Char

groupEntries :: [String] -> [[String]]
groupEntries = f [] []
  where
    f _ gs [] = gs
    f g gs [e] = if not (null e) then gs ++ [g ++ words e] else gs
    f g gs (e:es) = 
      if not (null e) 
        then f (g ++ words e) gs es 
        else f [] (gs ++ [g]) es

splitEntry :: String -> (String, String)
splitEntry s = (t, tail av)
  where (t, av) = break (':' == ) s

readPassport :: [String] -> [(String, String)]
readPassport = map splitEntry

hasCidEntry :: [String] -> Bool
hasCidEntry es = isJust $ find (isPrefixOf "cid:") es

isValid :: [String] -> Bool
isValid es
  | length es == 8 = True
  | length es == 7 = not (hasCidEntry es)
  | otherwise = False

hasAllEntries :: [(String, String)] -> Bool
hasAllEntries es = all (`elem` entryTypes) requiredEntries
  where
    entryTypes = map fst es
    requiredEntries = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

readAndCheckRange :: (Int, Int) -> String -> Bool
readAndCheckRange (min, max) s = 
  ((\n -> min <= n && n <= max) <$> (readMaybe s :: Maybe Int)) == Just True

readAndCheckHeight :: String -> Bool
readAndCheckHeight v
  | "cm" `isSuffixOf` v = readAndCheckRange (150, 193) (take (length v - 2) v)
  | "in" `isSuffixOf` v = readAndCheckRange (59, 76) (take (length v - 2) v)
  | otherwise = False

isColorCode :: String -> Bool
isColorCode s = (head s == '#') && (length s == 7) && all isHexDigit (tail s)

isValidEntry :: (String, String) -> Bool
isValidEntry (t, v) = case t of
  "byr" -> readAndCheckRange (1920, 2002) v
  "iyr" -> readAndCheckRange (2010, 2020) v
  "eyr" -> readAndCheckRange (2020, 2030) v
  "hgt" -> readAndCheckHeight v
  "hcl" -> isColorCode v
  "ecl" -> v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  "pid" -> (length v == 9) && all isDigit v
  "cid" -> True
  _ -> False


isValid2 :: [(String, String)] -> Bool
isValid2 pp = hasAllEntries pp && all isValidEntry pp

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  let grouped = groupEntries inputLines
  let split = map (map splitEntry) grouped
  print (sum $ map (fromEnum . isValid) grouped)
  print (sum $ map (fromEnum . isValid2) split)
