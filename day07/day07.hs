module Main where

import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map

data BagType = BagType {
  adjective :: String,
  color :: String
} deriving (Show, Ord, Eq)

data Contained = Contained {
  count :: Int,
  ty :: BagType
} deriving (Show, Ord, Eq)

type ContentMap = Map.Map BagType [Contained]

parseContained :: String -> [Contained]
parseContained s = if s == "no other bags" then [] else map readContained (splitOn ", " s)
  where
    readContained strc = Contained {
        count = read scnt :: Int, 
        ty = BagType {adjective = sadj, color = scol}
      }
      where 
        scnt:sadj:scol:_ = words strc

parseContent :: String -> (BagType, [Contained])
parseContent il = (BagType {adjective = adj, color = col}, parseContained content)
  where
    bag:content:_ = splitOn " contain " (init il)
    adj:col:_ = words bag

isShinyGold :: Contained -> Bool
isShinyGold Contained {count=_, ty=bt} = 
  (adjective bt == "shiny") && (color bt == "gold")

canContainShinyGold :: ContentMap -> BagType -> Bool
canContainShinyGold m bt = case Map.lookup bt m of
  Nothing -> False
  Just cs -> any isShinyGold cs || any (canContainShinyGold m . ty) cs

countBags :: ContentMap -> BagType -> Int
countBags m bt = case Map.lookup bt m of
  Nothing -> 0
  Just [] -> 0
  Just cs -> sum (map (\c -> count c * (1 + countBags m (ty c))) cs)

main :: IO ()
main = do
  inputLines <- lines <$> getContents 
  let 
    contentMap = Map.fromList (map parseContent inputLines)

  print (Map.size (Map.filterWithKey (\bt _ -> canContainShinyGold contentMap bt) contentMap))
  print (countBags contentMap BagType {adjective="shiny", color="gold"})
