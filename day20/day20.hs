import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Bifunctor

type TileData = [String]
type TileMap = Map.Map Int TileData
data Orientation = Ori Int Bool deriving(Show, Eq)
type ConnectivityMap = Map.Map Int [(Int, Orientation, Orientation)]

orient :: TileData -> Orientation -> TileData
orient td (Ori rots flipped)
  | flipped = map reverse rotated
  | otherwise = rotated
  where
    rotated = iterate (transpose . reverse) td !! rots

rightEdge :: TileData -> String
rightEdge = map last

leftEdge :: TileData -> String
leftEdge = map head

natural :: CharParser st Int
natural = (read :: String -> Int) <$> many1 digit

tileNumber :: CharParser st Int
tileNumber = string "Tile " *> natural <* (char ':' >> newline)

match :: TileData -> TileData -> [(Orientation, Orientation)]
match t1 t2 = filter leftRightAgree orientations
  where
    orientations = [(Ori r1 f1, Ori r2 f2) | r1 <- [0..3], f1 <- [True, False], r2 <- [0..3], f2 <- [True, False]]
    leftRightAgree (o1, o2) = rightEdge (orient t1 o1) == leftEdge (orient t2 o2)


tile :: CharParser st (Int, TileData)
tile = do tn <- tileNumber
          rows <- sepEndBy (many1 (oneOf ['#', '.'])) newline
          return (tn, rows)

uniquePairs :: [a] -> [(a, a)]
uniquePairs x = [(y, z) | (y:ys) <- tails x, z <- ys]

connect :: TileMap -> ConnectivityMap
connect m = foldl f Map.empty (uniquePairs (Map.keys m))
  where
    f :: ConnectivityMap -> (Int, Int) -> ConnectivityMap
    f n (tn1, tn2) = Map.insertWith (++) tn2 e2 (Map.insertWith (++) tn1 e1 n)
      where
        td1 = fromJust $ Map.lookup tn1 m
        td2 = fromJust $ Map.lookup tn2 m
        os = match td1 td2
        e1 = map (\(x, y) -> (tn2, x, y)) os
        e2 = map ((\(x, y) -> (tn1, y, x)) . bimap (prod (Ori 2 False)) (prod (Ori 2 False))) os

complement :: Orientation -> Orientation -> Orientation
complement (Ori r1 f1) (Ori r2 f2) = Ori ((4 - r1 + r2) `mod` 4) (f1 /= f2)

prod :: Orientation -> Orientation -> Orientation
prod (Ori r1 f1) (Ori r2 f2) = Ori ((r1 + r2) `mod` 4) (f1 /= f2)

link :: (Int, Orientation) -> ConnectivityMap -> [(Int, Orientation)]
link (t, o) m =
  case find (\(_, o1, _) -> o1 == o) conn
  of
    Nothing -> [(t, o)]
    Just (nt, _, no) -> (t, o):link (nt, no) m
  where
    conn = fromJust $ Map.lookup t m

main :: IO ()
main = do
  inpt <- getContents
  let
    tiles = fromRight [] (parse (sepBy1 tile newline <* eof) "" inpt)
    tileMap = foldl (\m (num, dat) -> Map.insert num dat m) Map.empty tiles
    connected = connect tileMap
    cornerTileConnections = filter ((4==) . length . snd) (Map.assocs connected)
    startTile =
      let
        (t, cs) = head cornerTileConnections
        [(_, Ori r1 f1, _), (_, Ori r2 f2, _)] = filter (\(_, Ori r f, _) -> not f) cs
        minOri = if r1 == (r2 + 1) `mod` 4 then Ori r2 False else Ori r1 False
      in
        (t, complement minOri (Ori 0 False))

    startsOfRows = map (second (prod (Ori 1 False))) (link startTile connected)
    grid = map (`link` connected) startsOfRows


  print (product (map fst cornerTileConnections))
  print grid

