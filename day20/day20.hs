import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Bifunctor
import Control.Monad

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
        e2 = map ((\(x, y) -> (tn1, y, x)) . bimap applyTwoRotations applyTwoRotations) os

applyRotation :: Orientation -> Orientation
applyRotation (Ori r f) = 
  if f then Ori ((4 + (r - 1)) `mod` 4) f else Ori ((r + 1) `mod` 4) f

applyTwoRotations :: Orientation -> Orientation
applyTwoRotations (Ori r f) = 
  if f then Ori ((4 + (r - 2)) `mod` 4) f else Ori ((r + 2) `mod` 4) f

link :: (Int, Orientation) -> ConnectivityMap -> [(Int, Orientation)]
link (t, o) m =
  case find (\(_, o1, _) -> o1 == o) conn
  of
    Nothing -> [(t, o)]
    Just (nt, _, no) -> (t, o):link (nt, no) m
  where
    conn = fromJust $ Map.lookup t m

glueTiles :: [[TileData]] -> [String]
glueTiles = concatMap glueRow
  where
    glueRow :: [TileData] -> [String]
    glueRow [] = []
    glueRow ds = foldl (zipWith (++)) (replicate (length (head ds)) []) ds

printTile :: [String] -> IO ()
printTile t = forM_ t putStrLn

assembleTiles :: [[(Int, Orientation)]] -> TileMap -> [[TileData]]
assembleTiles ts m = map (map (\(n,  o) -> orient (dropBorder $ fromJust (Map.lookup n m)) o)) ts
  where
    dropBorder :: [String] -> [String]
    dropBorder ss = (init . tail) (map (init . tail) ss)

isSeaMonster :: [String] -> (Int, Int) -> Bool
isSeaMonster img (x, y) = 
  (img !! y) !! (x + 18) == '#' && 
  (img !! (y + 1)) !! x == '#' && 
  (img !! (y + 1)) !! (x + 5) == '#' && 
  (img !! (y + 1)) !! (x + 6) == '#' && 
  (img !! (y + 1)) !! (x + 11) == '#' && 
  (img !! (y + 1)) !! (x + 12) == '#' && 
  (img !! (y + 1)) !! (x + 17) == '#' && 
  (img !! (y + 1)) !! (x + 18) == '#' && 
  (img !! (y + 1)) !! (x + 19) == '#' && 
  (img !! (y + 2)) !! (x + 1) == '#' && 
  (img !! (y + 2)) !! (x + 4) == '#' && 
  (img !! (y + 2)) !! (x + 7) == '#' && 
  (img !! (y + 2)) !! (x + 10) == '#' && 
  (img !! (y + 2)) !! (x + 13) == '#' && 
  (img !! (y + 2)) !! (x + 16) == '#'

countMonsters :: [String] -> Int
countMonsters img = 
  length (filter (isSeaMonster img) [(x, y) | x <- [0..(width - 20)], y <- [0..(height - 3)]])
  where
    width = length (head img)
    height = length img

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
        (t, minOri)

    startsOfRows = map (second applyRotation) (link startTile connected)
    grid = map (`link` connected) startsOfRows
    assembled = assembleTiles grid tileMap
    image = glueTiles assembled
    numberOfMonsters = fromJust $ find (>0) [countMonsters (orient image (Ori r f)) | r <- [0..3], f <- [False, True]]
    roughness = sum (map (length . filter (=='#')) image)

  print (product (map fst cornerTileConnections))
  print (roughness - 15 * numberOfMonsters)
