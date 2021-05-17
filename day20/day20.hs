import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import qualified Data.Array as A
import Data.Either

type TileData = A.Array (Int, Int) Char

data Tile = Tile{num :: Int, dat :: TileData} deriving(Show)

natural :: CharParser st Int
natural = (read :: String -> Int) <$> many1 digit

tileNumber :: CharParser st Int
tileNumber = string "Tile " *> natural <* (char ':' >> newline)

index :: [[a]] -> [[((Int, Int), a)]]
index = zipWith zip idx
  where
    idx = [[(i, j) | i <- [1..]] | j <- [1..]]

tile :: CharParser st Tile
tile = do tn <- tileNumber
          rows <- sepEndBy (many1 (oneOf ['#', '.'])) newline
          return Tile{
            num = tn, 
            dat = A.array ((1, 1), (length (head rows), length rows)) (concat (index rows))
          }


main :: IO ()
main = do
  inpt <- getContents
  let
    tiles = fromRight [] (parse (sepBy1 tile newline <* eof) "" inpt)
  print (length tiles)

