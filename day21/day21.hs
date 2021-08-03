import Data.List
import Data.List.Split
import qualified Data.Map as Map

data Food = Food {ings :: [String], algs :: [String]} deriving(Show)

type SuspectMap = Map.Map String [String]

parseFood :: String -> Food
parseFood s = Food { ings = words left, algs = splitOn ", " (init right) }
  where
    left:right:_ = splitOn " (contains " s

getSuspects :: SuspectMap -> Food -> SuspectMap
getSuspects m f = foldl g m (algs f)
  where
    g m' a = Map.update (\ss -> Just $ ss `intersect` ings f) a m'

initializeSuspects :: [Food] -> SuspectMap
initializeSuspects fs = Map.fromList (map (\a -> (a, uniqueIs)) uniqueAs)
  where
    uniqueIs = foldl union [] (map ings fs)
    uniqueAs = foldl union [] (map algs fs)

main :: IO ()
main = do
  foods <- map parseFood . lines <$> getContents
  let
    sMap = foldl getSuspects (initializeSuspects foods) foods
    ss = nub $ concat (Map.elems sMap)
    is = concatMap ings foods
    safe = nub is \\ ss
  print (sum $ map (\i -> length $ filter (i ==) is) safe)