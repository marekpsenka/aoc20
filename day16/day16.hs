import Data.Char
import Data.List
import Data.List.Split ( splitOn, startsWith )
import qualified Data.Map as Map
import Data.Maybe

data Rule = Rule { field :: String
                 , lmin :: Int
                 , lmax :: Int
                 , umin :: Int
                 , umax :: Int
                 } deriving (Show, Eq, Ord)

type Ticket = [Int]

getLinesUntilBlank :: [IO String] -> IO [String]
getLinesUntilBlank = getLinesUntilBlank' []
  where
    getLinesUntilBlank' :: [String] -> [IO String] -> IO [String]
    getLinesUntilBlank' rs (is:iss) = do
      s <- is
      if all isSpace s then return rs else getLinesUntilBlank' (s:rs) iss

readRule :: String -> Rule
readRule s = let
    field':rest:_ = splitOn ": " s
    sl:su:_ = splitOn " or " rest
    slmin:slmax:_ = splitOn "-" sl
    sumin:sumax:_ = splitOn "-" su
  in
    Rule { field=field'
          , lmin=read slmin :: Int
          , lmax=read slmax :: Int
          , umin=read sumin :: Int
          , umax=read sumax :: Int
          }

readTicket :: String -> Ticket
readTicket = map (read :: String -> Int) . splitOn ","

isValidAgainst :: Int -> Rule -> Bool
isValidAgainst i r = (lmin r <= i && i <= lmax r) ||
               (umin r <= i && i <= umax r)

invalidFieldsSum :: Ticket -> [Rule] -> Int
invalidFieldsSum t rs = sum invalid
  where
    invalid = filter (\i -> not (any (isValidAgainst i) rs)) t

isValid :: [Rule] -> Ticket -> Bool
isValid rs = all (\i -> any (isValidAgainst i) rs)

findEnds :: Map.Map Rule Int -> [[Rule]] -> [(Rule, Int)]
findEnds explained rs  = map (\(mr, i) -> (fromJust mr, i)) a
  where
    a = filter (isJust . fst) (zip (map (checkEnd explained) rs) [0..])
    checkEnd :: Map.Map Rule Int -> [Rule] -> Maybe Rule
    checkEnd explained cnds = f (filter (not . \k -> Map.member k explained) cnds)
      where
        f [x] = Just x
        f _ = Nothing

unknot :: [[Rule]] -> Map.Map Rule Int
unknot rs = unknot' rs Map.empty
  where
    unknot' :: [[Rule]] -> Map.Map Rule Int -> Map.Map Rule Int
    unknot' rs explained = 
      if Map.size explained == length rs 
        then explained
        else unknot' rs (foldl (\m (r, i) -> Map.insert r i m) explained (findEnds explained rs)) 
      
  
main :: IO ()
main = do
  ss <- getLinesUntilBlank $ repeat getLine
  let rules = map readRule ss

  mt:_ <- getLinesUntilBlank $ repeat getLine
  let myticket = readTicket mt

  _:ts <- lines <$> getContents 
  let tickets = map readTicket ts

  let valid = filter (isValid rules) tickets
      candidates = map (\col -> filter (\rule -> all (`isValidAgainst` rule) col) rules) (transpose valid)
      fieldIndices = map snd (filter (isPrefixOf "departure" . field . fst) (Map.toList (unknot candidates)))

  print (sum $ map (`invalidFieldsSum` rules) tickets)
  print (product (map (myticket !!) fieldIndices))