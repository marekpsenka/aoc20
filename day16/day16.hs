import Data.Char
import Data.List.Split

data Rule = Rule { field :: String
                 , lmin :: Int
                 , lmax :: Int
                 , umin :: Int
                 , umax :: Int
                 } deriving (Show)

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
  
main :: IO ()
main = do
  ss <- getLinesUntilBlank $ repeat getLine
  let rules = map readRule ss

  mt:_ <- getLinesUntilBlank $ repeat getLine
  let myticket = readTicket mt

  _:ts <- lines <$> getContents 
  let tickets = map readTicket ts

  print (sum $ map (\t -> invalidFieldsSum t rules) tickets)