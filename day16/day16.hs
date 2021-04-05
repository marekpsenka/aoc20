import Data.Char
import Data.List.Split

data Rule = Rule { field :: String
                 , lmin :: Int
                 , lmax :: Int
                 , umin :: Int
                 , umax :: Int
                 } deriving (Show)

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
  
main :: IO ()
main = do
  ss <- getLinesUntilBlank $ repeat getLine
  print (readRule (head ss))
  print ss