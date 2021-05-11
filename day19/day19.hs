import Data.Char
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Control.Monad
import Data.Either
import Data.List

import qualified Data.Map as Map

data Rule = Single Char | Seq [Int] | Alt Rule Rule deriving(Show)

natural :: CharParser st Int
natural = read <$> many1 digit

naturalSeq :: CharParser st [Int]
naturalSeq = sepEndBy1 natural space

anID :: CharParser st Int
anID = natural <* string ": "

single :: CharParser st Rule
single = Single <$> between (char '"') (char '"') letter

sequen :: CharParser st Rule
sequen = Seq <$> naturalSeq

alt :: CharParser st Rule
alt = do ids1 <- naturalSeq;
         _ <- string "| ";
         ids2 <- naturalSeq;
         return (Alt (Seq ids1) (Seq ids2))

rule :: CharParser st (Int, Rule)
rule = do i <- anID
          r <- try alt <|> try sequen <|> single
          return (i, r)


buildParser :: Map.Map Int Rule -> Rule -> CharParser st String
buildParser rs r =
  case r of
    Single c -> count 1 (char c)
    Seq rrs -> concat <$> mapM (buildParser rs . (rs Map.!)) rrs
    Alt r1 r2 -> try (buildParser rs r1) <|> buildParser rs r2

main :: IO ()
main = do
  inpt <- lines <$> getContents
  let
    (rules, _messages) = break (all isSpace) inpt
    messages = tail _messages
    parsedRules = fromRight [] (mapM (parse rule "") rules)
    ruleMap = foldl (\m (k, v) -> Map.insert k v m) Map.empty parsedRules
    p = buildParser ruleMap (ruleMap Map.! 0) <* eof
    r42 = buildParser ruleMap (ruleMap Map.! 42)
    r31 = buildParser ruleMap (ruleMap Map.! 31)
    p2 = do ss1 <- many1 (try r42)
            ss2 <- many1 r31 <* eof
            guard $ length ss1 > length ss2
            return (concat ss1 ++ concat ss2)

  print (length $ filter isRight (map (parse p "") messages))
  print (length $ filter isRight (map (parse p2 "") messages))

