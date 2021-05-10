import Data.Char
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Control.Monad
import Data.Either
import Data.List

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


buildParser :: [Rule] -> Rule -> CharParser st String
buildParser rs r =
  case r of
    Single c -> count 1 (char c)
    Seq rrs -> concat <$> mapM (buildParser rs . (rs !!)) rrs
    Alt r1 r2 -> try (buildParser rs r1) <|> buildParser rs r2

main :: IO ()
main = do
  inpt <- lines <$> getContents
  let
    (rules, _messages) = break (all isSpace) inpt
    messages = tail _messages
    parsedRules = fromRight [] (mapM (parse rule "") rules)
    sortedRules = map snd (sortOn fst parsedRules)
    p = buildParser sortedRules (head sortedRules) <* eof

  print (length $ filter isRight (map (parse p "") messages))

