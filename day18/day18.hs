
import Text.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Data.Either

natural :: CharParser st Int
natural = read <$> many1 digit 

term1 :: CharParser st Int
term1 = try (between (char ')') (char '(') expr1) <|> natural

expr1 :: CharParser st Int
expr1 = try (do {f <- term1; string " * "; g <- expr1; return (f * g)})
    <|> try (do {f <- term1; string " + "; g <- expr1; return (f + g)})
    <|> term1

term2 :: CharParser st Int
term2 = try (between (char '(') (char ')') expr2) <|> natural

factor2 :: CharParser st Int
factor2 = try (do {f <- term2; string " + "; g <- factor2; return (f + g)})
      <|> term2

expr2 :: CharParser st Int
expr2 = try (do {f <- factor2; string " * "; g <- expr2; return (f * g)})
    <|> factor2

main :: IO ()
main = do
  inpt <- lines <$> getContents
  print (sum (map (fromRight 0 . parse expr1 "" . reverse) inpt))
  print (sum (map (fromRight 0 . parse expr2 "") inpt))


