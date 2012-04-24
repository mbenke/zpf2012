module MyParsec2c
       ( testP,
         parse,
         Parser,
         char,
         (<|>),
         module MyParsec2c.Combinators,
  )where
import MyParsec2c.Prim
import MyParsec2c.Combinators
import Data.Char(isDigit,digitToInt)

parse :: Parser a -> String -> String -> Either ParseError a
parse p fname input = case runParser p input of
  Empty (Ok a st) -> Right a
  Consumed (Ok a st) -> Right a
  Consumed(Error e) -> Left e
  Empty(Error e) -> Left e

testP :: Show a => Parser a -> State -> Consumed a
testP = runParser
  
p0 = return ()
test0 = testP p0 ""

p2 = item >> item
test1 = testP p2 "" -- expect "EOF"
test2 = testP p2 "abc" -- "'b', c"

p3 :: Parser String
p3 = p <|> q where
  p = char 'p' >> eof >> return "p"
  q = char 'p' >> char 'q' >> eof >> return "q"
test3 = testP p3 "pq"

p4 :: Parser Int
p4 = fmap digitToInt (digit)
test4a = testP p4 "7"
test4b = testP p4 "x"