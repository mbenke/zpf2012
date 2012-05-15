module App1 where
import Applicative
import Data.Char(isDigit,digitToInt)

newtype Parser a = Parser{ runParser :: String -> [(a,String)]}
parse p name input  = case runParser p input of
  [] -> Left "no parse"
  (x:_)-> Right x

satisfy :: (Char->Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  (c:cs) | p c -> [(c,cs)]
  _ -> []
    
eof :: Parser ()
eof = Parser $ \s -> case s of
 [] -> [((),[])]
 _ -> []
 
first :: (a->b) -> (a,c) -> (b,c)
first f (a,c) = (f a,c)
for = flip map   
       
instance Functor Parser where
  -- (a->b) -> Parser a -> Parser b
  fmap f p = Parser $ \s -> for (runParser p s) (first f)
  

instance Applicative Parser where
  pure a = Parser $ \s -> [(a,s)]
  (Parser pab) <*> (Parser pa) = Parser $ \s -> 
    [(f a,s2) | (f,s1) <- pab s, (a,s2) <- pa s1]
  -- f <$> p = Parser $ \s -> for (runParser p s) (first f)
  -- r <$ p = Parser $ \s -> for (runParser p s) (\(_,s1)->(r,s1))
    
instance Alternative Parser where
  empty = Parser $ const []
  (Parser p) <|> (Parser q) = Parser $ \s -> p s ++ q s
  
opt :: Parser a -> a -> Parser a
p `opt` v = p <|> pure v
-- inna implementacja później

many, many1 :: Parser a -> Parser [a]
many p  = many1 p `opt` []
many1 p =(:) <$> p <*> many p
                         
digit :: Parser Char
digit = satisfy isDigit

pNat1 :: Parser Int
pNat1 = fmap digitToInt digit

pNat :: Parser Int
pNat = foldl adder 0 <$> (many1 pNat1) where
  adder :: Int -> Int -> Int
  adder n d = 10*n+d
  
char :: Char -> Parser Char
char c = satisfy (==c)

test1 = runParser digit "123"
test2 = runParser pNat1 "123"
test3 = runParser pNat "123"

-- S   ->  ( S ) S | epsilon
parens = (\_ s _ s2 -> max (1+s) s2) <$> 
         char '(' <*> parens <*> char ')' <*> parens 
         <|> pure 0
         
parens2 = (\s s2 -> max (1+s) s2) <$> 
         (char '(' *> parens) <* char ')' <*> parens 
         <|> pure 0 

-- C1 -> T C
-- C -> op T C | empty
chainr1 :: Parser a -> Parser (a->a->a) -> Parser a
-- chainr1 x pop = x <**> f where
chainr1 x pop = (flip ($)) <$> x <*> f where
  -- f :: Parser (a -> a) 
  f = (flip <$> pop <*> chainr1 x pop) <|> pure id

-- pop :: Parser (a->a->a)
-- flip pop :: Parser (a->a->a) 
-- flip <$> pop <*> chainr1 x pop :: Parser (a->a)

applyAll :: a -> [a->a] -> a
applyAll x [] = x
applyAll x (f:fs) = applyAll (f x) fs
-- applyAll x [f] = f x

chainl1 :: Parser a -> Parser (a->a->a) -> Parser a
chainl1 pt pop = applyAll <$> pt <*> many (flip <$> pop <*> pt)