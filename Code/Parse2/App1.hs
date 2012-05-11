{-# LANGUAGE ScopedTypeVariables #-}
module App1 where
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
  
infixl 3 <|>
infixl 4 <$>, <$
infixl 4 <*>, <*, *>, <**>

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>)  :: f (a->b) -> f a -> f b
  -- (<$>) :: (a->b) -> f a -> f b
  -- f <$> x  = fmap f x 
  
-- | Replace the value.  
(<$) :: Functor f => a -> f b -> f a
(<$) = (<$>) . const

-- | A synonym for 'fmap'.
(<$>) :: Functor f => (a -> b) -> f a -> f b
f <$> a = fmap f a



instance Applicative Parser where
  pure a = Parser $ \s -> [(a,s)]
  (Parser pab) <*> (Parser pa) = Parser $ \s -> 
    [(f a,s2) | (f,s1) <- pab s, (a,s2) <- pa s1]
  -- f <$> p = Parser $ \s -> for (runParser p s) (first f)
  -- r <$ p = Parser $ \s -> for (runParser p s) (\(_,s1)->(r,s1))
    
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  
instance Alternative Parser where
  empty = Parser $ const []
  (Parser p) <|> (Parser q) = Parser $ \s -> p s ++ q s
  
many, many1 :: Parser a -> Parser [a]
many p  = many1 p <|> pure []
many1 p =(:) <$> p <*> many p
                         
-- | Sequence actions, discarding the value of the first argument.
(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (const id)
 
-- | Sequence actions, discarding the value of the second argument.
(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const
 
-- | A variant of '<*>' with the arguments reversed.
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))

-- | Lift a function to actions.
-- This function may be used as a value for `fmap` in a `Functor` instance.
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

-- | Lift a binary function to actions.
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

-- | Lift a ternary function to actions.
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c

digit :: Parser Char
digit = satisfy isDigit

char :: Char -> Parser Char
char c = satisfy (==c)

test1 = runParser digit "123"

test2 = runParser (many1 digit) "123"

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