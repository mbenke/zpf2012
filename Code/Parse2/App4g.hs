{-# LANGUAGE ExplicitForAll, RankNTypes #-}
{-# LANGUAGE GADTs #-} -- for syntax only
module App4g where
import Data.Char(isDigit,digitToInt)

import Applicative
import Monoid

data Steps a where 
  Step :: Steps a -> Steps a
  Fail :: Steps a
  Done :: a -> Steps a
  deriving Show

noAlts :: Steps a
noAlts      =  Fail

best :: Steps a -> Steps a -> Steps a
best Fail r = r
best l Fail = l
best (Step l) (Step r) = Step $ best l r -- OBS laziness!
-- best x@(Step _) (Done _) = x -- prefer longer parse
-- best (Done _) x@(Step _) = x
best _ _ = error "incorrect parser" -- ambiguity

bestg :: Steps a -> Steps a -> Steps a
bestg l@(Step _) _ = l -- greeedy
bestg l r = best l r


(<<|>) :: Parser a -> Parser a -> Parser a
(Ph p) <<|> (Ph q) = Ph (\k s -> p k s `bestg` q k s)

instance Monoid (Steps a) where
  mempty = noAlts
  mappend = best
  
eval :: Steps a -> a
eval (Step l) = eval l
eval (Done v) = v
eval Fail = error "this should not happen: eval Fail"

newtype Ph a = Ph {unPh :: forall r.
                           (a -> String -> Steps r) 
                              -> String -> Steps r}
type Parser a = Ph a

runParser :: Ph a -> String -> Steps a
runParser (Ph p) input = p (\a s -> checkEmpty a s) input where
  checkEmpty a [] = Done a
  checkEmpty a _ = Fail

-- parse :: Ph a -> String -> String -> Either () a

parse p name input =  result (runParser p input) where
  result (Done a) = Right a
  result Fail = Left ()
  result (Step x) = result x
  
satisfy :: (Char->Bool) -> Ph Char
satisfy p = Ph $ \k s -> case s of
  (c:cs) | p c -> Step (k c cs)
  _ -> Fail

eof :: Ph ()
eof = Ph $ \k s -> case s of
 [] -> k () []
 _ -> Fail

first :: (a->b) -> (a,c) -> (b,c)
first f (a,c) = (f a,c)
second :: (a->b) -> (d,a) -> (d,b)
second f (d,a) = (d,f a)
for = flip map   
       
instance Functor Ph where
  -- (a->b) -> (forall r. (a -> String -> Steps r) 
  --                      -> String -> Steps r)
  --        -> (forall r h. (b -> String -> Steps r) 
  --                      -> String -> Steps r)
  fmap f (Ph p) = Ph (\k -> p (k . f))
                              
instance Applicative Ph where
  pure a = Ph (\k -> k a)
  (Ph p) <*> (Ph q) = Ph (\k -> p (\f -> q (k . f)))

instance Alternative Ph where
  empty = Ph $ \k s -> mempty
  (Ph p) <|> (Ph q) = Ph (p `mappend` q)
                      -- (\k s -> p k s `mappend` q k s)
    
many, many1 :: Parser a -> Parser [a]
many p  = (:) <$> p <*> many p <<|> pure [] -- NB greedy
many1 p = (:) <$> p <*> many p
                         
digit :: Parser Char
digit = satisfy isDigit

char :: Char -> Parser Char
char c = satisfy (==c)

test1 = runParser digit "123"

test2 = runParser  (many1 digit <* eof) "123"


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
