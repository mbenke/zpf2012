{-# LANGUAGE ExplicitForAll, RankNTypes #-}
{-# LANGUAGE GADTs #-} -- for syntax only
module App2 where
import Applicative
import Monoid
import Data.Char(isDigit,digitToInt)

data Steps a where 
  Step :: Steps a -> Steps a
  Fail :: Steps a
  Done :: a -> Steps a
  deriving Show

best :: Steps a -> Steps a -> Steps a
best Fail r = r
best l Fail = l
best (Step l) (Step r) = Step $ best l r -- OBS laziness!
best x@(Step _) (Done _) = x -- prefer longer parse
best (Done _) x@(Step _) = x
best _ _ = error "incorrect parser" -- ambiguity

instance Monoid (Steps a) where
  mempty = Fail
  mappend = best

eval :: Steps a -> a
eval (Step l) = eval l
eval (Done v) = v
eval Fail = error "this should not happen: eval Fail"

newtype R a = R {unR :: forall r.(String -> Steps r) -> String -> Steps r}
type Parser a = R a

runParser :: Parser a -> String -> Steps String
runParser (R p) input = p (\r -> Done r) input

-- >>> runParser digit "123"
-- Step (Done "23")

parse :: R a -> String -> String -> Bool
parse p name input = result (runParser p input) where
  result (Done _) = True
  result Fail = False
  result (Step x) = result x
  

{-
type Parser a = Recogniser String a
parse p name input  = case runParser p input of
  [] -> Left "no parse"
  (x:_)-> Right x
-}

satisfy :: (Char->Bool) -> R Char
satisfy p = R $ \k s -> case s of
  (c:cs) | p c -> Step (k cs)
  _ -> Fail

eof :: R ()
eof = R $ \k s -> case s of
 [] -> Step $ k []
 _ -> Fail
 
first :: (a->b) -> (a,c) -> (b,c)
first f (a,c) = (f a,c)
for = flip map   
       
-- Functor instance trivial since we have no result
instance Functor R where
  -- (a->b) -> R a -> R b
  fmap f (R q) = R q
  

instance Applicative R where
  pure a = R ($) 
  (R p) <*> (R q) = R (\k s -> p (q k) s)
    
instance Alternative R where
--  empty = R $ \k s -> Fail
--  (R p) <|> (R q) = R (\k s -> p k s `best` q k s)
  empty = R $ mempty  
  (R p) <|> (R q) = R $ p `mappend` q
  
many, many1 :: Parser a -> Parser [a]
many p  = (:) <$> p <*> many p <|> pure []
many1 p =(:) <$> p <*> many p
                         
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
