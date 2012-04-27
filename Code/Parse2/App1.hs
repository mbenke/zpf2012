newtype Parser a = Parser{ runParser :: String -> [(a,String)]}

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
  
class Applicative f where
  pure :: a -> f a
  (<*>)  :: f (a->b) -> f a -> f b
  -- (<$>) :: (a->b) -> f a -> f b
  -- f <$> x  = fmap f x = pure f <*> x

instance Applicative Parser where
  pure a = Parser $ \s -> [(a,s)]
  (Parser pab) <*> (Parser pa) = Parser $ \s -> 
    [(f a,s2) | (f,s1) <- pab s, (a,s2) <- pa s1]