# Parsowanie idiomatyczne

Parsec:

~~~~ {.haskell}
pExp = pNum `chainl1` addop
addop =  do{ char '+'; return (+) }
     <|> do{ char '-'; return (-) }
~~~~

albo

~~~~ {.haskell}
addop  =  (const (+)) <$> char '+'
      <|> (const -) <$> char '-'
~~~~

Idiom:

~~~~ {.haskell}
pExp = pNum `chainl1` addop
addop   =  (+) <$ char '+'
       <|> (-) <$ char '-' 
~~~~


~~~~ {.haskell}
-- S   ->  ( S ) S | epsilon
parens = depth  <$> 
         char '(' <*> parens <*> char ')' <*> parens 
         <|> pure 0 where
	 depth _ s _ s2 = max (1+s) s2
~~~~


# Parser niedeterministyczny

~~~~ {.haskell}
newtype Parser a = Parser{ runParser :: String -> [(a,String)] }

parse p name input  = case runParser p input of
  [] -> Left "no parse"
  (x:_)-> Right x

satisfy :: (Char->Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  (c:cs) | p c -> [(c,cs)]
  _ -> []

digit :: Parser Char
digit = satisfy isDigit

test1 = runParser digit "123"
-- >>> test1
-- [('1',"23")]
~~~~

# Functor

~~~~ {.haskell}
first :: (a->b) -> (a,c) -> (b,c)
first f (a,c) = (f a,c)
for = flip map   
       
instance Functor Parser where
  -- (a->b) -> Parser a -> Parser b
  fmap f p = Parser $ \s -> for (runParser p s) (first f)

pNat1 :: Parser Int
pNat1 = fmap digitToInt digit

-- >>> runParser pNat1 "123"
-- [(1,"23")]
~~~~

# Applicative

~~~~ {.haskell}
instance Applicative Parser where
  pure a = Parser $ \s -> [(a,s)]
  (Parser pab) <*> (Parser pa) = Parser $ \s -> 
    [(f a,s2) | (f,s1) <- pab s, (a,s2) <- pa s1]
    
instance Alternative Parser where
  empty = Parser $ const []
  (Parser p) <|> (Parser q) = Parser $ \s -> p s ++ q s
~~~~

* Ciągi

~~~~ {.haskell}
opt :: Parser a -> a -> Parser a
p `opt` v = p <|> pure v
-- inna implementacja później

many, many1 :: Parser a -> Parser [a]
many p  = many1 p `opt` []
many1 p = (:) <$> p <*> many p    

pNat :: Parser Int
pNat = foldl adder 0 <$> (many1 pNat1) where
  adder :: Int -> Int -> Int
  adder n d = 10*n+d 

-- >>> > runParser pNat "123"
-- [(123,""),(12,"3"),(1,"23")]
~~~~

# Funkcje pomocnicze (ogólne dla Applicative):

~~~~ {.haskell}
-- | Replace the value.  
(<$) :: Functor f => a -> f b -> f a
(<$) = (<$>) . const

-- | Lift a binary function to actions.
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

-- | Sequence actions, discarding the value of the first argument.
(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (const id)
 
-- | Sequence actions, discarding the value of the second argument.
(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const
 
-- | A variant of '<*>' with the arguments reversed.
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (flip ($))
~~~~

~~~~ {.haskell}
addop   =  (+) <$ char '+'
       <|> (-) <$ char '-' 

chainr1 x op = x <**> f where
  -- f :: Parser (a->a->a)
~~~~


# Od DFS do DFS: Steps

* Zwykle parsowanie oznacza obejście drzewa możliwości DFS-em

* Zamiana na BFS może usprawnić i dac nowe możliwości

* Idea: wynikiem parsera będzie ciąg kroków, leniwa ewaluacja umozliwi 
  *równoległe* sprawdzanie składników alternatywy i wybór lepszego wariantu:

~~~~ {.haskell}
data Steps a where 
  Step :: Steps a -> Steps a
  Fail :: Steps a
  Done :: a -> Steps a
  deriving Show

best :: Steps a -> Steps a -> Steps a
best Fail r = r
best l Fail = l
best (Step l) (Step r) = Step $ best l r -- NB laziness!
best x@(Step _) (Done _) = x -- prefer longer parse
best (Done _) x@(Step _) = x
best _ _ = error "incorrect parser" -- ambiguity
~~~~


# Recogniser

Najpierw recogniser --- parser, który tylko bada przynależność do języka:

~~~~ {.haskell}
newtype R a = R {unR :: forall r.(String -> Steps r) -> String -> Steps r}
type Parser a = R a

runParser :: Parser a -> String -> Steps String
runParser (R p) input = p (\r -> Done r) input
-- >>> runParser digit "123"
-- Step (Done "23")

satisfy :: (Char->Bool) -> R Char
satisfy p = R $ \k s -> case s of
  (c:cs) | p c -> Step (k cs)
  _ -> Fail
~~~~

NB

* `R a` ignoruje `a`, bo nie daje wyniku

* pierwszym argumentem parsera jest kontynuacja mówiąca, co zrobić
  z resztą wejścia.

# Applicative

~~~~ {.haskell}
instance Functor R where
  -- (a->b) -> R a -> R b
  fmap f (R q) = R q
  
instance Applicative R where
  pure a = R ($) 
  (R p) <*> (R q) = R (\k s -> p (q k) s)
    
instance Alternative R where
  empty = R $ \k s -> Fail
  (R p) <|> (R q) = R (\k s -> p k s `best` q k s)
~~~~

NB

* `fmap` trywialne --- `R` jest funktorem stałym
* `pure` ignoruje argument, aplikuje kontynuację
* `<*>` składa kontynuacje
* `<|>` wybiera lepszą alternatywę (lenistwo!)

# Jeszcze sekwencje

~~~~ {.haskell}
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
~~~~

# Monoid Steps

~~~~ {.haskell}
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty

instance Monoid b => Monoid (a -> b) where
  mempty = const mempty
  mappend f g = \x ->f x `mappend` g x

instance Monoid (Steps a) where
  mempty = Fail
  mappend = best

instance Alternative R where
  empty = R $ mempty  
  (R p) <|> (R q) = R $ p `mappend` q
~~~~

# Polityka historyczna, czyli parsery z przeszłościa

* Idea: funkcje do zaaplikowania odkładamy na stos, tworząc "historię"

* Kontynuacja zalezy od historii i obliczonej wartości

~~~~ {.haskell}
newtype Ph a = Ph {unPh :: forall r h.
                           ((h,a) -> String -> Steps r) 
                           -> h -> String -> Steps r}
type Parser a = Ph a
runParser (Ph p) input = p (\(h,a) -> (\s -> Done a)) () input

~~~~

Aplikacja funkcji ze szczytu stosu:

~~~~ {.haskell}
aph :: ((h,b->a),b) -> (h,a)
aph ((h,b2a),b) = (h,b2a b)

type Cont h a = forall r.(h,a) -> r
coaph :: Cont h b  -> Cont (h, (a->b)) a
coaph k ((h,b2a),b) = k (h,b2a b)
~~~~

# Functor

~~~~ {.haskell}
instance Functor Ph where
  -- (a->b) -> (forall r h. ((h,a) -> String -> Steps r) 
  --                      -> h -> String -> Steps r)
  --        -> (forall r h. ((h,b) -> String -> Steps r) 
  --                      -> h -> String -> Steps r)
  fmap f (Ph pka) = Ph (\kb -> pka (kb . second f))

-- pka: parser z kontynuacją dla a
-- kb : kontynuacja dla b
second :: (a->b) -> (d,a) -> (d,b)
second f (d,a) = (d,f a)
~~~~

# Applicative

~~~~ {.haskell}
instance Applicative Ph where
  pure a = Ph (\k h  -> k (h,a) )
  -- Ph (a->b) -> Ph a -> Ph b
  (Ph p) <*> (Ph q) = Ph (\k -> p (q (\((h,b2a),b)->k(h,b2a b)))) 
    -- Ph(\k -> p (q (coaph k))) 

coaph :: Cont h b  -> Cont (h, (a->b)) a
coaph k ((h,b2a),b) = k (h,b2a b)
~~~~

`Alternative` jest łatwe dzieki `Monoid`:

~~~~ {.haskell}
instance Alternative Ph where
  empty = Ph $ mempty
  (Ph p) <|> (Ph q) = Ph $ p `mappend` q
~~~~

NB to wygląda analogicznie jak dla `R`, ale to jest jednak inna funkcja
--- "magia" odbywa się tu:

~~~~ {.haskell}
instance Monoid b => Monoid (a -> b) where
  mempty = const mempty
  mappend f g = \x ->f x `mappend` g x
~~~~


# Odrzućmy balast historii

Skoro i tak używamy kontynuacji, to możemy sobie darować "przekładanie papierków"

~~~~ {.haskell}
newtype Ph a = Ph {unPh :: forall r.
                           (a -> String -> Steps r) 
                              -> String -> Steps r}
runParser (Ph p) input = p (\a s -> checkEmpty a s) input where
  checkEmpty a [] = Done a
  checkEmpty a _ = Fail

instance Functor Ph where
  fmap f (Ph p) = Ph (\k -> p (k . f))
                              
instance Applicative Ph where
  pure a = Ph (\k -> k a)
  (Ph p) <*> (Ph q) = Ph (\k -> p (\f -> q (k . f)))
~~~~

# Problem

Jesli drzewo możliwości ma rozmiar wykładniczy, to obejście go BFS
niekoniecznie jest najlepszym pomysłem...

~~~~ {.haskell}
pExp = (pNum `chainl1` addop) <* eof
addop   =  (+) <$ char '+'
       <|> (-) <$ char '-' 
~~~~

# Greed is good

Rozwiązaniem może być "zachłanna" alternatywa (jak w Parsec-u):

~~~~ {.haskell}
(<<|>) :: Parser a -> Parser a -> Parser a
(Ph p) <<|> (Ph q) = Ph (\k s -> p k s `bestg` q k s)

bestg l@(Step _) _ = l -- greeedy
bestg l r = best l r

opt :: Parser a -> a -> Parser a
p `opt` v = p <<|> pure v

many p  = (:) <$> p <*> many p `opt` [] -- NB greedy
chainl1 :: Parser a -> Parser (a->a->a) -> Parser a
chainl1 pt pop = applyAll <$> pt <*> many (flip <$> pop <*> pt)
~~~~

# Obsługa błędów

~~~~ {.haskell}
data Steps a where 
  Fail :: [String] -> Steps a
  ...

best :: Steps a -> Steps a -> Steps a
best (Fail ls)(Fail rs) = Fail (ls++rs)
...

satisfy p = Ph $ \k s -> case s of
  (c:cs) | p c -> Step (k c cs)
         | otherwise -> Fail ["unexpected " ++ [c]]
  [] -> Fail ["unexpected EOF"]

parse p name input =  result (runParser p input) where
  result (Done a) = Right a
  result (Fail msgs) = Left msgs
  result (Step x) = result x
~~~~

# Naprawa błędów


# UU-parsinglib

* Utrecht University, Doaitse Swierstra

<http://hackage.haskell.org/package/uu-parsinglib>


~~~~ {.haskell}
pNum :: Parser Int
pNum = fmap digitToInt digit

chainl1 = flip pChainl
pExp = pNum `chainl1` addop
addop   =  (+) <$ char '+'
       <<|> (-) <$ char '-' 
       
-- pEnd :: Parser [Error]
doparse :: Parser a -> String -> String -> a
doparse p name input = let 
  extp = ( (,) <$> p <*> pEnd) 
  str = (createStr (LineColPos 0 0 0) input)
  (a, errors) =  parse extp str 
  in a {-case errors of
     [] -> a
     (e:_) -> error $ show e
  -}
~~~~

# Ćwiczenia

* Używając uu-parsinglib napisz parser dla wyrażeń budujący drzewo struktury.

* Stanem dotychczasowych parserów był zawsze `String`. Przerób je tak, by uzywały abstrakcyjnego stanu, mogącego dotatkowo przechowywać pozycję i liste błedówe, np.

~~~~ {.haskell}
class ParserState st where
  splitState :: st -> Maybe (Char,st) -- albo Maybe(Token,st)
  isEof :: Bool
  getErrors :: Errors  -- np. [String]
~~~~

# ExtAlternative


~~~~ {.haskell}
class (Alternative p, Applicative p, ExtAlternative p) => IsParser p 

class (Alternative p) => ExtAlternative p where
   -- | `<<|>` is the greedy version of `<|>` (a la Parsec). 
   (<<|>)  :: p a -> p a -> p a
   -- |  The `<?>` combinator replaces this list of symbols by the string argument.   
   (<?>)   :: p a -> String -> p a
   -- | `doNotInterpret` makes a parser opaque for abstract interpretation; 
   doNotInterpret :: p a -> p a
   doNotInterpret = id
   -- |  `must_be_non_empty` checks whether its second argument
   --    is a parser which can recognise the empty input. 
   must_be_non_empty   :: String -> p a ->        c -> c
   must_be_non_empties :: String -> p a -> p b -> c -> c 
   -- | If 'p' can be recognized, the return value of 'p' is used. Otherwise,
   --   the value 'v' is used. Note that `opt` by default is greedy. 
   opt     :: p a ->   a -> p a
   opt p v = must_be_non_empty "opt" p (p <<|> pure v)   
~~~~

# Koniec

~~~~ {.haskell}

~~~~
