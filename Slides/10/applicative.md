# Przypomnienie - Funktory

Funktor to operacja `T :: * -> *` na typach  
wraz z operacją `fmap` na funkcjach 

~~~~ {.haskell}
fmap :: (a -> b) -> (T a -> T b) 
~~~~

zachowującą strukturę składania funkcji, czyli

~~~~
fmap id = id
fmap (f . g) = fmap f . fmap g
~~~~

# Motywacja - sekwencje

Przypomnijmy sobie parser dla cyfr:

~~~~ {.haskell}
pNum :: Parser Int
pNum = fmap digitToInt digit
~~~~

co jest krótszym zapisem

~~~~ {.haskell}
pNum = do
     d <- digit
     return $ digitToInt d
~~~~

# Motywacja - sekwencje

Przypomnijmy sobie funkcję `sequence` (tu dla monady IO):

~~~~ {.haskell}
sequence1 :: [IO a] -> IO [a]
sequence1 [] = return []
sequence1 (c : cs) = do
  x <- c
  xs <- sequence1 cs
  return (x : xs)
~~~~

która "wykonuje" listę akcji zbierając ich wyniki w liste.

Możemy ją zapisac prościej uzywając "monadycznej" aplikacji, `ap`:

~~~~ {.haskell}
sequence2 (c:cs) = return (:) `ap` c `ap` sequence cs

ap mf mx = do
  f <- mf
  x <- mx
  return $ f x
~~~~

# Motywacja - `zipWith`

~~~~ {.haskell}
zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
-- zipWith_n :: (a1 -> ... -> an -> b) -> [a1] -> ... -> [an] -> [b]
-- zipWith_n f as1 ... asn = repeat f `zap` as1 `zap` ... `zap` asn

zap :: [a->b] -> [a] -> [b]
zap (f:fs) (x:xs) = f x:zap fs xs
zap _ _ = []

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = repeat (:) `zap` xs `zap` transpose xss
-- zipWith (:) xs (transpose xss)
~~~~

# Motywacja - interpreter

~~~~ {.haskell}
data Exp v = Var v
     	   | Val Int
           | Add (Exp v) (Exp v)

eval1 :: Exp v -> Env v -> Int
eval1 (Var x) env = fetch x env
eval1 (Val i) env = i
eval1 (Add p q) env = eval1 p env + eval1 q env

eval2 (Var x) = fetch x
eval2 (Val i) = k i
eval2 (Add p q) = k (+) `s` eval p `s` eval q

k x y = x
s :: (env -> a -> b) -> (env -> a) -> (env -> b)
s ef es env = (ef env)
~~~~

# Klasa Applicative

~~~~ {.haskell}
-- Control.Applicative
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  
~~~~

Przykład:

~~~~ {.haskell}
instance Applicative Maybe where
  pure = Just
  (Just f) <*> (Just x) = Just (f x)
  _        <*> _ = Nothing
  
-- >>> fmap (+1) (Just 5)
-- Just 6
-- >>> pure (+1) <*> Just 5
-- Just 6
-- >>> pure (+) <*> Just 2 <*> Just 2
-- Just 4
~~~~

# Prawa

~~~~
fmap g x = pure g <*> x
pure id <*> u = u (konsekwencja powyższego i praw fmap)
pure (.) <*> u <*> v <*> w =  u <*> v <*> w
pure f <*> pure x = pure (f x)
u <*> pure x = pure (\f -> f x) <*> u
~~~~

W  stylu aplikatywnym fmap zapisujemy jako `<$>`:

~~~~ {.haskell}
f <$> u = pure f <*> u
~~~~

**Ćwiczenie:** sprawdź, że powyższe prawa zachodzą dla podanej instancji dla `Maybe`.

# Przykłady idiomatycznie

~~~~ {.haskell}
sequence3 (c:cs) = (:) <$> c <*> sequence cs

instance Applicative [] where
  pure = repeat
  (f : fs) <*> (x : xs) = f x : (fs <*> xs)
  _        <*> _        = []
  
transpose2 :: [[a]] -> [[a]]
transpose2 [] = pure []
transpose2 (xs :xss) = (:) <$> xs <*> transpose2 xss

instance Applicative ((->) env) where
  pure = const
  ef <*> es = \env -> (ef env) (es env)

eval3 (Var x) = fetch x
eval3 (Val i) = pure i
eval3 (Add p q) = pure (+) <*> eval3 p <*> eval3 q
~~~~

# Nawiasy idiomatyczne (idiom brackets)

Conor McBride zaproponował specjalną notację idiomatyczną:

~~~~ 
 (|f a1 .. an|) = pure f <*> a1 <*> .. <*> an
sequence4 (c:cs) = (| (:) c (sequence cs) |)
eval4  (Add p q) = (| (+) (eval3 p) (eval3 q) |)
~~~~

nie weszła ona do standardu Haskella, choć jest dostępna w SHE.

https://personal.cis.strath.ac.uk/~conor/pub/she/

# Nawiasy idiomatyczne (idiom brackets)

Przy pomocy klas możemy udostępnić podobną, choć brzydszą notację:

~~~~ {.haskell}
sequence4 (c:cs) = iI (:) c (sequence cs) Ii
eval4  (Add p q) = iI (+) (eval3 p) (eval3 q) Ii

class Applicative i => Idiomatic i f g | g -> f i where
   idiomatic :: i f -> g
 
iI :: Idiomatic i f g => f -> g
iI = idiomatic . pure
 
data Ii  =  Ii
 
instance Applicative i    => Idiomatic i x (Ii -> i x) where
  idiomatic xi Ii     = xi
 
instance Idiomatic i f g  => Idiomatic i (s -> f) (i s -> g) where
  idiomatic sfi si    = idiomatic (sfi <*> si)

~~~~

# Idiomy a monady

Każda monada jest funktorem aplikatywnym, np.

~~~~ {.haskell}
instance Applicative Maybe where
  pure = return
  (<*>) = ap

ap mf mx = mf >>= \f -> m x >>= \x -> return (f x)
~~~~

Natomiast w ogólności nie na odwrót, np. nasza instancja dla list

~~~~ {.haskell}
instance Applicative [] where
  pure = repeat
  (f : fs) <*> (x : xs) = f x : (fs <*> xs)
  _        <*> _        = []
~~~~

czy da się zdefiniować `>>=` tak by `ap` odpowiadało `<*>` ?

*Ćwiczenie:* sprawdź, że prawa dla idiomów wynikają z praw dla monad.

# Idiomy a monady

Strukturze monadycznej dla list odpowiada inna instancja Applicative dla list, gdzie listę funkcji aplikujemy do listy argumentów metodą "każdy z każdym":

~~~~ {.haskell}
instance Applicative [] where
  pure = (:[])
  fs <*> xs = concat $ for fs (for xs)
  
for = flip map
~~~~

**Ćwiczenie:** wykaż poprawność powyższej definicji

**Ćwiczenie:** napisz dwie instancje Applicative dla State.

# Idiomy a monady

W ogólności sekwencjonowanie monadyczne jest  silniejsze od idiomatycznego:

~~~~ {.haskell}
mif c t e = do { b <- c; if b then t else e }

aif fc ft fe = cond <$> fc <*> ft <*> fe where
  cond c t e =if c then t else e

main = do
  putStrLn "Monad:"
  mif (return True) (putStrLn "True") (putStrLn "False")
  putStrLn "Idiom:"
  aif (pure True) (putStrLn "True") (putStrLn "False")
~~~~

~~~~
Monad:
True
Idiom:
True
False
~~~~

# Monoid

~~~~ {.haskell}
-- Data.Monoid
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty
~~~~

Monoid, aplikatywnie:

~~~~ {.haskell}
-- typ fantomowy: nie używa swojego argumentu
newtype Accy o a = Acc{acc::o}

instance Monoid o => Applicative (Accy o) where
  pure _ = Acc mempty
  Acc o1 <*> Acc o2 = Acc (o1 `mappend` o2)
~~~~

nie jest monadyczne, bo jak zdefiniować

~~~~ {.haskell}
(>>=) :: Accy o a -> (a->Accy o b) -> Accy o b
~~~~

# Akumulowanie błędów

~~~~ {.haskell}
data Except err a = Ok a | Failed err

instance Monoid err => Applicative (Except err) where
  pure = Ok
  Ok f <*> Ok x = Ok (f x)
  Ok _ <*> Failed err = Failed err
  Failed err <*> Ok _ = Failed err
  Failed e1 <*> Failed e2 = Failed (e1 `mappend` e2)
~~~~

trudno zrobić analog monadyczny

# Składanie idiomów

Składanie monad jest trudne (i nie zawsze możliwe). 

Składanie idiomów jest łatwe

~~~~ {.haskell}
newtype (g :. f) a = O { unO :: (g (f a)) }

instance (Applicative g, Applicative f) => Applicative (g :. f) where
  pure  = O . pure . pure
  O gs <*> O xs = -- O (| (<*>) gs xs |) 
                  O ( (<*>) <$> gs <*> xs)
~~~~

**Ćwiczenie:** zdefiniować

~~~~ {.haskell}
instance (Functor g, Functor f) => Functor (g :. f) where ...
~~~~

i sprawdzić, że złożenie funktorów aplikatywnych spełnia prawa dla funktora aplikatywnego.

# Kategoryjnie: strong lax monoidal functor

~~~~ {.haskell}
class Functor f => Monoidal f where                  
  unit :: f ()
  pair :: f a -> f b -> f (a,b)
  
instance Applicative f => Monoidal f where
  unit = pure ()
  pair fa fb = (,) <$> fa <*> fb
  
instance Monoidal f => Applicative f where  
  pure x = fmap (const x) unit
  mf <*> mx = fmap ($) (pair mf mx)
~~~~

Żeby uzyskać prawdziwą równoważność trzeba oczywiście mieć pewne prawa
dla Monoidal. Okazuje się, że jest to coś, co w teori kategorii nazywa
się *strong lax monoidal functor* ;-)

# Parsery

Zauważmy natomiast, że `Monoidal` jest naturalną strukturą dla parserów

~~~~ {.haskell}
class Functor f => Monoidal f where                  
  unit :: f ()
  pair :: f a -> f b -> f (a,b)

emptyP :: Parser ()
thenP :: Parser a -> Parser b -> Parser (a,b)
~~~~

tyle, że typy robią się skomplikowane, dlatego łatwiej używać `Applicative`

~~~~ {.haskell}
-- S   ->  ( S ) S | epsilon
parens = (\_ s _ s2 -> max (1+s) s2) <$> 
         char '(' <*> parens <*> char ')' <*> parens 
         <|> pure 0
~~~~

# Alternative

Potrzebujemy jeszcze tylko alternatywy:

~~~~ {.haskell}
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
~~~~

spełniającej aksjomaty monoidu. Patrz też MonadPlus:

~~~~ {.haskell}
class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

-- mzero >>= f  =  mzero
-- v >> mzero   =  mzero
~~~~

# Koniec

~~~~ {.haskell}

~~~~
