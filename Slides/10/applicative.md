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
  
-- >>> pure id <*> Just 5
-- Just 5
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