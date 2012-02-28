# Operacje na typach

* Prosty przykład:

    ~~~~ {.haskell}
    data Tree a = Leaf | Node a (Tree a) (Tree a)
    ~~~~

* Konstruktory typowe transformują typy

* `Tree` może zamienić np. `Int` w drzewo

+ Funkcje wyższego rzędu transformują funkcje

+ Konstruktory wyższego rzędu transformują konstruktory typów

~~~~ {.haskell}
newtype IdentityT m a = IdentityT { runIdentityT :: m a }
~~~~ 

# Klasy konstruktorowe

* klasy konstruktorowe opisują własności konstruktorów typów:

    ~~~~ {.haskell}
    class Functor f where
      fmap :: (a->b) -> f a -> f b
    instance Functor [] where
      fmap = map

    class Functor f => Pointed f where
       pure :: a -> f a
    instance Pointed [] where
       pure = (:[])

    class Pointed f => Applicative f where
      (<*>) :: f(a->b) -> f a -> f b 
      (*>) :: f a -> f b -> f b
      x *> y = (flip const) <$> x <*> y
      (<*) :: f a -> f b -> f a
      x <* y = const <$> x <*> y

    instance Applicative [] where
      fs <*> xs = concat $ flip map fs (flip map xs)

    class Applicative m => Monad' m where
      (>>=) :: m a -> (a -> m b) -> m b

    (<$>) :: (Functor f) => (a -> b) -> f a -> f b
    (<$>) = fmap
    ~~~~

<!-- 
    liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
    liftA2 f a b = f <$> a <*> b

-->

# Rodzaje (kinds)

* Operacje na wartościach są opisywane przez ich typy

* Operacje na typach są opisywane przez ich rodzaje (kinds)

* Typy (np. `Int`) są rodzaju `*`

* Jednoargumentowe konstruktory (np. `Tree`) są rodzaju `* -> *`

    ~~~~ {.haskell}
    {-#LANGUAGE KindSignatures, ExplicitForAll #-}

    class Functor f => Pointed (f :: * -> *) where
        pure :: forall (a :: *).a -> f a
    ~~~~

* Występują też bardziej złożone rodzaje, np. dla transformatorów monad:

    ~~~~ {.haskell}
    class MonadTrans (t :: (* -> *) -> * -> *) where
        lift :: Monad (m :: *) => forall (a :: *).m a -> t m a
    ~~~~

NB spacje są niezbędne - `::*->*` jest jednym leksemem.

# Klasy wieloparametrowe

* Czasami potrzebujemy opisać nie tyle pojedynczy typ, co relacje między typami:

    ~~~~ {.haskell}
    {-#LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
    class Iso a b where
      iso :: a -> b
      osi :: b -> a
      
    instance Iso a a where
      iso = id
      osi = id

    instance Iso ((a,b)->c) (a->b->c) where
      iso = curry
      osi = uncurry

    instance (Iso a b) => Iso [a] [b] where
     iso = map iso
     osi = map osi
    ~~~~

* Uwaga: w ostatnim przykładzie `iso` ma inny typ po lewej, inny po prawej 

* Ćwiczenie: napisz jeszcze jakieś instancje klasy `Iso`


    ~~~~ {.haskell}
    instance (Functor f, Iso a b) => Iso (f a) (f b) where 
    instance Iso (a->b->c) (b->a->c) where
    ~~~~

# Dygresja - FlexibleInstances

Haskell 2010

<!--
An instance declaration introduces an instance of a class. Let class
cx => C u where { cbody } be a class declaration. The general form of
the corresponding instance declaration is: instance cx′ => C (T u1 …
uk) where { d } where k ≥ 0. The type (T u1 … uk) must take the form
of a type constructor T applied to simple type variables u1, … uk;
furthermore, T must not be a type synonym, and the ui must all be
distinct.
-->

* an instance head must have the form C (T u1 ... uk), where T is a type constructor defined by a data or newtype declaration  and the ui are distinct type variables, and

<!--
*    each assertion in the context must have the form C' v, where v is one of the ui. 
-->

This prohibits instance declarations such as:

  instance C (a,a) where ...  
  instance C (Int,a) where ...  
  instance C [[a]] where ...

`instance Iso a a` nie spełnia tych warunków, ale wiadomo o jaką relację nam chodzi :)

# Problem z klasami wieloparametrowymi
Spróbujmy stworzyć klasę kolekcji, np.

`BadCollection.hs`

~~~~ {.haskell}
class Collection c where
  insert :: e -> c -> c    
  member :: e -> c -> Bool

instance Collection [a] where
     insert = (:)
     member = elem  
~~~~

~~~~
    Couldn't match type `e' with `a'
      `e' is a rigid type variable bound by
          the type signature for member :: e -> [a] -> Bool
          at BadCollection.hs:7:6
      `a' is a rigid type variable bound by
          the instance declaration
          at BadCollection.hs:5:22
~~~~

Dlaczego?

# Problem z klasami wieloparametrowymi

~~~~ {.haskell}
class Collection c where
 insert :: e -> c -> c    
 member :: e -> c -> Bool
~~~~

tłumaczy się (mniej więcej) do

~~~~
data ColDic c = CD 
 { 
 , insert :: forall e.e -> c -> c
 , member :: forall e.e -> c -> Bool
 }
~~~~

 ... nie o to nam chodziło.
   
~~~~ {.haskell}
instance Collection [a] where
   insert = (:)
   member = undefined
~~~~

~~~~
-- (:) :: forall t. t -> [t] -> [t]
ColList :: forall a. ColDic a
ColList = \@ a -> CD { insert = (:) @ a, member = 
~~~~

# Problem z klasami wieloparametrowymi

`BadCollection2.hs`

~~~~ {.haskell}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

class Collection c e where
  insert :: e -> c -> c
  member :: e -> c -> Bool

instance Eq a => Collection [a] a where
  insert  = (:)
  member = elem
     
ins2 x y c = insert y (insert x c)
-- ins2 :: (Collection c e, Collection c e1) => e1 -> e -> c -> c

problem1 :: [Int]
problem1 = ins2 1 2 []
-- No instances for (Collection [Int] e0, Collection [Int] e1)
-- arising from a use of `ins2'

problem2 = ins2 'a' 'b' []
-- No instance for (Collection [a0] Char)
--       arising from a use of `ins2'

problem3 :: (Collection c0 Char, Collection c0 Bool) => c0 -> c0
problem3 = ins2 True 'a'
-- Tu problem akurat polega na tym, że to jest poprawne typowo
-- ...a chyba nie powinno być
~~~~

# Zależności funkcyjne
Czasami w klasach wieloparametrowych, jeden parametr wyznacza inny, np.

~~~~ {.haskell}
 class (Monad m) => MonadState s m | m -> s where ...

 class Collects e ce | ce -> e where
      empty  :: ce
      insert :: e -> ce -> ce
      member :: e -> ce -> Bool
~~~~

Problem: *Fundeps are very, very tricky.* - SPJ

Więcej: http://research.microsoft.com/en-us/um/people/simonpj/papers/fd-chr/

# Refleksja - czemu nie klasy konstruktorowe?

Problem kolekcji możemy rozwiązać np. tak:

~~~~ {.haskell}
class Collection c where
  insert :: e -> c e -> c e
  member :: Eq e => e -> c e-> Bool

instance Collection [] where
     insert x xs = x:xs
     member = elem
~~~~

ale nie rozwiązuje to problemu np. z monadą stanu:

~~~~ {.haskell}
 class (Monad m) => MonadState s m | m -> s where 
   get :: m s
   put :: s -> m ()
~~~~

typ stanu nie jest tu parametrem konstruktora m.

# Fundeps are very very tricky

~~~~ {.haskell}
class Mul a b c | a b -> c where
  (*) :: a -> b -> c
  
newtype Vec a = Vec [a]
instance Functor Vec where
  fmap f (Vec as) = Vec $ map f as
  
instance Mul a b c => Mul a (Vec b) (Vec c) where
  a * b = fmap (a*) b
  
f b x y = if b then  x * (Vec [y]) else y
~~~~

Jakiego typu jest f? Niech x::a, y::b. 

Wtedy typem wyniku jest b i musimy mieć instancję `Mul a (Vec b) b`

Z kolei `a b -> c` implikuje, że `b = Vec c` dla pewnego c, czyli szukamy instancji

~~~~
Mul a (Vec (Vec c)) (Vec c)
~~~~

zastosowanie reguły `Mul a b c => Mul a (Vec b) (Vec c)` doprowadzi nas do `Mul a (Vec c) c`.

...i tak w kółko.


# Spróbujmy

~~~~ {.haskell}
Mul1.hs:16:21:
    Context reduction stack overflow; size = 21
    Use -fcontext-stack=N to increase stack size to N
      co :: c18 ~ Vec c19
      $dMul :: Mul a0 c17 c18
      $dMul :: Mul a0 c16 c17
      ...
      $dMul :: Mul a0 c1 c2
      $dMul :: Mul a0 c c1
      $dMul :: Mul a0 c0 c
      $dMul :: Mul a0 (Vec c0) c0
    When using functional dependencies to combine
      Mul a (Vec b) (Vec c),
        arising from the dependency `a b -> c'
        in the instance declaration at 3/Mul1.hs:13:10
      Mul a0 (Vec c18) c18,
        arising from a use of `mul' at 3/Mul1.hs:16:21-23
    In the expression: mul x (Vec [y])
    In the expression: if b then mul x (Vec [y]) else y
~~~~

(musimy użyć UndecidableInstances, żeby GHC w ogóle spróbowało - ten przykład pokazuje co jest 'Undecidable').

# Rodziny typów

Rodziny to funkcje na typach - jak na pierwszym wykładzie, ale bardziej systematycznie.

~~~~ {.haskell}
{-# TypeFamilies #-}

data Zero = Zero
data Suc n = Suc n

type family m :+ n
type instance Zero :+ n = n
type instance (Suc m) :+ n = Suc(m:+n)

data Vec :: * -> * -> * where
  VNil :: Vec Zero a  
  (:>) :: a -> Vec n a -> Vec (Suc n) a

vhead :: Vec (Suc n) a -> a
vappend :: Vec m a -> Vec n a -> Vec (m:+n) a
~~~~


# Typy skojarzone

W Prelude mamy

~~~~
  (+) :: Num a -> a -> a
~~~~
  
Chcemy dodawać liczby różnych typów, np Int i Float

~~~~
  instance GNum Int Float where
    x + y = plusFloat (int2Float x) y  

  class GNum a b where
    (+) :: a -> b -> ?
~~~~

Musimy określić typ sumy elementu a i elementu b

~~~~ {.haskell}
> class GNum a b where
>   type SumTy a b :: *
>   (+) :: a -> b -> SumTy a b

> instance GNum Int Float where
>   type SumTy Int Float = Float
>   x + y = plusFloat (int2Float x) y  

> instance GNum Int Int where
>    type SumTy Int Int = Int
>    x + y = plusInt x y

*Main> (1::Int) + (1::Int)
2
*Main> (1::Int) + (1::Float)
2.0
~~~~

# Typy skojarzone zamiast zależności

~~~~ {.haskell}
class Collection c where
  type Elem c
  empty :: c
  insert :: Elem c -> c -> c
  member :: Elem c -> c -> Bool

instance Eq a => Collection [a] where
  type Elem [a] = a
  empty = []
  insert  = (:)
  member = elem

-- ins2 :: Collection c => Elem c -> Elem c -> c -> c
ins2 x y c = insert y (insert x c)

-- tu sygnatura niezbędna
noproblem :: [Char]
noproblem = ins2 'a' 'b' empty
-- ogólny typ:
-- noproblem :: (Collection c, Elem c ~ Char) => c

~~~~

# Generalised Algebraic Data Types (GADT)

Rozważmy typowy typ algebraiczny:

~~~~ {.haskell}
data Foo a = Bar | Baz a (Foo a) 
~~~~

alternatywnie można go zapisać wypisując typy konstruktorów:

~~~~ {.haskell}
{-# LANGUAGE GADTs #-}
data Foo a where
  Bar :: Foo a
  Baz :: a -> Foo a -> Foo a
~~~~

~~~~
*Main> :t Baz () Bar
Baz () Bar :: Foo ()
*Main> :t Mar () Mud
Mar () Mud :: Moo Int
~~~~

# Generalised Algebraic Data Types (GADT)
Trochę ciekawszy przykład:

~~~~ {.haskell}
data Exp = ALitInt Int | ALitStr String | APlus Exp Exp deriving(Eq,Show)
data Expr a where
  ELitInt :: Int -> Expr Int
  ELitStr :: String -> Expr String
  EPlus :: Expr a -> Expr a -> Expr a

{-# LANGUAGE StandaloneDeriving #-}  
deriving instance Show a => Show(Expr a)  

interpret :: HasPlus a => Expr a -> a
interpret (ELitInt i) = i
interpret (ELitStr s) = s
interpret (EPlus e1 e2) = plus (interpret e1) (interpret e2)
~~~~

~~~~
*Gadt> interpret $ EPlus (ELitInt 40) (ELitInt 2)
42
*Gadt> :t APlus (ALitStr "foo") (ALitInt 42)
APlus (ALitStr "foo") (ALitInt 42) :: Exp
*Gadt> :t EPlus (ELitStr "foo") (ELitInt (42::Int))
    Couldn't match expected type `String' with actual type `Int'...
~~~~

**Ćwiczenie:** napisz ciekawszy interpreter dla wersji Exp i Expr


# Finis coronat opus.

~~~~ {.haskell}

~~~~


