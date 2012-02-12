# Plan wykładu
* Typy i klasy (ok. 3 wykładów)
    * Typy algebraiczne i klasy typów
    * Klasy konstruktorowe
    * Klasy wieloparametrowe, zależności funkcyjne
    * Rodziny typów, typy skojarzone, uogólnione typy algebraiczne (GADT)
* Metody kontrolowania efektów w języku funkcyjnym (ok. 6 wykładów)
    * Rodzaje efektów (błędy, stan, I/O, nawracanie)
    * Monady Error, State, IO, List
    * Studium biblioteki monadycznej
    * Funktory aplikatywne i idiomy
    * Studium biblioteki idiomatycznej
    * Strzałki
* Programowanie równoległe w Haskellu
    * Programowanie wielordzeniowe i wieloprocesorowe (SMP)
    * Równoległość danych (Data Parallel Haskell)
* Prezentacje projektów

# Języki funkcyjne
* typowane dynamicznie, gorliwe: Lisp
* typowane statycznie, gorliwe, nieczyste: ML
* typowane statycznie, leniwe, czyste: Haskell

Types are a design language - SPJ

# Typy w Haskellu

* typy bazowe: `zeroInt :: Int`
* typy funkcyjne: `plusInt :: Int -> Int -> Int`
* typy polimorficzne `id :: a -> a`

    ~~~~ {.haskell}
    {-# LANGUAGE ExplicitForAll #-}
    g :: forall b.b -> b
    ~~~~

* typy algebraiczne 

    ~~~~ {.haskell}
    data Tree a = Leaf | Node a (Tree a) (Tree a)
    ~~~~

* NB od niedawna Haskell dopuszcza puste typy:

    ~~~~ {.haskell}
    data Zero
    ~~~~
  
* `Tree` jest *konstruktorem typowym*, czyli operacją na typach

# Operacje na typach

* Konstruktory typowe transformują typy

* `Tree` może zamienić np. `Int` w drzewo

# Klasy

* klasy opisują własności typów

    ~~~~ {.haskell}
    class Eq a where
      (==) :: a -> a -> Bool
    instance Eq Bool where
       True  == True  = True
       False == False = True
       _     == _     = False
    ~~~~

# Typy jako język specyfikacji

Typ funkcji często specyfikuje nie tylko jej wejście i wyjście ale i relacje między nimi:

~~~~ {.haskell}
f :: forall a. a -> a
f x = ?
~~~~

Jeśli `(f x)` daje wynik, to musi nim być `x`

* Philip Wadler "Theorems for Free"

* Funkcja typu `a -> IO b` może mieć efekty uboczne

    ~~~~ {.haskell}
    import Data.IORef

    f :: Int -> IO (IORef Int)
    f i = do
      print i
      r <- newIORef i
      return r

    main = do
      r <- f 42
      j <- readIORef r
      print j    
    ~~~~



# Typy jako język specyfikacji (2)

Funkcja typu `Integer -> Integer` zasadniczo nie może mieć efektów ubocznych

Liczby Fibonacciego w stałej pamięci

~~~~ {.haskell}
import Control.Monad.ST
import Data.STRef
fibST :: Integer -> Integer
fibST n = 
    if n < 2 then n else runST fib2 where
      fib2 =  do
        x <- newSTRef 0
        y <- newSTRef 1
        fib3 n x y
 
      fib3 0 x _ = readSTRef x
      fib3 n x y = do
              x' <- readSTRef x
              y' <- readSTRef y
              writeSTRef x y'
              writeSTRef y (x'+y')
              fib3 (n-1) x y
~~~~

Jak to?

~~~~
runST :: (forall s. ST s a) -> a
~~~~

Typ `runST` gwarantuje, że efekty uboczne nie wyciekają. Funkcja `fibST`
jest czysta.

# Typy jako język projektowania

* Projektowanie programu przy użyciu typów i `undefined`

    ~~~~ {.haskell}
    conquer :: [Foo] -> [Bar]
    conquer fs = concatMap step fs

    step :: Foo -> [Bar]
    step = undefined
    ~~~~

# Typy jako język programowania

*    Funkcje na typach obliczane w czasie kompilacji

    ~~~~ {.haskell}
    data Zero
    data Succ n

    type One   = Succ Zero
    type Two   = Succ One
    type Three = Succ Two
    type Four  = Succ Three

    one   = undefined :: One
    two   = undefined :: Two
    three = undefined :: Three
    four  = undefined :: Four

    class Add a b c | a b -> c where
      add :: a -> b -> c
      add = undefined
    instance              Add  Zero    b  b
    instance Add a b c => Add (Succ a) b (Succ c)
    ~~~~ 

    ~~~~
    *Main> :t add three one
    add three one :: Succ (Succ (Succ (Succ Zero)))
    ~~~~

* Ćwiczenie: rozszerzyć o mnożenie i silnię

# Typy jako język programowania (2)
Wektory przy użyciu klas:

~~~~ {.haskell}
data Vec :: * -> * -> * where
  VNil :: Vec Zero a  
  (:>) :: a -> Vec n a -> Vec (Succ n) a

vhead :: Vec (Succ n) a -> a
vhead (x :> xs) = x
~~~~

**Ćwiczenie:** dopisać `vtail`, `vlast`

Chcielibyśmy również mieć

~~~~ {.haskell}
vappend :: Add m n s => Vec m a -> Vec n a -> Vec s a
~~~~

ale tu niestety system typów okazuje się za słaby

# Typy jako język programowania (3)

* Wektory przy użyciu rodzin typów:

    ~~~~ {.haskell}
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

* Sprytna sztuczka o wątpliwej wartości praktycznej

# Podklasy

* Jeśli typ a jest klasy C i są zdefiniowane funkcje ... to jest klasy D

    ~~~~ {.haskell}
    class Eq a => Ord a where
      compare :: a -> a -> Ordering
      (<) :: a -> a -> Bool
      ...
    ~~~~

* Graf podklas musi być acykliczny

* Specyfikowanie nadklas jest kwestią smaku - można dyskutować, 
czy `Eq` rzeczywiście jest niezbędne dla `Ord`, albo czy każda instancja `Monad` musi być instancją `Functor`.

# Klasy konstruktorowe

* klasy konstruktorowe opisują własności konstruktorów typów

    ~~~~ {.haskell}
    class Functor f where
      fmap :: (a->b) -> f a -> f b
    ~~~~


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

# Zależności funkcyjne
Czasami w klasach wieloparametrowych, jeden parametr wyznacza inny, np.

~~~~ {.haskell}
 class (Monad m) => MonadState s m | m -> s where ...

 class Collects e ce | ce -> e where
      empty  :: ce
      insert :: e -> ce -> ce
      member :: e -> ce -> Bool
~~~~

# Rodziny typów

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

# Koniec

Finis coronat opus.

    ~~~~ {.haskell}

    ~~~~

~~~~ {.haskell}

~~~~
