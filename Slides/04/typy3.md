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

Rodziny to funkcje na typach - jak na pierwszym wykładzie

~~~~ {.haskell}
{-# TypeFamilies #-}

data Zero = Zero
data Suc n = Suc n

type family m :+ n
type instance Zero :+ n = n
type instance (Suc m) :+ n = Suc(m:+n)

vhead :: Vec (Suc n) a -> a
vappend :: Vec m a -> Vec n a -> Vec (m:+n) a
~~~~

Trochę dalej powiemy sobie o nich bardziej systematycznie.

# Rodziny typów

Inny, może ciekawszy przykład

~~~~ {.haskell}
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

import Data.Array

type family ListOfIndex i a
type instance ListOfIndex () a = a
type instance ListOfIndex (Int, i) a = [ListOfIndex i a]

class Ix i => ArrConv i where
  acArgs :: ListOfIndex i a -> ((i, i), [a])

instance ArrConv () where
  acArgs x = (((), ()), [x])

instance ArrConv i => ArrConv (Int, i) where
  acArgs lst =
    (((0, inStart), (length lst - 1, inEnd)), args >>= snd)
    where
      args = map acArgs lst
      (inStart, inEnd) = fst (head args)

arrFromNestedLists :: ArrConv i => ListOfIndex i a -> Array i a
arrFromNestedLists = uncurry listArray . acArgs
~~~~

Źródło:

<http://stackoverflow.com/questions/2043610/haskell-type-families-and-dummy-arguments>

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
~~~~

# Generalised Algebraic Data Types (GADT)
W uogólnionych typach algebraicznych (GADT), końcowe typy konstruktorów mogą być indeksowane różnymi typami:

~~~~ {.haskell}
{-# LANGUAGE GADTs #-}
data Moo a where
  Mud :: Moo ()
  Mar :: a -> Moo a -> Moo Int
~~~~

~~~~
*Main> :t Mar () Mud
Mar () Mud :: Moo Int
~~~~

Widzieliśmy już przykład przy okazji wektorów:

~~~~ {.haskell}
data Vec :: * -> * -> * where
  VNil :: Vec Zero a  
  (:>) :: a -> Vec n a -> Vec (Suc n) a
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

# Systemy typów

Zacznijmy od reguł podstawowych:

* Zmienna

$$\Gamma(x:t) \vdash x:t $$

* Abstrakcja

$${\Gamma(x:t)\vdash e:u}\over {\Gamma\vdash \lambda (x:t).e:t \to u} $$

[NB w Haskellu pomijamy typ zmiennej pod lambdą, kompilator znajduje właściwe t.]

* Aplikacja

$${\Gamma \vdash e_1 : t \to u\quad \Gamma \vdash e_2 : t}\over {\Gamma \vdash e_1 e_2 : u} $$

# Typowanie polimorficzne

* Generalizacja:

$${\Gamma \vdash e : t, a \notin FV( \Gamma )}\over {\Gamma \vdash \Lambda a.e : \forall a.t}$$

* Instancjacja

$$ {\Gamma \vdash e : \forall a.t}\over {\Gamma \vdash e(s) : t[a:=s]} $$

W Haskellu pomijamy abstrakcję i aplikację typową, nie piszemy też przeważnie jawnie kwantyfikatora. 

Natomiast w Haskell Core wszystkie te elementy występują, np

~~~~
  neq :: %forall a . (DicE a) -> a -> a -> B =
    \ @ a0  -- a0 jest typem (tzw "duża lambda", Λa.M)
      (dicE::(DicE a0))
      (x::a0)
      (y::a0) -> ... (CoDE a0) ...
~~~~

# Konstruktory typów

Aby mówić o operacjach na typach potrzebujemy wyrazić pewne fakty o nich; służą do tego rodzaje, np.

$$ {n : * \quad a : *}\over{\mathrm{Vec}\;n\;a:*} $$

w ogólności

$$ {\Gamma\vdash C : k_1 \to k_2 \quad \Gamma\vdash a : k_1}\over{\Gamma\vdash C a : k_2} $$ 

...czyli analogicznie jak "zwykła" aplikacja!

w Haskellu zasadniczo nie piszemy jawnie rodzajów; w GHC mozna to robić (rozszerzenie KindSignatures)

# Konstruktory typów (2)

Jeżeli dopuścimy tworzenie dowolnych funkcji na typach, dostaniemy system Fω:

* Rodzaj typów (*) jest rodzajem:

$$ * : \Box $$

* Możemy tworzyć funkcje na rodzajach:

$$ {k_1,k_2 : \Box}\over {k_1 \to k_2 : \Box} $$

* Abstrakcja rodzajowa:

$${\Gamma(a:k_1)\vdash C : k_2 \quad k_1\to k_2:\Box}\over {\Gamma\vdash \lambda a.C:k_1 \to k_2} $$

czyli znowu podobnie jak "zwykła" abstrakcja. Czy moiglibyśmy to ujednolicić?

# Systemy typów (2)

Wróćmy reguł podstawowych:

* Zmienna

$${\Gamma\vdash t:k\quad k\in\{*,\Box\}}\over{\Gamma(x:t) \vdash x:t} $$

* Abstrakcja

$${\Gamma(x:t)\vdash e:u}\over {\Gamma\vdash \lambda (x:t).e:t \to u} $$

jeżeli u może zależeć od x, możemy uogólnić funkcje do produktów i  powyższą regułę ogólniej zapisać jako

$${\Gamma(x:t)\vdash e:u}\over {\Gamma\vdash \lambda (x:t).e: (x:t) \to u} $$

albo 

$${\Gamma(x:t)\vdash e:u}\over {\Gamma\vdash \lambda (x:t).e:\Pi (x:t). u} $$

# Abstrakcja

<!-- Zauważmy, że mamy trzy rodzaje abstrakcji: -->

* termowa 

$${\Gamma(x:t)\vdash e:u}\over {\Gamma\vdash \lambda (x:t).e:t \to u} $$

* typowa

$${\Gamma \vdash e : t, a \notin FV( \Gamma )}\over {\Gamma \vdash \Lambda a.e : \forall a.t}$$

   możemy ją zapisać jako

$${\Gamma(a:*) \vdash e : t}\over 
{\Gamma \vdash \lambda (a:*).e : (a:*) \to t}$$

* rodzajowa

Wszystkie są szczególnymi przypadkami ogólnej reguły:

$${ \Gamma(x:t)\vdash e:u\quad \Gamma\vdash (x:t) \to u:k\quad k\in\{*,\Box\}} 
\over {\Gamma\vdash \lambda (x:t).e: (x:t) \to u} $$

# Aplikacja

* termowa 
$${\Gamma \vdash e_1 : t \to u\quad \Gamma \vdash e_2 : t}\over {\Gamma \vdash e_1 e_2 : u} $$

* typowa

$$ {\Gamma \vdash e : \forall a.u}\over {\Gamma \vdash e(s) : u[a:=s]} $$
czyli inaczej
$$ {\Gamma \vdash e : (a:*)\to u\quad \Gamma\vdash s:*}\over {\Gamma \vdash e(s) : u[a:=s]} $$

w ogólności
$$ {\Gamma \vdash e : (a:t)\to u\quad \Gamma\vdash t:k\quad k\in\{*,\Box\}}\over {\Gamma \vdash e(s) : u[a:=s]} $$

# Dozwolone Produkty
Musimy jeszcze ustalić dla jakich rodzajów (k1,k2) dozwolone są produkty:

$$ {\Gamma \vdash t : k_1\quad\Gamma(x:t)\vdash u : k_2 }
\over {\Gamma\vdash (x:t)\to u : k_2}$$

* (\*,\*) : funkcje termowe
$$ {\Gamma \vdash t : *\quad\Gamma(x:t)\vdash u : * }
\over {\Gamma\vdash (x:t)\to u : *}$$

* (☐,\*) : forall
$$ {\Gamma \vdash k :\Box \quad\Gamma(x:k)\vdash u : * }
\over {\Gamma\vdash (x:*)\to u : *}$$

NB k może być *, albo innym rodzajem, np.

~~~~ {.haskell}
{-# LANGUAGE ExplicitForAll, KindSignatures #-}
f :: forall (m :: * -> *) . (Monad m) => forall (a :: *) . m a -> m a
f = ...
~~~~

# Dozwolone Produkty(2)
Musimy jeszcze ustalić dla jakich rodzajów (k1,k2) dozwolone są produkty:

$$ {\Gamma \vdash t : k_1\quad\Gamma(x:t)\vdash u : k_2 }
\over {\Gamma\vdash (x:t)\to u : k_2}$$

* (☐,☐) : rodzaje wyższych rzędów, operatory na typach

$$ {\vdash t : \Box\quad\vdash u : \Box }
\over {\Gamma\vdash t \to u : \Box}$$

$$ * : \Box \quad\mbox{(aksjomat)}$$

Czyli mamy (\*,\*),(☐,☐),(☐,\*)  a co z (\*,☐) ?

Nie ma w Haskellu.

# (\*,☐) czyli typy zależne

$$ {\Gamma \vdash t : *\quad\Gamma(x:t)\vdash u : \Box }
\over {\Gamma\vdash (x:t)\to u : \Box}$$

co to w ogóle znaczy? Przykład (N - liczby naturalne):

$$ {\Gamma \vdash N : *\quad\Gamma(n:N)\vdash * \to * : \Box }
\over {\Gamma\vdash (n:N)\to * \to * : \Box}$$

$$ {\Gamma\vdash \mathrm{Vec} : (n:N)\to * \to *\quad \Gamma\vdash n : N\quad \Gamma\vdash a:*}
\over {\mathrm{Vec}\; n\; a : *}$$
...czyli `Vec n a` jest typem.

# Finis coronat opus.

~~~~ {.haskell}

~~~~

$${}\over {} $$