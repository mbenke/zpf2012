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



