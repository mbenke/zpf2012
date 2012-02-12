> {-# LANGUAGE TypeFamilies, GADTs, EmptyDataDecls #-}
> {-# LANGUAGE KindSignatures, TypeOperators, FlexibleContexts #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
> module ElimNat where

Liczby naturalne Peano:

> data N where
>   Z :: N
>   S :: N -> N

> class TN a where
>   toN :: a -> N

Liczby naturalne na poziomie typów pozwolą klasyfikować wektory według długości

> data Zero = Zero
> instance TN Zero where
>   toN Zero = Z

> data Suc n = Suc n
> -- instance TN n => TN (Suc n) where
> -- toN (Suc n) = S(toN n)


> class IterNat n where
>   iterNat :: c -> (c -> c) -> n -> c

> instance IterNat Zero where
>   iterNat cz s Zero = cz
>
> instance (IterNat m) => IterNat (Suc m) where
>   iterNat z s n@(Suc m)= s (iterNat z s m)

> class Nat n c where
>   elimNat :: (c Zero) -> (forall m.(c m) -> c (Suc m)) -> n -> c n

> instance Nat Zero c where
>   elimNat cz cs z@Zero = cz

> instance (Nat m c) => Nat (Suc m) c where
>   elimNat cz cs n@(Suc m) = cs (elimNat cz cs m)

> class Card c where
>   c2num :: Num a => c -> a
>   cpred :: (Suc c) -> c
>   cpred = undefined

> {-
> instance Card Zero where
>   c2num  _ = 0
> instance Card c => Card (Suc c) where
>   c2num c = 1 + c2num (cpred c)
> -}

Albo
 instance IterNat n => Card n where
   c2num n = iterNat 0 (1+) n
