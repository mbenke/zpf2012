> {-# LANGUAGE TypeFamilies, GADTs, EmptyDataDecls, RankNTypes #-}
> module Nat where

Liczby naturalne Peano:

> data Zero = Zero
> data Suc n = Suc n

> class Card n => ElimNat n where
>   elimNat :: (Zero -> c) -> ((m -> c) -> (Suc m) -> c) -> n -> c

> instance ElimNat Zero where
>   elimNat dz ds z = dz z

> instance ElimNat n => ElimNat (Suc n) where
>   elimNat dz {- :: Zero -> b -} 
>           ds {- :: (c -> b) -> (Suc c) -> b -}
>           x  {- :: n -}
>           = elimNat dz ds (cpred x)

> class Card c where
>   c2num :: Num a => c -> a
>   cpred :: (Suc c) -> c
>   cpred = undefined

> instance Card Zero where
>   c2num  _ = 0
> instance Card c => Card (Suc c) where
>   c2num c = 1 + c2num (cpred c)
