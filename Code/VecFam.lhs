> {-# LANGUAGE TypeFamilies, GADTs, EmptyDataDecls #-}
> {-# LANGUAGE KindSignatures, TypeOperators, FlexibleContexts #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE RankNTypes #-}

> module VecFam where

Liczby naturalne Peano:

> data N where
>   Z :: N
>   S :: N -> N

Liczby naturalne na poziomie typów pozwolą klasyfikować wektory według długości

> data Zero = Zero
> data Suc n = Suc n

Synonimy ustalają równości na typach, np.

    type Name = [Char]

Rodziny typów pozwalają definiować (ograniczone) funkcje na typach:

> type family m :+ n
> type instance Zero :+ n = n
> type instance (Suc m) :+ n = Suc(m:+n)

head na liście pustej poprawne typowo, daje błąd czasu wykonania:

head []
*** Exception: Prelude.head: empty list

Wektory z długością kontrolowaną typami:

> data Vec :: * -> * -> * where
>   VNil :: Vec Zero a  
>   (:>) :: a -> Vec n a -> Vec (Suc n) a

Bezpieczne head:

> vhead :: {- TN n => -}  Vec (Suc n) a -> a
> vhead (x:>_) = x

*Vec> vhead VNil

<interactive>:1:6:
    Couldn't match expected type `Suc n' against inferred type `Zero'
      Expected type: Vec (Suc n) a
      Inferred type: Vec Zero a1
    In the first argument of `vhead', namely `VNil'
    In the expression: vhead VNil
                 
Póba wzięcia głowy pustego wektora jest niepoprawna typowo.

> vtail :: {- (TN n) => -} Vec (Suc n) a -> Vec n a
> vtail (_:>xs) = xs

Możemy  zdefiniować append z kontrolowaną długościa

> vappend :: Vec m a -> Vec n a -> Vec (m:+n) a
> vappend VNil ys = ys
> vappend (x :> xs) ys = x :> (vappend xs ys)

Kontrolowanie długości wektorów w ten sposób nie jest praktyczne, 
ale pokazuje możliwości.