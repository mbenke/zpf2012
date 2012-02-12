> {-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
> import Prelude hiding(Num,(+))
> import GHC.Base(plusInt, timesInt,negateInt) 
> import GHC.Float(plusFloat, int2Float)

W Prelude mamy

  (+) :: Num a -> a -> a
  
Chcemy dodawać liczby różnych typów, np Int i Float

  instance GNum Int Float where
    x + y = plusFloat (int2Float x) y  

  class GNum a b where
    (+) :: a -> b -> ?

Musimy określić typ sumy elementu a i elementu b

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

newtype Dollars = MkD Int
instance GNum Dollars Dollars where
...