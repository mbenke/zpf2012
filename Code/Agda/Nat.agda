module Nat where
open import Equality

data Nat : Set  where
  zero : Nat
  suc : Nat → Nat

{-# BUILTIN NATURAL Nat #-}
{-# BUILTIN ZERO zero #-}
{-# BUILTIN SUC suc #-}

infixl 6 _+_
_+_ : Nat → Nat → Nat
zero + y = y
suc x + y = suc (x + y)

thmPlusZero : ∀(n : Nat) → n + 0 ≡ n   -- ∀ n ∈ N . n + 0 = n
thmPlusZero zero = refl
thmPlusZero (suc y) = mapId suc (y + zero) y (thmPlusZero y)
