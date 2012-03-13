module Core where

data ⊥ : Set where

⊥-elim : ∀ {Whatever : Set} → ⊥ → Whatever
⊥-elim ()

infix 3 ¬_
¬_ : Set -> Set
¬ A = A → ⊥
