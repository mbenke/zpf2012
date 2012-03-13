module Equality where
open import Core
open import Rel2

infix 4 _≡_ _≢_

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

-- Nonequality.

_≢_ : ∀ {A : Set} → A → A → Set
x ≢ y = ¬ x ≡ y

reflexive : ∀ {A : Set} → Reflexive {A} _≡_
reflexive x = refl

substitutive : ∀ {A : Set} → Substitutive {A} _≡_
substitutive P y .y refl py = py

sym : ∀ {A} → Symmetric {A} _≡_
sym = subst-sym _≡_ reflexive substitutive

mapId : ∀ {A  B : Set} → (f : A → B) → (x y : A) → x ≡ y → f x ≡ f y
mapId f .y y refl = refl