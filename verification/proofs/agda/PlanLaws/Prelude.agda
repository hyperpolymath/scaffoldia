{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- PlanLaws.Prelude: the small logic the plan laws are stated in.
--
-- Proves nothing about plans. It defines propositional equality, the empty
-- and unit types, decidability, Maybe, lists with membership, Σ, products,
-- sums and logical equivalence, with the elementary lemmas the other modules
-- use. It imports no library (not the Agda standard library), so the import
-- cone of every PlanLaws module is this directory plus Agda.Primitive, which
-- Agda loads for every module.

module PlanLaws.Prelude where

infix  4 _≡_ _∈_
infix  0 _⇔_
infixr 5 _∷_ _++_
infixr 4 _,_
infixr 2 _×_
infixr 1 _⊎_
infix  3 ¬_

------------------------------------------------------------------------
-- Equality

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

trans : {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl e = e

cong : {A B : Set} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

cong₂ : {A B C : Set} (f : A → B → C) {x y : A} {u v : B} →
        x ≡ y → u ≡ v → f x u ≡ f y v
cong₂ f refl refl = refl

subst : {A : Set} (P : A → Set) {x y : A} → x ≡ y → P x → P y
subst P refl px = px

------------------------------------------------------------------------
-- Empty, unit, negation, decidability

data ⊥ : Set where

⊥-elim : {A : Set} → ⊥ → A
⊥-elim ()

¬_ : Set → Set
¬ A = A → ⊥

record ⊤ : Set where
  constructor tt

data Dec (A : Set) : Set where
  yes : A → Dec A
  no  : ¬ A → Dec A

------------------------------------------------------------------------
-- Σ, products, sums, logical equivalence

record Σ (A : Set) (B : A → Set) : Set where
  constructor _,_
  field
    proj₁ : A
    proj₂ : B proj₁
open Σ public

_×_ : Set → Set → Set
A × B = Σ A (λ _ → B)

data _⊎_ (A B : Set) : Set where
  inj₁ : A → A ⊎ B
  inj₂ : B → A ⊎ B

record _⇔_ (A B : Set) : Set where
  constructor mk⇔
  field
    to   : A → B
    from : B → A
open _⇔_ public

------------------------------------------------------------------------
-- Maybe

data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A

-- The value under just, or the default d. just-injective goes through it
-- rather than matching on refl, which --cubical-compatible warns about.
from-just : {A : Set} → A → Maybe A → A
from-just d nothing  = d
from-just d (just a) = a

just-injective : {A : Set} {a b : A} → just a ≡ just b → a ≡ b
just-injective {a = a} e = cong (from-just a) e

nothing≢just : {A : Set} {a : A} → ¬ (nothing ≡ just a)
nothing≢just ()

-- Every Maybe value is one of the two shapes, with the equation recorded.
maybe-cases : {A : Set} (m : Maybe A) → Σ A (λ a → m ≡ just a) ⊎ (m ≡ nothing)
maybe-cases nothing  = inj₂ refl
maybe-cases (just a) = inj₁ (a , refl)

-- Left-biased choice: the first value present.
_<|>_ : {A : Set} → Maybe A → Maybe A → Maybe A
nothing <|> n = n
just a  <|> n = just a

------------------------------------------------------------------------
-- Lists and membership

data List (A : Set) : Set where
  []  : List A
  _∷_ : A → List A → List A

_++_ : {A : Set} → List A → List A → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

map : {A B : Set} → (A → B) → List A → List B
map f []       = []
map f (x ∷ xs) = f x ∷ map f xs

++-assoc : {A : Set} (xs ys zs : List A) → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
++-assoc []       ys zs = refl
++-assoc (x ∷ xs) ys zs = cong (x ∷_) (++-assoc xs ys zs)

map-++ : {A B : Set} (f : A → B) (xs ys : List A) →
         map f (xs ++ ys) ≡ map f xs ++ map f ys
map-++ f []       ys = refl
map-++ f (x ∷ xs) ys = cong (f x ∷_) (map-++ f xs ys)

-- Membership, by recursion on the list (not an indexed family, so no
-- pattern match below needs injectivity of _∷_, which --cubical-compatible
-- warns about).
_∈_ : {A : Set} → A → List A → Set
x ∈ []       = ⊥
x ∈ (y ∷ ys) = (x ≡ y) ⊎ (x ∈ ys)

∈-++ˡ : {A : Set} {x : A} (xs ys : List A) → x ∈ xs → x ∈ (xs ++ ys)
∈-++ˡ []       ys ()
∈-++ˡ (y ∷ xs) ys (inj₁ e) = inj₁ e
∈-++ˡ (y ∷ xs) ys (inj₂ m) = inj₂ (∈-++ˡ xs ys m)

∈-++ʳ : {A : Set} {x : A} (xs ys : List A) → x ∈ ys → x ∈ (xs ++ ys)
∈-++ʳ []       ys m = m
∈-++ʳ (y ∷ xs) ys m = inj₂ (∈-++ʳ xs ys m)

∈-++⁻ : {A : Set} {x : A} (xs ys : List A) →
        x ∈ (xs ++ ys) → (x ∈ xs) ⊎ (x ∈ ys)
∈-++⁻ []       ys m        = inj₂ m
∈-++⁻ (y ∷ xs) ys (inj₁ e) = inj₁ (inj₁ e)
∈-++⁻ (y ∷ xs) ys (inj₂ m) with ∈-++⁻ xs ys m
... | inj₁ m′ = inj₁ (inj₂ m′)
... | inj₂ m′ = inj₂ m′
