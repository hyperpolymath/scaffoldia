{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- PlanLaws.Example: a concrete instance, so that no law is vacuous.
--
-- Three paths (README.adoc, readme.adoc, LICENSE) and two contents. Shows,
-- by evaluation (refl), that the join computes: two packs writing one path
-- differently give a conflict naming that path, two packs writing it the
-- same way join, and the join is order-independent. Then instantiates
-- PlanLaws.Safety with a case-fold clash relation and obtains the
-- counterexample: README.adoc and readme.adoc are each a safe one-entry
-- plan, they are compatible, and their join is not plan-safe.

module PlanLaws.Example where

open import PlanLaws.Prelude

data P : Set where
  README-adoc readme-adoc LICENSE : P

_≟P_ : (x y : P) → Dec (x ≡ y)
README-adoc ≟P README-adoc = yes refl
README-adoc ≟P readme-adoc = no (λ ())
README-adoc ≟P LICENSE     = no (λ ())
readme-adoc ≟P README-adoc = no (λ ())
readme-adoc ≟P readme-adoc = yes refl
readme-adoc ≟P LICENSE     = no (λ ())
LICENSE     ≟P README-adoc = no (λ ())
LICENSE     ≟P readme-adoc = no (λ ())
LICENSE     ≟P LICENSE     = yes refl

data C : Set where
  blob₁ blob₂ : C

_≟C_ : (c d : C) → Dec (c ≡ d)
blob₁ ≟C blob₁ = yes refl
blob₁ ≟C blob₂ = no (λ ())
blob₂ ≟C blob₁ = no (λ ())
blob₂ ≟C blob₂ = yes refl

open import PlanLaws.Plan P _≟P_ C _≟C_
open import PlanLaws.Join P _≟P_ C _≟C_

pack-A pack-B pack-B′ : Plan
pack-A  = (LICENSE , file blob₁) ∷ (README-adoc , file blob₁) ∷ []
pack-B  = (README-adoc , file blob₂) ∷ []
pack-B′ = (README-adoc , file blob₁) ∷ (readme-adoc , absent) ∷ []

-- Two packs write README.adoc differently: a conflict at README.adoc, in
-- either order. Never last-writer-wins.
conflict-AB : pack-A ⊔ pack-B ≡ conflict README-adoc
conflict-AB = refl

conflict-BA : pack-B ⊔ pack-A ≡ conflict README-adoc
conflict-BA = refl

-- Two packs write README.adoc the same way: they join.
joined-AB′ : pack-A ⊔ pack-B′ ≡ ok (pack-A ∪ pack-B′)
joined-AB′ = refl

------------------------------------------------------------------------
-- Plan-level safety: a case-fold clash

data Clash : P → P → Set where
  fold₁ : Clash README-adoc readme-adoc
  fold₂ : Clash readme-adoc README-adoc

clash-sym : {x y : P} → Clash x y → Clash y x
clash-sym fold₁ = fold₂
clash-sym fold₂ = fold₁

clash-irrefl : {x : P} → ¬ Clash x x
clash-irrefl ()

-- Every path of this example is individually safe.
Safe : P → Set
Safe _ = ⊤

open import PlanLaws.Safety P _≟P_ C _≟C_ Safe Clash clash-sym clash-irrefl

-- README.adoc in one pack and readme.adoc in another: each plan is safe,
-- they are compatible and join, and the joined plan is not.
case-fold-counterexample :
  PlanSafe ((README-adoc , file blob₁) ∷ []) ×
  PlanSafe ((readme-adoc , file blob₂) ∷ []) ×
  Σ Plan (λ r → ((README-adoc , file blob₁) ∷ []) ⊔ ((readme-adoc , file blob₂) ∷ []) ≡ ok r
              × ¬ PlanSafe r)
case-fold-counterexample =
  plan-safety-not-preserved README-adoc readme-adoc tt tt fold₁ (file blob₁) (file blob₂)
