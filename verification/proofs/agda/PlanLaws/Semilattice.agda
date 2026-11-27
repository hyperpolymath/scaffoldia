{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- PlanLaws.Semilattice: the total form of law 1, in which "pack order
-- cannot matter" needs no definedness side condition.
--
-- Outcome, under ~, is the quotient Plan/≈ with one adjoined element for
-- "conflict" (all conflicts are identified). The lifted join _⊔ᴼ_
-- (PlanLaws.Join) is total on it. Proves that it is a join-semilattice
-- operation with a unit and an absorbing top:
--   ⊔ᴼ-cong   it respects ~ in both arguments (so it is an operation on the
--             quotient);
--   ⊔ᴼ-comm   o ⊔ᴼ o′ ~ o′ ⊔ᴼ o;
--   ⊔ᴼ-assoc  (o ⊔ᴼ o′) ⊔ᴼ o″ ~ o ⊔ᴼ (o′ ⊔ᴼ o″);
--   ⊔ᴼ-idem   o ⊔ᴼ o ~ o;
--   ⊔ᴼ-unit   the empty plan is a unit: ok [] ⊔ᴼ o ~ o;
--   ⊔ᴼ-top    a conflict absorbs: conflict x ⊔ᴼ o ~ conflict x, and
--             o ⊔ᴼ conflict x ~ conflict x.
-- Consequently any bracketing and any order of joining a finite collection
-- of packs gives ~-equal outcomes: the same plan up to ≈, or a conflict.

open import PlanLaws.Prelude

module PlanLaws.Semilattice
  (Path    : Set) (_≟P_ : (x y : Path)    → Dec (x ≡ y))
  (Content : Set) (_≟C_ : (c d : Content) → Dec (c ≡ d))
  where

open import PlanLaws.Plan Path _≟P_ Content _≟C_
open import PlanLaws.Join Path _≟P_ Content _≟C_

⊔ᴼ-topˡ : (x : Path) (o : Outcome) → conflict x ⊔ᴼ o ~ conflict x
⊔ᴼ-topˡ x o = conflict~

⊔ᴼ-topʳ : (o : Outcome) (x : Path) → o ⊔ᴼ conflict x ~ conflict x
⊔ᴼ-topʳ (conflict y) x = conflict~
⊔ᴼ-topʳ (ok r)       x = conflict~

⊔ᴼ-cong : {o₁ o₁′ o₂ o₂′ : Outcome} → o₁ ~ o₁′ → o₂ ~ o₂′ → o₁ ⊔ᴼ o₂ ~ o₁′ ⊔ᴼ o₂′
⊔ᴼ-cong {conflict x} {conflict x′} _        _ = conflict~
⊔ᴼ-cong {conflict x} {ok r′}       (~-by ()) _
⊔ᴼ-cong {ok r}       {conflict x′} (~-by ()) _
⊔ᴼ-cong {ok r} {ok r′} {conflict y} {conflict y′} _        _        = conflict~
⊔ᴼ-cong {ok r} {ok r′} {conflict y} {ok s′}       _        (~-by ())
⊔ᴼ-cong {ok r} {ok r′} {ok s}       {conflict y′} _        (~-by ())
⊔ᴼ-cong {ok r} {ok r′} {ok s}       {ok s′}       (~-by e) (~-by f) = join-cong e f

⊔ᴼ-comm : (o o′ : Outcome) → o ⊔ᴼ o′ ~ o′ ⊔ᴼ o
⊔ᴼ-comm (conflict x) (conflict y) = conflict~
⊔ᴼ-comm (conflict x) (ok s)       = conflict~
⊔ᴼ-comm (ok r)       (conflict y) = conflict~
⊔ᴼ-comm (ok r)       (ok s)       = join-comm r s

⊔ᴼ-assoc : (o o′ o″ : Outcome) → (o ⊔ᴼ o′) ⊔ᴼ o″ ~ o ⊔ᴼ (o′ ⊔ᴼ o″)
⊔ᴼ-assoc (conflict x) o′           o″           = conflict~
⊔ᴼ-assoc (ok r)       (conflict y) o″           = conflict~
⊔ᴼ-assoc (ok r)       (ok s)       (conflict z) = ⊔ᴼ-topʳ (r ⊔ s) z
⊔ᴼ-assoc (ok r)       (ok s)       (ok t)       = join-assoc r s t

⊔ᴼ-idem : (o : Outcome) → o ⊔ᴼ o ~ o
⊔ᴼ-idem (conflict x) = conflict~
⊔ᴼ-idem (ok r)       = join-idem r

-- The empty plan mentions no path, so it is compatible with every plan and
-- its union with q is q itself.
⊔ᴼ-unit : (o : Outcome) → ok [] ⊔ᴼ o ~ o
⊔ᴼ-unit (conflict y) = conflict~
⊔ᴼ-unit (ok q)       =
  subst (λ o → o ~ ok q) (sym (join-ok [] q empty-compatible)) ~-refl
  where
    empty-compatible : Compatible [] q
    empty-compatible = compatible (λ x ea eb → ⊥-elim (nothing≢just ea))
