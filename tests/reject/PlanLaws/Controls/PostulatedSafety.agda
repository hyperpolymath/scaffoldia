{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- NEGATIVE CONTROL (must be refused). Assumes, by postulate, the false law
-- that a defined join of plan-safe plans is plan-safe (PlanLaws.Safety,
-- plan-safety-not-preserved, refutes it), and derives a contradiction from
-- the case-fold example. --safe refuses the postulate; the postulate scan
-- would refuse it too.
-- EXPECT-REFUSED: agda: [Cc]annot postulate

module PlanLaws.Controls.PostulatedSafety where

open import PlanLaws.Prelude
open import PlanLaws.Example
open import PlanLaws.Plan P _≟P_ C _≟C_
open import PlanLaws.Join P _≟P_ C _≟C_
open import PlanLaws.Safety P _≟P_ C _≟C_ Safe Clash clash-sym clash-irrefl

postulate
  plan-safe-join-always : (p q r : Plan) → p ⊔ q ≡ ok r → PlanSafe p → PlanSafe q → PlanSafe r

absurd : ⊥
absurd with case-fold-counterexample
... | sp , sq , r , e , unsafe = unsafe (plan-safe-join-always _ _ r e sp sq)
