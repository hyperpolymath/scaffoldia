{-# OPTIONS --safe --with-K #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- NEGATIVE CONTROL (must be refused). Weakens the flags in the file: its
-- OPTIONS pragma asks for K, and the file proves uniqueness of identity
-- proofs with it. The gate refuses any first line other than the canonical
-- OPTIONS pragma, before Agda runs. (Agda 2.7.0.1 refuses it too: "Cannot
-- set OPTIONS pragmas --cubical-compatible and --with-K with safe flag".)
-- EXPECT-REFUSED: gate: first line is not the canonical OPTIONS pragma

module PlanLaws.Controls.WithK where

open import PlanLaws.Prelude

uip : {A : Set} {x y : A} (e e′ : x ≡ y) → e ≡ e′
uip refl refl = refl
