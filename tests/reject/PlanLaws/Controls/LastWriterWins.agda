{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- NEGATIVE CONTROL (must be refused). A false lemma: that two packs writing
-- README.adoc differently join by last-writer-wins. The join reports a
-- conflict instead, so `refl` does not type-check.
-- EXPECT-REFUSED: agda: (conflict README-adoc != ok|!=)

module PlanLaws.Controls.LastWriterWins where

open import PlanLaws.Prelude
open import PlanLaws.Example
open import PlanLaws.Plan P _≟P_ C _≟C_
open import PlanLaws.Join P _≟P_ C _≟C_

last-writer-wins : pack-A ⊔ pack-B ≡ ok (pack-B ∪ pack-A)
last-writer-wins = refl
