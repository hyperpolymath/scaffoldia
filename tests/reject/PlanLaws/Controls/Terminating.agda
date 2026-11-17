{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- NEGATIVE CONTROL (must be refused). Proves every compatibility claim by a
-- non-terminating definition admitted with a TERMINATING pragma. The gate's
-- pragma check refuses it before Agda runs (Agda under --safe refuses the
-- pragma as well).
-- EXPECT-REFUSED: gate: pragma other than the canonical OPTIONS line

module PlanLaws.Controls.Terminating where

open import PlanLaws.Prelude

{-# TERMINATING #-}
loop : ⊥
loop = loop
