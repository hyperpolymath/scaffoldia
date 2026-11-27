{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- NEGATIVE CONTROL (must be refused). A law whose proof is an unfilled
-- hole. The gate refuses the hole marker; Agda refuses the unsolved
-- interaction meta.
-- EXPECT-REFUSED: gate: hole

module PlanLaws.Controls.Hole where

open import PlanLaws.Prelude
open import PlanLaws.Example
open import PlanLaws.Plan P _≟P_ C _≟C_
open import PlanLaws.Join P _≟P_ C _≟C_

every-pair-compatible : (p q : Plan) → Compatible p q
every-pair-compatible p q = {! !}
