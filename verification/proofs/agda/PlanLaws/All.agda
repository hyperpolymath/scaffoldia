{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- PlanLaws.All: the graded root of the plan-law development.
--
-- Proves nothing itself. Checking this module checks every PlanLaws module,
-- and `agda --dependency-graph` on it gives the import cone that a proof
-- receipt records (PROOF-POLICY §1.1, §8.4). The modules and what each
-- proves:
--   Prelude      the logic (no plan content);
--   Plan         plans, lookup, union, ≈;
--   Join         ADR-0003 law 1 and conflict soundness and completeness;
--   Apply        ADR-0003 laws 3 and 4, and the converse of law 4;
--   Safety       ADR-0003 law 2: true per path, false per plan (with the
--                exact condition and a counterexample);
--   Semilattice  the total join on Plan/≈ with a top for conflict;
--   Kernel       the refinement with preconditions (expect);
--   Example      a concrete instance, so no law is vacuous.

module PlanLaws.All where

import PlanLaws.Prelude
import PlanLaws.Plan
import PlanLaws.Join
import PlanLaws.Apply
import PlanLaws.Safety
import PlanLaws.Semilattice
import PlanLaws.Kernel
import PlanLaws.Example
