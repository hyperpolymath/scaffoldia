{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- PlanLaws.Apply: applying a plan to a file system (ADR-0003 laws 3 and 4).
--
-- A file system is a total map from paths to states (a file with content,
-- or absent). apply p fs overrides fs at the paths p mentions and leaves
-- every other path as it was. Proves:
--   apply-establishes  after apply p, each path of p has its desired state;
--   apply-frame        a path p does not mention is unchanged;
--   apply-idem         apply p (apply p fs) ≗ apply p fs           (law 3);
--   apply-join         if p ⊔ q ≡ ok r then
--                      apply r fs ≗ apply q (apply p fs)           (law 4);
--   apply-comm         for compatible p, q the two orders agree;
--   commute-compatible the converse: if the two orders agree on one file
--                      system, then p and q are compatible, so
--   apply-comm-iff     compatibility is exactly "apply commutes";
--   apply-≈, apply-faithful
--                      ≈ is exactly equality of effect: plans with equal
--                      lookup have equal effect, and (given one content
--                      value) plans with equal effect on every file system
--                      are ≈.
--
-- Equality used. On file systems, ≗: pointwise equality (a record around
-- (x : Path) → f x ≡ g x). Function extensionality is never assumed.
-- Preconditions (expect) are not modelled here; see PlanLaws.Kernel.

open import PlanLaws.Prelude

module PlanLaws.Apply
  (Path    : Set) (_≟P_ : (x y : Path)    → Dec (x ≡ y))
  (Content : Set) (_≟C_ : (c d : Content) → Dec (c ≡ d))
  where

open import PlanLaws.Plan Path _≟P_ Content _≟C_
open import PlanLaws.Join Path _≟P_ Content _≟C_

infix 4 _≗_

------------------------------------------------------------------------
-- File systems and their equality

FS : Set
FS = Path → State

record _≗_ (f g : FS) : Set where
  constructor ≗-intro
  field
    ≗-at : (x : Path) → f x ≡ g x
open _≗_ public

≗-refl : {f : FS} → f ≗ f
≗-refl = ≗-intro (λ x → refl)

≗-sym : {f g : FS} → f ≗ g → g ≗ f
≗-sym e = ≗-intro (λ x → sym (≗-at e x))

≗-trans : {f g h : FS} → f ≗ g → g ≗ h → f ≗ h
≗-trans e e′ = ≗-intro (λ x → trans (≗-at e x) (≗-at e′ x))

------------------------------------------------------------------------
-- Apply

-- The desired state if there is one, else the current state.
override : Maybe State → State → State
override (just s) t = s
override nothing  t = t

apply : Plan → FS → FS
apply p fs x = override (lookup p x) (fs x)

apply-establishes : (p : Plan) (fs : FS) {x : Path} {s : State} →
                    lookup p x ≡ just s → apply p fs x ≡ s
apply-establishes p fs {x} e = cong (λ m → override m (fs x)) e

apply-frame : (p : Plan) (fs : FS) {x : Path} →
              lookup p x ≡ nothing → apply p fs x ≡ fs x
apply-frame p fs {x} e = cong (λ m → override m (fs x)) e

-- apply respects equality of file systems...
apply-cong : (p : Plan) {fs gs : FS} → fs ≗ gs → apply p fs ≗ apply p gs
apply-cong p e = ≗-intro (λ x → cong (override (lookup p x)) (≗-at e x))

-- ...and equality of plans.
apply-≈ : {p q : Plan} → p ≈ q → (fs : FS) → apply p fs ≗ apply q fs
apply-≈ e fs = ≗-intro (λ x → cong (λ m → override m (fs x)) (≈-at e x))

------------------------------------------------------------------------
-- ADR-0003 law 3: applying a plan twice equals applying it once

private
  override-idem : (m : Maybe State) (t : State) → override m (override m t) ≡ override m t
  override-idem (just s) t = refl
  override-idem nothing  t = refl

apply-idem : (p : Plan) (fs : FS) → apply p (apply p fs) ≗ apply p fs
apply-idem p fs = ≗-intro (λ x → override-idem (lookup p x) (fs x))

------------------------------------------------------------------------
-- ADR-0003 law 4: for compatible plans, applying the join is applying one, then
-- the other

apply-∪ : (p q : Plan) → Compatible p q → (fs : FS) →
          apply (p ∪ q) fs ≗ apply q (apply p fs)
apply-∪ p q h fs = ≗-intro at
  where
    at : (x : Path) → apply (p ∪ q) fs x ≡ apply q (apply p fs) x
    at x with maybe-cases (lookup p x)
    ... | inj₂ en = trans (cong (λ m → override m (fs x)) (∪-nothing p q en))
                          (cong (λ m → override (lookup q x) (override m (fs x))) (sym en))
    ... | inj₁ (a , ea) = trans (cong (λ m → override m (fs x)) (∪-just p q ea))
                                (trans (right (maybe-cases (lookup q x)))
                                       (cong (λ m → override (lookup q x) (override m (fs x))) (sym ea)))
      where
        right : Σ State (λ b → lookup q x ≡ just b) ⊎ (lookup q x ≡ nothing) →
                a ≡ override (lookup q x) a
        right (inj₁ (b , eb)) = trans (agree h x ea eb) (cong (λ m → override m a) (sym eb))
        right (inj₂ en′)      = cong (λ m → override m a) (sym en′)

apply-join : (p q : Plan) {r : Plan} → p ⊔ q ≡ ok r → (fs : FS) →
             apply r fs ≗ apply q (apply p fs)
apply-join p q e fs with ok-sound p q e
... | h , refl = apply-∪ p q h fs

-- Hence the two orders agree.
apply-comm : (p q : Plan) → Compatible p q → (fs : FS) →
             apply q (apply p fs) ≗ apply p (apply q fs)
apply-comm p q h fs =
  ≗-trans (≗-sym (apply-∪ p q h fs))
          (≗-trans (apply-≈ (∪-comm p q h) fs) (apply-∪ q p (compatible-sym h) fs))

-- The converse: if the two orders agree on one file system, the plans are
-- compatible. (At a path both plans mention, applying q last leaves q's
-- state and applying p last leaves p's.) So no side condition beyond
-- compatibility is needed, and none can be dropped.
commute-compatible : (p q : Plan) (fs : FS) →
                     apply q (apply p fs) ≗ apply p (apply q fs) → Compatible p q
commute-compatible p q fs e = compatible at
  where
    at : (x : Path) → CompatibleAt p q x
    at x {a} {b} ea eb =
      sym (trans (cong₂ (λ m n → override m (override n (fs x))) (sym eb) (sym ea))
                 (trans (≗-at e x)
                        (cong₂ (λ m n → override m (override n (fs x))) ea eb)))

apply-comm-iff : (p q : Plan) →
                 Compatible p q ⇔ ((fs : FS) → apply q (apply p fs) ≗ apply p (apply q fs))
apply-comm-iff p q =
  mk⇔ (apply-comm p q) (λ c → commute-compatible p q (λ _ → absent) (c (λ _ → absent)))

------------------------------------------------------------------------
-- ≈ is exactly equality of effect

private
  -- A state other than s (needs one content value).
  other : Content → State → State
  other c₀ (file _) = absent
  other c₀ absent   = file c₀

  other-≢ : (c₀ : Content) (s : State) → ¬ (s ≡ other c₀ s)
  other-≢ c₀ (file c) e = file≢absent e
  other-≢ c₀ absent   e = file≢absent (sym e)

apply-faithful : Content → (p q : Plan) →
                 ((fs : FS) → apply p fs ≗ apply q fs) → p ≈ q
apply-faithful c₀ p q obs = ≈-intro at
  where
    -- Observe both plans at x on a constant file system.
    seen : (x : Path) (t : State) {m n : Maybe State} →
           lookup p x ≡ m → lookup q x ≡ n → override m t ≡ override n t
    seen x t em en =
      trans (cong (λ m → override m t) (sym em))
            (trans (≗-at (obs (λ _ → t)) x) (cong (λ n → override n t) en))

    at : (x : Path) → lookup p x ≡ lookup q x
    at x with maybe-cases (lookup p x) | maybe-cases (lookup q x)
    ... | inj₁ (a , ea) | inj₁ (b , eb) =
          trans ea (trans (cong just (seen x absent ea eb)) (sym eb))
    ... | inj₁ (a , ea) | inj₂ en = ⊥-elim (other-≢ c₀ a (seen x (other c₀ a) ea en))
    ... | inj₂ en | inj₁ (b , eb) = ⊥-elim (other-≢ c₀ b (sym (seen x (other c₀ b) en eb)))
    ... | inj₂ en | inj₂ en′ = trans en (sym en′)
