{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- PlanLaws.Safety: what "a join of safe plans is safe" (ADR-0003 law 2)
-- does and does not mean.
--
-- Plan format v0.1 has two kinds of safety condition.
--   * Per path: a path is safe (relative, no `..`, NFC, not under .git, ...).
--     Modelled by an arbitrary predicate Safe on paths.
--   * Per plan: no two paths are equal after case folding, and no path is a
--     proper directory prefix of another. Modelled by an arbitrary relation
--     Clash on paths, assumed symmetric and irreflexive (both real conditions
--     are: each relates two different paths, in either order).
--
-- Proves:
--   safe-join          per-path safety is preserved by union, hence by the
--                      join (safe-join-ok), with the converse (safe-∪-iff);
--   plan-safe-∪-iff    the union is plan-safe iff both parts are plan-safe
--                      and no path of one clashes with a path of the other;
--   plan-safe-join     for a defined join of plan-safe plans, the result is
--                      plan-safe iff there is no cross clash;
--   plan-safety-not-preserved
--                      two plan-safe, compatible plans whose paths clash have
--                      a defined join that is not plan-safe.
-- So ADR-0003 law 2 holds as stated for per-path safety only. For the
-- plan-level conditions it is false: a kernel must re-check case-fold and
-- prefix clashes on the composed plan; a composed plan is not safe merely
-- because its parts were.

open import PlanLaws.Prelude

module PlanLaws.Safety
  (Path    : Set) (_≟P_ : (x y : Path)    → Dec (x ≡ y))
  (Content : Set) (_≟C_ : (c d : Content) → Dec (c ≡ d))
  (Safe    : Path → Set)
  (Clash   : Path → Path → Set)
  (clash-sym    : {x y : Path} → Clash x y → Clash y x)
  (clash-irrefl : {x : Path} → ¬ Clash x x)
  where

open import PlanLaws.Plan Path _≟P_ Content _≟C_
open import PlanLaws.Join Path _≟P_ Content _≟C_

private
  -- Membership in the keys of a union.
  ∈-∪⁻ : (p q : Plan) {x : Path} → x ∈ keys (p ∪ q) → (x ∈ keys p) ⊎ (x ∈ keys q)
  ∈-∪⁻ p q m = ∈-++⁻ (keys p) (keys q) (subst (_ ∈_) (keys-∪ p q) m)

  ∈-∪ˡ : (p q : Plan) {x : Path} → x ∈ keys p → x ∈ keys (p ∪ q)
  ∈-∪ˡ p q m = subst (_ ∈_) (sym (keys-∪ p q)) (∈-++ˡ (keys p) (keys q) m)

  ∈-∪ʳ : (p q : Plan) {x : Path} → x ∈ keys q → x ∈ keys (p ∪ q)
  ∈-∪ʳ p q m = subst (_ ∈_) (sym (keys-∪ p q)) (∈-++ʳ (keys p) (keys q) m)

------------------------------------------------------------------------
-- Per-path safety

record AllSafe (p : Plan) : Set where
  constructor all-safe
  field
    safe-at : (x : Path) → x ∈ keys p → Safe x
open AllSafe public

safe-join : (p q : Plan) → AllSafe p → AllSafe q → AllSafe (p ∪ q)
safe-join p q sp sq = all-safe at
  where
    at : (x : Path) → x ∈ keys (p ∪ q) → Safe x
    at x m with ∈-∪⁻ p q m
    ... | inj₁ mp = safe-at sp x mp
    ... | inj₂ mq = safe-at sq x mq

safe-∪-iff : (p q : Plan) → AllSafe (p ∪ q) ⇔ (AllSafe p × AllSafe q)
safe-∪-iff p q = mk⇔
  (λ s → all-safe (λ x m → safe-at s x (∈-∪ˡ p q m)) , all-safe (λ x m → safe-at s x (∈-∪ʳ p q m)))
  (λ { (sp , sq) → safe-join p q sp sq })

-- ADR-0003 law 2, for per-path safety: a defined join of safe plans is safe.
safe-join-ok : (p q : Plan) {r : Plan} → p ⊔ q ≡ ok r → AllSafe p → AllSafe q → AllSafe r
safe-join-ok p q e sp sq with ok-sound p q e
... | _ , refl = safe-join p q sp sq

------------------------------------------------------------------------
-- Plan-level safety

-- No two paths of p clash.
record NoClash (p : Plan) : Set where
  constructor no-clash
  field
    apart : (x y : Path) → x ∈ keys p → y ∈ keys p → ¬ Clash x y
open NoClash public

-- No path of p clashes with a path of q.
record Cross (p q : Plan) : Set where
  constructor cross
  field
    apart-across : (x y : Path) → x ∈ keys p → y ∈ keys q → ¬ Clash x y
open Cross public

PlanSafe : Plan → Set
PlanSafe p = AllSafe p × NoClash p

plan-safe-∪-iff : (p q : Plan) → PlanSafe (p ∪ q) ⇔ (PlanSafe p × PlanSafe q × Cross p q)
plan-safe-∪-iff p q = mk⇔ to′ from′
  where
    to′ : PlanSafe (p ∪ q) → PlanSafe p × PlanSafe q × Cross p q
    to′ (s , n) =
      ( (proj₁ (to (safe-∪-iff p q) s)
        , no-clash (λ x y mx my → apart n x y (∈-∪ˡ p q mx) (∈-∪ˡ p q my)))
      , (proj₂ (to (safe-∪-iff p q) s)
        , no-clash (λ x y mx my → apart n x y (∈-∪ʳ p q mx) (∈-∪ʳ p q my)))
      , cross (λ x y mx my → apart n x y (∈-∪ˡ p q mx) (∈-∪ʳ p q my)) )

    from′ : PlanSafe p × PlanSafe q × Cross p q → PlanSafe (p ∪ q)
    from′ ((sp , np) , (sq , nq) , c) = safe-join p q sp sq , no-clash at
      where
        at : (x y : Path) → x ∈ keys (p ∪ q) → y ∈ keys (p ∪ q) → ¬ Clash x y
        at x y mx my with ∈-∪⁻ p q mx | ∈-∪⁻ p q my
        ... | inj₁ mxp | inj₁ myp = apart np x y mxp myp
        ... | inj₂ mxq | inj₂ myq = apart nq x y mxq myq
        ... | inj₁ mxp | inj₂ myq = apart-across c x y mxp myq
        ... | inj₂ mxq | inj₁ myp = λ k → apart-across c y x myp mxq (clash-sym k)

-- For a defined join of plan-safe plans: the result is plan-safe exactly
-- when no path of one clashes with a path of the other.
plan-safe-join : (p q : Plan) {r : Plan} → p ⊔ q ≡ ok r →
                 PlanSafe p → PlanSafe q → PlanSafe r ⇔ Cross p q
plan-safe-join p q e sp sq with ok-sound p q e
... | _ , refl = mk⇔ (λ s → proj₂ (proj₂ (to (plan-safe-∪-iff p q) s)))
                     (λ c → from (plan-safe-∪-iff p q) (sp , sq , c))

------------------------------------------------------------------------
-- The counterexample: law 2 is false for plan-level safety

private
  singleton-keys : (x : Path) (s : State) {z : Path} → z ∈ keys ((x , s) ∷ []) → z ≡ x
  singleton-keys x s (inj₁ e) = e
  singleton-keys x s (inj₂ ())

  singleton-safe : (x : Path) (s : State) → Safe x → PlanSafe ((x , s) ∷ [])
  singleton-safe x s sx =
    all-safe (λ z m → subst Safe (sym (singleton-keys x s m)) sx) ,
    no-clash (λ z w mz mw k →
      clash-irrefl (subst (λ v → Clash v x) (singleton-keys x s mz)
                          (subst (Clash z) (singleton-keys x s mw) k)))

-- Two individually plan-safe plans whose paths clash (for example
-- README.adoc and readme.adoc, or a and a/b) are compatible, so their join
-- is defined; the joined plan is not plan-safe.
plan-safety-not-preserved :
  (x y : Path) → Safe x → Safe y → Clash x y → (s t : State) →
  PlanSafe ((x , s) ∷ []) × PlanSafe ((y , t) ∷ []) ×
  Σ Plan (λ r → ((x , s) ∷ []) ⊔ ((y , t) ∷ []) ≡ ok r × ¬ PlanSafe r)
plan-safety-not-preserved x y sx sy k s t =
  singleton-safe x s sx , singleton-safe y t sy ,
  (p ∪ q , join-ok p q compat , not-safe)
  where
    p q : Plan
    p = (x , s) ∷ []
    q = (y , t) ∷ []

    x≢y : ¬ (x ≡ y)
    x≢y e = clash-irrefl (subst (Clash x) (sym e) k)

    compat : Compatible p q
    compat = compatible (λ z ea eb →
      ⊥-elim (x≢y (trans (sym (singleton-keys x s (lookup-keys p ea)))
                         (singleton-keys y t (lookup-keys q eb)))))

    not-safe : ¬ PlanSafe (p ∪ q)
    not-safe (_ , n) = apart n x y (inj₁ refl) (inj₂ (inj₁ refl)) k
