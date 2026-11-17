{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- PlanLaws.Plan: the model of a plan (ADR-0003; plan format v0.1).
--
-- Defines desired states, plans, lookup, the domain of a plan and the
-- equality used for plans, and proves the facts about them that the laws
-- use: the domain is exactly the paths lookup finds (keys-lookup,
-- lookup-keys), lookup distributes over union (lookup-∪), and the plan
-- equality is an equivalence relation.
--
-- Model.
--   * Path and Content are abstract, each with decidable equality (module
--     parameters). Content stands for everything a write fixes: the blob
--     hash and the mode in plan format v0.1.
--   * A desired State is a file with some content, or absent (a delete).
--   * A Plan is a finite list of (path, state) entries, read through lookup,
--     which returns the first entry for a path. Plan format v0.1 forbids two
--     operations on one path (PATH_DUPLICATE); the laws do not need that, so
--     they hold for every list, and a duplicate-free plan is a special case.
--   * Preconditions (expect) and provenance (source) are left out: the laws
--     are about desired states. PlanLaws.Kernel states the refinement that
--     brings expect back in.
--
-- Equality of plans. Function extensionality is not available under
-- --safe --without-K, so plans are never compared as functions. Two plans
-- are equal, p ≈ q, when lookup agrees at every path (pointwise, i.e.
-- extensional, equality of lookup). Every law about plans is stated up to ≈
-- unless it says _≡_.

open import PlanLaws.Prelude

module PlanLaws.Plan
  (Path    : Set) (_≟P_ : (x y : Path)    → Dec (x ≡ y))
  (Content : Set) (_≟C_ : (c d : Content) → Dec (c ≡ d))
  where

------------------------------------------------------------------------
-- Desired states

data State : Set where
  file   : Content → State
  absent : State

private
  content-or : Content → State → Content
  content-or c (file d) = d
  content-or c absent   = c

file-injective : {c d : Content} → file c ≡ file d → c ≡ d
file-injective {c = c} e = cong (content-or c) e

file≢absent : {c : Content} → ¬ (file c ≡ absent)
file≢absent ()

_≟S_ : (s t : State) → Dec (s ≡ t)
file c ≟S file d with c ≟C d
... | yes e  = yes (cong file e)
... | no  ne = no (λ e → ne (file-injective e))
file c ≟S absent = no file≢absent
absent ≟S file d = no (λ e → file≢absent (sym e))
absent ≟S absent = yes refl

------------------------------------------------------------------------
-- Plans

Plan : Set
Plan = List (Path × State)

lookup : Plan → Path → Maybe State
lookup []             x = nothing
lookup ((y , s) ∷ p) x with x ≟P y
... | yes _ = just s
... | no  _ = lookup p x

-- The paths a plan mentions, in order (with any duplicates).
keys : Plan → List Path
keys = map proj₁

-- Union of plans: the entries of p, then those of q. As a map it is
-- left-biased (lookup-∪); the join (PlanLaws.Join) only forms it for
-- compatible plans, where the bias is never visible.
_∪_ : Plan → Plan → Plan
p ∪ q = p ++ q

infixr 5 _∪_
infix  4 _≈_

------------------------------------------------------------------------
-- The domain of a plan is exactly where lookup finds a state

lookup-keys : (p : Plan) {x : Path} {s : State} → lookup p x ≡ just s → x ∈ keys p
lookup-keys []             e = ⊥-elim (nothing≢just e)
lookup-keys ((y , t) ∷ p) {x} e with x ≟P y
... | yes x≡y = inj₁ x≡y
... | no  _   = inj₂ (lookup-keys p e)

keys-lookup : (p : Plan) {x : Path} → x ∈ keys p → Σ State (λ s → lookup p x ≡ just s)
keys-lookup []             ()
keys-lookup ((y , t) ∷ p) {x} m with x ≟P y
... | yes _  = t , refl
keys-lookup ((y , t) ∷ p) {x} (inj₁ x≡y) | no ne = ⊥-elim (ne x≡y)
keys-lookup ((y , t) ∷ p) {x} (inj₂ m)   | no ne = keys-lookup p m

keys-∪ : (p q : Plan) → keys (p ∪ q) ≡ keys p ++ keys q
keys-∪ p q = map-++ proj₁ p q

------------------------------------------------------------------------
-- Lookup in a union

lookup-∪ : (p q : Plan) (x : Path) → lookup (p ∪ q) x ≡ (lookup p x <|> lookup q x)
lookup-∪ []             q x = refl
lookup-∪ ((y , s) ∷ p) q x with x ≟P y
... | yes _ = refl
... | no  _ = lookup-∪ p q x

∪-just : (p q : Plan) {x : Path} {a : State} →
         lookup p x ≡ just a → lookup (p ∪ q) x ≡ just a
∪-just p q {x} e = trans (lookup-∪ p q x) (cong (_<|> lookup q x) e)

∪-nothing : (p q : Plan) {x : Path} →
            lookup p x ≡ nothing → lookup (p ∪ q) x ≡ lookup q x
∪-nothing p q {x} e = trans (lookup-∪ p q x) (cong (_<|> lookup q x) e)

-- A path found in a union was found in the left plan, or only in the right.
∪-inv : (p q : Plan) {x : Path} {a : State} → lookup (p ∪ q) x ≡ just a →
        (lookup p x ≡ just a) ⊎ (lookup p x ≡ nothing × lookup q x ≡ just a)
∪-inv p q {x} e with maybe-cases (lookup p x)
... | inj₁ (b , eb) = inj₁ (trans eb (cong just (just-injective (trans (sym (∪-just p q eb)) e))))
... | inj₂ en       = inj₂ (en , trans (sym (∪-nothing p q en)) e)

------------------------------------------------------------------------
-- Equality of plans: pointwise equality of lookup

-- A record rather than a bare function type, so that p and q can be
-- inferred from a proof of p ≈ q (lookup is not injective).
record _≈_ (p q : Plan) : Set where
  constructor ≈-intro
  field
    ≈-at : (x : Path) → lookup p x ≡ lookup q x
open _≈_ public

≈-refl : {p : Plan} → p ≈ p
≈-refl = ≈-intro (λ x → refl)

≈-sym : {p q : Plan} → p ≈ q → q ≈ p
≈-sym e = ≈-intro (λ x → sym (≈-at e x))

≈-trans : {p q r : Plan} → p ≈ q → q ≈ r → p ≈ r
≈-trans e f = ≈-intro (λ x → trans (≈-at e x) (≈-at f x))

-- Union respects ≈ on both sides.
∪-cong : {p p′ q q′ : Plan} → p ≈ p′ → q ≈ q′ → (p ∪ q) ≈ (p′ ∪ q′)
∪-cong {p} {p′} {q} {q′} ep eq = ≈-intro λ x →
  trans (lookup-∪ p q x)
        (trans (cong₂ _<|>_ (≈-at ep x) (≈-at eq x)) (sym (lookup-∪ p′ q′ x)))

-- Union is associative on the nose (it is list append).
∪-assoc : (p q r : Plan) → (p ∪ q) ∪ r ≡ p ∪ (q ∪ r)
∪-assoc = ++-assoc
