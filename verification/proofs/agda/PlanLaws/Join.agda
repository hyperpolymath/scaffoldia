{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- PlanLaws.Join: composition of plans is a join, not an overwrite
-- (ADR-0003, "Composition is a join, not an overwrite", law 1, and the
-- never-last-writer-wins guarantee).
--
-- Proves, for the join _⊔_ : Plan → Plan → Outcome, which either reports a
-- conflict at a path or returns the union:
--   conflict-sound     a reported path is given two different desired states;
--   conflict-complete  if some path is given two different desired states,
--                      the join reports a conflict;
--   join-ok, ok-sound  otherwise the join is the union, and only then;
--   join-lub           the result is the least plan extending both inputs, so
--                      it keeps every desired state of each (no last writer);
--   join-defined-sym   definedness is symmetric;
--   join-comm          p ⊔ q ~ q ⊔ p;
--   join-assoc         (p ⊔ q) ⊔ r ~ p ⊔ (q ⊔ r), with
--   join-assoc-defined either side is defined exactly when p, q and r are
--                      pairwise compatible;
--   join-idem          p ⊔ p ~ ok p;
--   join-cong          _⊔_ respects ≈ on both arguments.
--
-- Equality used. On plans, ≈ (pointwise equality of lookup; PlanLaws.Plan).
-- On outcomes, ~: two conflicts are related whatever paths they name, and
-- ok r ~ ok s exactly when r ≈ s. The conflict path is deliberately left out
-- of ~: p ⊔ q and q ⊔ p can name different conflicting paths (each names
-- the first one in its left argument), and conflict-sound says each named
-- path is a genuine conflict.
--
-- Out of scope: a replacement that an input contract authorises by name
-- (ADR-0003; issue #70). It is not a join and is not modelled here.

open import PlanLaws.Prelude

module PlanLaws.Join
  (Path    : Set) (_≟P_ : (x y : Path)    → Dec (x ≡ y))
  (Content : Set) (_≟C_ : (c d : Content) → Dec (c ≡ d))
  where

open import PlanLaws.Plan Path _≟P_ Content _≟C_

infixl 6 _⊔_ _⊔ᴼ_
infix  4 _⊑_ _~_

------------------------------------------------------------------------
-- Compatibility and conflicts

-- p and q agree at x: if both give x a desired state, it is the same one.
CompatibleAt : Plan → Plan → Path → Set
CompatibleAt p q x = {a b : State} → lookup p x ≡ just a → lookup q x ≡ just b → a ≡ b

-- p and q agree on every path both mention. (A record, so that p and q can
-- be inferred from a proof.)
record Compatible (p q : Plan) : Set where
  constructor compatible
  field
    agree : (x : Path) → CompatibleAt p q x
open Compatible public

-- A path that p and q give two different desired states.
record Conflict (p q : Plan) : Set where
  constructor conflict-at
  field
    path     : Path
    left     : State
    right    : State
    in-left  : lookup p path ≡ just left
    in-right : lookup q path ≡ just right
    differ   : ¬ (left ≡ right)

conflict-incompatible : {p q : Plan} → Conflict p q → ¬ Compatible p q
conflict-incompatible (conflict-at x a b ea eb ne) h = ne (agree h x ea eb)

private
  compatible-at? : (p q : Plan) (x : Path) → Conflict p q ⊎ CompatibleAt p q x
  compatible-at? p q x with maybe-cases (lookup p x) | maybe-cases (lookup q x)
  ... | inj₂ en | _       = inj₂ (λ ea _ → ⊥-elim (nothing≢just (trans (sym en) ea)))
  ... | inj₁ _  | inj₂ en = inj₂ (λ _ eb → ⊥-elim (nothing≢just (trans (sym en) eb)))
  ... | inj₁ (a , ea) | inj₁ (b , eb) with a ≟S b
  ...   | no ne   = inj₁ (conflict-at x a b ea eb ne)
  ...   | yes a≡b = inj₂ (λ ea′ eb′ →
                      trans (just-injective (trans (sym ea′) ea))
                            (trans a≡b (just-injective (trans (sym eb) eb′))))

  compatible-on? : (p q : Plan) (ks : List Path) →
                   Conflict p q ⊎ ((x : Path) → x ∈ ks → CompatibleAt p q x)
  compatible-on? p q []       = inj₂ (λ x ())
  compatible-on? p q (k ∷ ks) with compatible-at? p q k
  ... | inj₁ c  = inj₁ c
  ... | inj₂ hk with compatible-on? p q ks
  ...   | inj₁ c  = inj₁ c
  ...   | inj₂ hs = inj₂ λ { x (inj₁ x≡k) → subst (CompatibleAt p q) (sym x≡k) hk
                           ; x (inj₂ m)   → hs x m }

-- The decision procedure: a conflict, or a proof of compatibility. It
-- inspects the paths of p in order and reports the first that conflicts.
compatible? : (p q : Plan) → Conflict p q ⊎ Compatible p q
compatible? p q with compatible-on? p q (keys p)
... | inj₁ c = inj₁ c
... | inj₂ h = inj₂ (compatible (λ x ea eb → h x (lookup-keys p ea) ea eb))

compatible-dec : (p q : Plan) → Dec (Compatible p q)
compatible-dec p q with compatible? p q
... | inj₁ c = no (conflict-incompatible c)
... | inj₂ h = yes h

------------------------------------------------------------------------
-- The join

-- What a kernel returns: a refusal naming a path, or the composed plan.
data Outcome : Set where
  conflict : Path → Outcome
  ok       : Plan → Outcome

outcome : (p q : Plan) → Conflict p q ⊎ Compatible p q → Outcome
outcome p q (inj₁ c) = conflict (Conflict.path c)
outcome p q (inj₂ _) = ok (p ∪ q)

_⊔_ : Plan → Plan → Outcome
p ⊔ q = outcome p q (compatible? p q)

private
  ok-injective : {r s : Plan} → ok r ≡ ok s → r ≡ s
  ok-injective {r} e = cong pick e
    where
      pick : Outcome → Plan
      pick (ok s)       = s
      pick (conflict _) = r

  conflict-injective : {x y : Path} → conflict x ≡ conflict y → x ≡ y
  conflict-injective {x} e = cong pick e
    where
      pick : Outcome → Path
      pick (conflict y) = y
      pick (ok _)       = x

  ok≢conflict : {r : Plan} {x : Path} → ¬ (ok r ≡ conflict x)
  ok≢conflict ()

  outcome-ok : (p q : Plan) (d : Conflict p q ⊎ Compatible p q) →
               Compatible p q → outcome p q d ≡ ok (p ∪ q)
  outcome-ok p q (inj₁ c) h = ⊥-elim (conflict-incompatible c h)
  outcome-ok p q (inj₂ _) h = refl

  outcome-ko : (p q : Plan) (d : Conflict p q ⊎ Compatible p q) →
               ¬ Compatible p q → Σ Path (λ x → outcome p q d ≡ conflict x)
  outcome-ko p q (inj₁ c) n = Conflict.path c , refl
  outcome-ko p q (inj₂ h) n = ⊥-elim (n h)

  outcome-ok-inv : (p q : Plan) (d : Conflict p q ⊎ Compatible p q) {r : Plan} →
                   outcome p q d ≡ ok r → Compatible p q × r ≡ p ∪ q
  outcome-ok-inv p q (inj₁ c) e = ⊥-elim (ok≢conflict (sym e))
  outcome-ok-inv p q (inj₂ h) e = h , sym (ok-injective e)

  outcome-conflict : (p q : Plan) (d : Conflict p q ⊎ Compatible p q) {x : Path} →
                     outcome p q d ≡ conflict x →
                     Σ State (λ a → Σ State (λ b →
                       (lookup p x ≡ just a) × (lookup q x ≡ just b) × ¬ (a ≡ b)))
  outcome-conflict p q (inj₁ (conflict-at y a b ea eb ne)) {x} e =
    subst (λ z → Σ State (λ a → Σ State (λ b →
                   (lookup p z ≡ just a) × (lookup q z ≡ just b) × ¬ (a ≡ b))))
          (conflict-injective e) (a , b , ea , eb , ne)
  outcome-conflict p q (inj₂ h) e = ⊥-elim (ok≢conflict e)

-- For compatible plans the join is their union.
join-ok : (p q : Plan) → Compatible p q → p ⊔ q ≡ ok (p ∪ q)
join-ok p q = outcome-ok p q (compatible? p q)

-- For incompatible plans the join is a conflict.
join-ko : (p q : Plan) → ¬ Compatible p q → Σ Path (λ x → p ⊔ q ≡ conflict x)
join-ko p q = outcome-ko p q (compatible? p q)

-- A plan returned by the join is the union, of compatible plans.
ok-sound : (p q : Plan) {r : Plan} → p ⊔ q ≡ ok r → Compatible p q × r ≡ p ∪ q
ok-sound p q = outcome-ok-inv p q (compatible? p q)

------------------------------------------------------------------------
-- Conflicts are exact: never last-writer-wins

-- A reported path is one that p and q give two different desired states.
conflict-sound : (p q : Plan) {x : Path} → p ⊔ q ≡ conflict x →
                 Σ State (λ a → Σ State (λ b →
                   (lookup p x ≡ just a) × (lookup q x ≡ just b) × ¬ (a ≡ b)))
conflict-sound p q = outcome-conflict p q (compatible? p q)

-- If some path is given two different desired states, the join reports a
-- conflict (at some path, which conflict-sound says is also a conflict).
conflict-complete : (p q : Plan) {x : Path} {a b : State} →
                    lookup p x ≡ just a → lookup q x ≡ just b → ¬ (a ≡ b) →
                    Σ Path (λ y → p ⊔ q ≡ conflict y)
conflict-complete p q {x} ea eb ne = join-ko p q (λ h → ne (agree h x ea eb))

-- Both directions at once: the join is a conflict iff the plans are not
-- compatible.
conflict-iff : (p q : Plan) → (Σ Path (λ y → p ⊔ q ≡ conflict y)) ⇔ ¬ Compatible p q
conflict-iff p q = mk⇔ to′ (join-ko p q)
  where
    to′ : Σ Path (λ y → p ⊔ q ≡ conflict y) → ¬ Compatible p q
    to′ (y , e) h with conflict-sound p q e
    ... | a , b , ea , eb , ne = ne (agree h y ea eb)

------------------------------------------------------------------------
-- The extension order: the join is a least upper bound

-- q extends p: q gives every path of p the same desired state.
record _⊑_ (p q : Plan) : Set where
  constructor ⊑-intro
  field
    extends : (x : Path) {a : State} → lookup p x ≡ just a → lookup q x ≡ just a
open _⊑_ public

⊑-refl : {p : Plan} → p ⊑ p
⊑-refl = ⊑-intro (λ x e → e)

⊑-trans : {p q r : Plan} → p ⊑ q → q ⊑ r → p ⊑ r
⊑-trans f g = ⊑-intro (λ x e → extends g x (extends f x e))

-- ≈ is mutual extension.
≈⇔⊑⊒ : {p q : Plan} → p ≈ q ⇔ (p ⊑ q × q ⊑ p)
≈⇔⊑⊒ {p} {q} = mk⇔ to′ from′
  where
    to′ : p ≈ q → p ⊑ q × q ⊑ p
    to′ e = ⊑-intro (λ x ea → trans (sym (≈-at e x)) ea) , ⊑-intro (λ x eb → trans (≈-at e x) eb)

    from′ : p ⊑ q × q ⊑ p → p ≈ q
    from′ (f , g) = ≈-intro at
      where
        at : (x : Path) → lookup p x ≡ lookup q x
        at x with maybe-cases (lookup p x) | maybe-cases (lookup q x)
        ... | inj₁ (a , ea) | _             = trans ea (sym (extends f x ea))
        ... | inj₂ _        | inj₁ (b , eb) = trans (extends g x eb) (sym eb)
        ... | inj₂ en       | inj₂ en′      = trans en (sym en′)

∪-upperˡ : (p q : Plan) → p ⊑ (p ∪ q)
∪-upperˡ p q = ⊑-intro (λ x e → ∪-just p q e)

∪-upperʳ : (p q : Plan) → Compatible p q → q ⊑ (p ∪ q)
∪-upperʳ p q h = ⊑-intro at
  where
    at : (x : Path) {b : State} → lookup q x ≡ just b → lookup (p ∪ q) x ≡ just b
    at x eb with maybe-cases (lookup p x)
    ... | inj₁ (a , ea) = trans (∪-just p q ea) (cong just (agree h x ea eb))
    ... | inj₂ en       = trans (∪-nothing p q en) eb

∪-least : (p q s : Plan) → p ⊑ s → q ⊑ s → (p ∪ q) ⊑ s
∪-least p q s f g = ⊑-intro at
  where
    at : (x : Path) {a : State} → lookup (p ∪ q) x ≡ just a → lookup s x ≡ just a
    at x e with ∪-inv p q e
    ... | inj₁ ea       = extends f x ea
    ... | inj₂ (_ , eb) = extends g x eb

-- Plans with a common extension are compatible.
bounded-compatible : {p q s : Plan} → p ⊑ s → q ⊑ s → Compatible p q
bounded-compatible f g =
  compatible (λ x ea eb → just-injective (trans (sym (extends f x ea)) (extends g x eb)))

-- The join, when defined, is the least common extension of its inputs: it
-- keeps every desired state of p and every desired state of q, and adds
-- nothing else.
join-lub : (p q : Plan) {r : Plan} → p ⊔ q ≡ ok r →
           (p ⊑ r) × (q ⊑ r) × ((s : Plan) → p ⊑ s → q ⊑ s → r ⊑ s)
join-lub p q e with ok-sound p q e
... | h , refl = ∪-upperˡ p q , ∪-upperʳ p q h , ∪-least p q

-- The join is defined exactly when the inputs have a common extension.
join-defined-iff-bounded : (p q : Plan) →
  (Σ Plan (λ r → p ⊔ q ≡ ok r)) ⇔ (Σ Plan (λ s → p ⊑ s × q ⊑ s))
join-defined-iff-bounded p q = mk⇔ to′ from′
  where
    to′ : Σ Plan (λ r → p ⊔ q ≡ ok r) → Σ Plan (λ s → p ⊑ s × q ⊑ s)
    to′ (r , e) with join-lub p q e
    ... | f , g , _ = r , f , g

    from′ : Σ Plan (λ s → p ⊑ s × q ⊑ s) → Σ Plan (λ r → p ⊔ q ≡ ok r)
    from′ (s , f , g) = p ∪ q , join-ok p q (bounded-compatible {p} {q} {s} f g)

------------------------------------------------------------------------
-- Facts about compatibility and union

compatible-refl : (p : Plan) → Compatible p p
compatible-refl p = compatible (λ x ea ea′ → just-injective (trans (sym ea) ea′))

compatible-sym : {p q : Plan} → Compatible p q → Compatible q p
compatible-sym h = compatible (λ x eb ea → sym (agree h x ea eb))

compatible-cong : {p p′ q q′ : Plan} → p ≈ p′ → q ≈ q′ →
                  Compatible p q → Compatible p′ q′
compatible-cong ep eq h =
  compatible (λ x ea eb → agree h x (trans (≈-at ep x) ea) (trans (≈-at eq x) eb))

∪-comm : (p q : Plan) → Compatible p q → (p ∪ q) ≈ (q ∪ p)
∪-comm p q h = from (≈⇔⊑⊒ {p ∪ q} {q ∪ p})
  ( ∪-least p q (q ∪ p) (∪-upperʳ q p (compatible-sym {p} {q} h)) (∪-upperˡ q p)
  , ∪-least q p (p ∪ q) (∪-upperʳ p q h) (∪-upperˡ p q) )

∪-idem : (p : Plan) → (p ∪ p) ≈ p
∪-idem p = from (≈⇔⊑⊒ {p ∪ p} {p}) (∪-least p p p (⊑-refl {p}) (⊑-refl {p}) , ∪-upperˡ p p)

-- A union is compatible with r when both parts are (no side condition).
∪-compatibleˡ : (p q r : Plan) → Compatible p r → Compatible q r → Compatible (p ∪ q) r
∪-compatibleˡ p q r hp hq = compatible at
  where
    at : (x : Path) → CompatibleAt (p ∪ q) r x
    at x e ec with ∪-inv p q e
    ... | inj₁ ea       = agree hp x ea ec
    ... | inj₂ (_ , eb) = agree hq x eb ec

-- Conversely, for compatible p and q, a union compatible with r has both
-- parts compatible with r. (Without Compatible p q the second part fails:
-- p can hide a disagreement between q and r.)
∪-compatibleˡ⁻ : (p q r : Plan) → Compatible p q → Compatible (p ∪ q) r →
                 Compatible p r × Compatible q r
∪-compatibleˡ⁻ p q r hpq h = hp , hq
  where
    hp : Compatible p r
    hp = compatible (λ x ea ec → agree h x (∪-just p q ea) ec)

    at : (x : Path) → CompatibleAt q r x
    at x eb ec with maybe-cases (lookup p x)
    ... | inj₁ (a , ea) = trans (sym (agree hpq x ea eb)) (agree h x (∪-just p q ea) ec)
    ... | inj₂ en       = agree h x (trans (∪-nothing p q en) eb) ec

    hq : Compatible q r
    hq = compatible at

-- Pairwise compatibility of three plans.
Compatible₃ : Plan → Plan → Plan → Set
Compatible₃ p q r = Compatible p q × Compatible q r × Compatible p r

-- The definedness condition of the left-nested join is pairwise
-- compatibility...
assoc-definedˡ : (p q r : Plan) → (Compatible p q × Compatible (p ∪ q) r) ⇔ Compatible₃ p q r
assoc-definedˡ p q r = mk⇔ to′ from′
  where
    to′ : Compatible p q × Compatible (p ∪ q) r → Compatible₃ p q r
    to′ (hpq , h) with ∪-compatibleˡ⁻ p q r hpq h
    ... | hpr , hqr = hpq , hqr , hpr

    from′ : Compatible₃ p q r → Compatible p q × Compatible (p ∪ q) r
    from′ (hpq , hqr , hpr) = hpq , ∪-compatibleˡ p q r hpr hqr

-- ...and so is that of the right-nested join.
assoc-definedʳ : (p q r : Plan) → (Compatible q r × Compatible p (q ∪ r)) ⇔ Compatible₃ p q r
assoc-definedʳ p q r = mk⇔ to′ from′
  where
    to′ : Compatible q r × Compatible p (q ∪ r) → Compatible₃ p q r
    to′ (hqr , h) with ∪-compatibleˡ⁻ q r p hqr (compatible-sym {p} {q ∪ r} h)
    ... | hqp , hrp = compatible-sym {q} {p} hqp , hqr , compatible-sym {r} {p} hrp

    from′ : Compatible₃ p q r → Compatible q r × Compatible p (q ∪ r)
    from′ (hpq , hqr , hpr) =
      hqr , compatible-sym {q ∪ r} {p}
              (∪-compatibleˡ q r p (compatible-sym {p} {q} hpq) (compatible-sym {p} {r} hpr))

------------------------------------------------------------------------
-- Equality of outcomes

-- Two conflicts are equal whatever paths they name; two plans are equal
-- when ≈. Under ~, Outcome is the quotient Plan/≈ with one adjoined element
-- for "conflict".
-- (Defined by recursion and wrapped in a record, rather than as an indexed
-- family, so that no proof needs injectivity of conflict or ok, which
-- --cubical-compatible warns about, and the outcomes stay inferable.)
SameOutcome : Outcome → Outcome → Set
SameOutcome (conflict x) (conflict y) = ⊤
SameOutcome (conflict x) (ok s)       = ⊥
SameOutcome (ok r)       (conflict y) = ⊥
SameOutcome (ok r)       (ok s)       = r ≈ s

record _~_ (o o′ : Outcome) : Set where
  constructor ~-by
  field
    same-outcome : SameOutcome o o′
open _~_ public

conflict~ : {x y : Path} → conflict x ~ conflict y
conflict~ = ~-by tt

ok~ : {r s : Plan} → r ≈ s → ok r ~ ok s
ok~ e = ~-by e

~-refl : {o : Outcome} → o ~ o
~-refl {conflict x} = conflict~
~-refl {ok r}       = ok~ ≈-refl

~-sym : {o o′ : Outcome} → o ~ o′ → o′ ~ o
~-sym {conflict x} {conflict y} _        = conflict~
~-sym {conflict x} {ok s}       (~-by ())
~-sym {ok r}       {conflict y} (~-by ())
~-sym {ok r}       {ok s}       (~-by e) = ok~ (≈-sym e)

~-trans : {o o′ o″ : Outcome} → o ~ o′ → o′ ~ o″ → o ~ o″
~-trans {conflict x} {conflict y} {conflict z} _        _        = conflict~
~-trans {ok r}       {ok s}       {ok t}       (~-by e) (~-by f) = ok~ (≈-trans e f)
~-trans {conflict x} {ok s}                    (~-by ()) _
~-trans {ok r}       {conflict y}              (~-by ()) _
~-trans {conflict x} {conflict y} {ok t}       _        (~-by ())
~-trans {ok r}       {ok s}       {conflict z} _        (~-by ())

-- Outcomes are ~ when they are defined together and agree when defined.
~-intro : (o o′ : Outcome) →
          (Σ Plan (λ r → o ≡ ok r)) ⇔ (Σ Plan (λ r′ → o′ ≡ ok r′)) →
          ((r r′ : Plan) → o ≡ ok r → o′ ≡ ok r′ → r ≈ r′) → o ~ o′
~-intro (conflict x) (conflict y) d f = conflict~
~-intro (ok r)       (ok r′)      d f = ok~ (f r r′ refl refl)
~-intro (ok r)       (conflict y) d f with to d (r , refl)
... | _ , ()
~-intro (conflict x) (ok r′)      d f with from d (r′ , refl)
... | _ , ()

------------------------------------------------------------------------
-- ADR-0003 law 1: commutative, associative, idempotent wherever defined

-- Definedness is symmetric.
join-defined-sym : (p q : Plan) → Σ Plan (λ r → p ⊔ q ≡ ok r) → Σ Plan (λ r → q ⊔ p ≡ ok r)
join-defined-sym p q (r , e) with ok-sound p q e
... | h , _ = q ∪ p , join-ok q p (compatible-sym h)

join-comm : (p q : Plan) → p ⊔ q ~ q ⊔ p
join-comm p q = ~-intro (p ⊔ q) (q ⊔ p)
  (mk⇔ (join-defined-sym p q) (join-defined-sym q p))
  (λ r r′ e e′ → same (ok-sound p q e) (ok-sound q p e′))
  where
    same : {r r′ : Plan} → Compatible p q × r ≡ p ∪ q → Compatible q p × r′ ≡ q ∪ p → r ≈ r′
    same (h , refl) (_ , refl) = ∪-comm p q h

-- The join lifted to outcomes: a conflict on either side is kept (the left
-- one first), otherwise the plans are joined. PlanLaws.Semilattice proves
-- that it is a total join-semilattice operation up to ~.
_⊔ᴼ_ : Outcome → Outcome → Outcome
conflict x ⊔ᴼ o          = conflict x
ok r       ⊔ᴼ conflict y = conflict y
ok r       ⊔ᴼ ok s       = r ⊔ s

private
  ⊔ᴼ-okˡ-inv : (o : Outcome) (r : Plan) {s : Plan} → o ⊔ᴼ ok r ≡ ok s →
               Σ Plan (λ t → (o ≡ ok t) × (t ⊔ r ≡ ok s))
  ⊔ᴼ-okˡ-inv (conflict x) r ()
  ⊔ᴼ-okˡ-inv (ok t)       r e = t , refl , e

  ⊔ᴼ-okʳ-inv : (p : Plan) (o : Outcome) {s : Plan} → ok p ⊔ᴼ o ≡ ok s →
               Σ Plan (λ t → (o ≡ ok t) × (p ⊔ t ≡ ok s))
  ⊔ᴼ-okʳ-inv p (conflict y) ()
  ⊔ᴼ-okʳ-inv p (ok t)       e = t , refl , e

  ≡→≈ : {p q : Plan} → p ≡ q → p ≈ q
  ≡→≈ refl = ≈-refl

-- Associativity. Either side is defined exactly when p, q and r are
-- pairwise compatible...
join-assoc-defined : (p q r : Plan) →
  ((Σ Plan (λ s → (p ⊔ q) ⊔ᴼ ok r ≡ ok s)) ⇔ Compatible₃ p q r) ×
  ((Σ Plan (λ s → ok p ⊔ᴼ (q ⊔ r) ≡ ok s)) ⇔ Compatible₃ p q r)
join-assoc-defined p q r = mk⇔ lto lfrom , mk⇔ rto rfrom
  where
    lto : Σ Plan (λ s → (p ⊔ q) ⊔ᴼ ok r ≡ ok s) → Compatible₃ p q r
    lto (s , e) with ⊔ᴼ-okˡ-inv (p ⊔ q) r e
    ... | t , e₁ , e₂ with ok-sound p q e₁ | ok-sound t r e₂
    ...   | hpq , refl | h , _ = to (assoc-definedˡ p q r) (hpq , h)

    lfrom : Compatible₃ p q r → Σ Plan (λ s → (p ⊔ q) ⊔ᴼ ok r ≡ ok s)
    lfrom c with from (assoc-definedˡ p q r) c
    ... | hpq , h = (p ∪ q) ∪ r ,
                    trans (cong (_⊔ᴼ ok r) (join-ok p q hpq)) (join-ok (p ∪ q) r h)

    rto : Σ Plan (λ s → ok p ⊔ᴼ (q ⊔ r) ≡ ok s) → Compatible₃ p q r
    rto (s , e) with ⊔ᴼ-okʳ-inv p (q ⊔ r) e
    ... | t , e₁ , e₂ with ok-sound q r e₁ | ok-sound p t e₂
    ...   | hqr , refl | h , _ = to (assoc-definedʳ p q r) (hqr , h)

    rfrom : Compatible₃ p q r → Σ Plan (λ s → ok p ⊔ᴼ (q ⊔ r) ≡ ok s)
    rfrom c with from (assoc-definedʳ p q r) c
    ... | hqr , h = p ∪ (q ∪ r) ,
                    trans (cong (ok p ⊔ᴼ_) (join-ok q r hqr)) (join-ok p (q ∪ r) h)

-- ...and then the two sides are equal.
join-assoc : (p q r : Plan) → (p ⊔ q) ⊔ᴼ ok r ~ ok p ⊔ᴼ (q ⊔ r)
join-assoc p q r = ~-intro ((p ⊔ q) ⊔ᴼ ok r) (ok p ⊔ᴼ (q ⊔ r))
  (mk⇔ (λ d → from (proj₂ defined) (to (proj₁ defined) d))
       (λ d → from (proj₁ defined) (to (proj₂ defined) d)))
  same
  where
    defined = join-assoc-defined p q r

    same : (s s′ : Plan) → (p ⊔ q) ⊔ᴼ ok r ≡ ok s → ok p ⊔ᴼ (q ⊔ r) ≡ ok s′ → s ≈ s′
    same s s′ e e′ with ⊔ᴼ-okˡ-inv (p ⊔ q) r e | ⊔ᴼ-okʳ-inv p (q ⊔ r) e′
    ... | t , e₁ , e₂ | t′ , e₁′ , e₂′
        with ok-sound p q e₁ | ok-sound t r e₂ | ok-sound q r e₁′ | ok-sound p t′ e₂′
    ...   | _ , refl | _ , refl | _ , refl | _ , refl = ≡→≈ (∪-assoc p q r)

-- Idempotence: a plan joined with itself is defined and is itself.
join-idem : (p : Plan) → p ⊔ p ~ ok p
join-idem p = ~-intro (p ⊔ p) (ok p)
  (mk⇔ (λ _ → p , refl) (λ _ → p ∪ p , join-ok p p (compatible-refl p)))
  (λ r r′ e e′ → same (ok-sound p p e) e′)
  where
    same : {r r′ : Plan} → Compatible p p × r ≡ p ∪ p → ok p ≡ ok r′ → r ≈ r′
    same (_ , refl) e′ = ≈-trans (∪-idem p) (≡→≈ (ok-injective e′))

-- The join respects ≈, so the laws above are laws of the quotient Plan/≈.
join-cong : {p p′ q q′ : Plan} → p ≈ p′ → q ≈ q′ → p ⊔ q ~ p′ ⊔ q′
join-cong {p} {p′} {q} {q′} ep eq = ~-intro (p ⊔ q) (p′ ⊔ q′)
  (mk⇔ (λ { (r , e) → p′ ∪ q′ ,
              join-ok p′ q′ (compatible-cong ep eq (proj₁ (ok-sound p q e))) })
       (λ { (r , e) → p ∪ q ,
              join-ok p q (compatible-cong (≈-sym ep) (≈-sym eq) (proj₁ (ok-sound p′ q′ e))) }))
  (λ r r′ e e′ → same (ok-sound p q e) (ok-sound p′ q′ e′))
  where
    same : {r r′ : Plan} → Compatible p q × r ≡ p ∪ q → Compatible p′ q′ × r′ ≡ p′ ∪ q′ → r ≈ r′
    same (_ , refl) (_ , refl) = ∪-cong ep eq
