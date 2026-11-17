{-# OPTIONS --safe --without-K --cubical-compatible #-}
-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
--
-- PlanLaws.Kernel: the refinement that brings preconditions (expect) back.
--
-- The laws in PlanLaws.Join and PlanLaws.Apply are about desired states. A
-- kernel operation also carries a precondition, `expect` in plan format
-- v0.1, on the state the target has before the write. Expect is abstract
-- here: any type with a decidable "holds on this state" relation (in v0.1:
-- "absent", or a content hash). The kernel step of "Apply" in
-- verification/conformance/README.adoc is modelled as kapply: every
-- operation must find its path already in the desired state (a no-op) or
-- find its expect holding; otherwise refuse and write nothing. Proves:
--   kapply-refines   an accepted kernel apply is the model's apply of the
--                    desired states;
--   kapply-refuses   a refusal is exact: some operation is neither done nor
--                    expected;
--   unique-functional
--                    in a plan with no two operations on one path (v0.1,
--                    PATH_DUPLICATE), lookup finds each operation's state;
--   kapply-idem      for such a plan, a second kernel apply is accepted, finds
--                    every path already in its desired state (receipt: only
--                    unchanged or already-absent), and changes nothing.
-- Not modelled: blobs and hashes, modes beyond what Content holds, symbolic
-- links, the directory structure of the target, and partial writes. The
-- refinement from this model to kernel code is a Creusot/Pearlite and Verus
-- obligation on the Rust kernel (PROOF-POLICY §1.2), which does not exist yet.

open import PlanLaws.Prelude

module PlanLaws.Kernel
  (Path    : Set) (_≟P_ : (x y : Path)    → Dec (x ≡ y))
  (Content : Set) (_≟C_ : (c d : Content) → Dec (c ≡ d))
  where

open import PlanLaws.Plan  Path _≟P_ Content _≟C_
open import PlanLaws.Apply Path _≟P_ Content _≟C_

-- The precondition language is a parameter of this inner module, because
-- it mentions State.
module WithExpect
  (Expect : Set)
  (Holds  : Expect → State → Set)
  (holds? : (e : Expect) (s : State) → Dec (Holds e s))
  where

  record KOp : Set where
    constructor kop
    field
      kpath   : Path
      desired : State
      expect  : Expect
  open KOp public

  KPlan : Set
  KPlan = List KOp

  -- The plan of desired states that a kernel plan carries.
  desired-plan : KPlan → Plan
  desired-plan = map (λ o → kpath o , desired o)

  -- A property of every operation of a kernel plan.
  Every : (KOp → Set) → KPlan → Set
  Every P []       = ⊤
  Every P (o ∷ kp) = P o × Every P kp

  -- An operation may run on fs: its path is already in the desired state,
  -- or its expect holds there.
  Ready : FS → KOp → Set
  Ready fs o = (fs (kpath o) ≡ desired o) ⊎ Holds (expect o) (fs (kpath o))

  private
    ready? : (fs : FS) (o : KOp) → Dec (Ready fs o)
    ready? fs o with fs (kpath o) ≟S desired o
    ... | yes e = yes (inj₁ e)
    ... | no ne with holds? (expect o) (fs (kpath o))
    ...   | yes h = yes (inj₂ h)
    ...   | no nh = no (λ { (inj₁ e) → ne e ; (inj₂ h) → nh h })

  every-ready? : (fs : FS) (kp : KPlan) → Dec (Every (Ready fs) kp)
  every-ready? fs []       = yes tt
  every-ready? fs (o ∷ kp) with ready? fs o
  ... | no n = no (λ r → n (proj₁ r))
  ... | yes r with every-ready? fs kp
  ...   | yes rs = yes (r , rs)
  ...   | no n   = no (λ r′ → n (proj₂ r′))

  -- The kernel: refuse (nothing) unless every operation is ready, then
  -- apply all of them.
  decide-apply : (kp : KPlan) (fs : FS) → Dec (Every (Ready fs) kp) → Maybe FS
  decide-apply kp fs (yes _) = just (apply (desired-plan kp) fs)
  decide-apply kp fs (no _)  = nothing

  kapply : KPlan → FS → Maybe FS
  kapply kp fs = decide-apply kp fs (every-ready? fs kp)

  private
    decide-refines : (kp : KPlan) (fs : FS) (d : Dec (Every (Ready fs) kp)) {fs′ : FS} →
                     decide-apply kp fs d ≡ just fs′ →
                     Every (Ready fs) kp × apply (desired-plan kp) fs ≡ fs′
    decide-refines kp fs (yes r) e = r , just-injective e
    decide-refines kp fs (no _)  e = ⊥-elim (nothing≢just e)

    decide-refuses : (kp : KPlan) (fs : FS) (d : Dec (Every (Ready fs) kp)) →
                     decide-apply kp fs d ≡ nothing → ¬ Every (Ready fs) kp
    decide-refuses kp fs (yes r) e = ⊥-elim (nothing≢just (sym e))
    decide-refuses kp fs (no n)  e = n

    decide-accepts : (kp : KPlan) (fs : FS) (d : Dec (Every (Ready fs) kp)) →
                     Every (Ready fs) kp → decide-apply kp fs d ≡ just (apply (desired-plan kp) fs)
    decide-accepts kp fs (yes _) r = refl
    decide-accepts kp fs (no n)  r = ⊥-elim (n r)

  kapply-refines : (kp : KPlan) (fs : FS) {fs′ : FS} → kapply kp fs ≡ just fs′ →
                   Every (Ready fs) kp × apply (desired-plan kp) fs ≡ fs′
  kapply-refines kp fs = decide-refines kp fs (every-ready? fs kp)

  kapply-refuses : (kp : KPlan) (fs : FS) → kapply kp fs ≡ nothing → ¬ Every (Ready fs) kp
  kapply-refuses kp fs = decide-refuses kp fs (every-ready? fs kp)

  ----------------------------------------------------------------------
  -- Plans with one operation per path

  Unique : List Path → Set
  Unique []       = ⊤
  Unique (x ∷ xs) = ¬ (x ∈ xs) × Unique xs

  -- Lookup in the desired plan finds each operation's own desired state.
  Functional : KPlan → Set
  Functional kp = Every (λ o → lookup (desired-plan kp) (kpath o) ≡ just (desired o)) kp

  private
    every-map : {P Q : KOp → Set} (kp : KPlan) → Every (λ o → P o → Q o) kp → Every P kp → Every Q kp
    every-map []       _        _        = tt
    every-map (o ∷ kp) (f , fs) (p , ps) = f p , every-map kp fs ps

    every-intro : {P : KOp → Set} (kp : KPlan) → ((o : KOp) → P o) → Every P kp
    every-intro []       f = tt
    every-intro (o ∷ kp) f = f o , every-intro kp f

    -- Each operation's path is among the paths of the plan.
    every-mem : (kp : KPlan) → Every (λ o → kpath o ∈ map kpath kp) kp
    every-mem []       = tt
    every-mem (o ∷ kp) = inj₁ refl ,
      every-map kp (every-intro kp (λ o′ m → inj₂ m)) (every-mem kp)

    lookup-head : (x : Path) (s : State) (p : Plan) → lookup ((x , s) ∷ p) x ≡ just s
    lookup-head x s p with x ≟P x
    ... | yes _ = refl
    ... | no ne = ⊥-elim (ne refl)

    lookup-tail : (x y : Path) (s : State) (p : Plan) → ¬ (y ≡ x) →
                  lookup ((x , s) ∷ p) y ≡ lookup p y
    lookup-tail x y s p ne with y ≟P x
    ... | yes e = ⊥-elim (ne e)
    ... | no _  = refl

  unique-functional : (kp : KPlan) → Unique (map kpath kp) → Functional kp
  unique-functional []       _            = tt
  unique-functional (o ∷ kp) (fresh , uq) =
    lookup-head (kpath o) (desired o) (desired-plan kp) ,
    every-map kp
      (every-map kp
        (every-intro kp (λ o′ m e →
          trans (lookup-tail (kpath o) (kpath o′) (desired o) (desired-plan kp)
                  (λ eq → fresh (subst (_∈ map kpath kp) eq m)))
                e))
        (every-mem kp))
      (unique-functional kp uq)

  ----------------------------------------------------------------------
  -- A second kernel apply changes nothing

  kapply-idem : (kp : KPlan) → Functional kp → (fs : FS) {fs′ : FS} →
                kapply kp fs ≡ just fs′ →
                Every (λ o → fs′ (kpath o) ≡ desired o) kp ×
                Σ FS (λ fs″ → (kapply kp fs′ ≡ just fs″) × (fs″ ≗ fs′))
  kapply-idem kp fn fs {fs′} e with kapply-refines kp fs e
  ... | _ , refl =
        done ,
        apply (desired-plan kp) fs′ ,
        decide-accepts kp fs′ (every-ready? fs′ kp) (every-map kp (every-intro kp (λ o d → inj₁ d)) done) ,
        apply-idem (desired-plan kp) fs
    where
      done : Every (λ o → apply (desired-plan kp) fs (kpath o) ≡ desired o) kp
      done = every-map kp (every-intro kp (λ o l → apply-establishes (desired-plan kp) fs l)) fn
