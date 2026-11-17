// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! Pure decision core of a file-plan kernel (plan format v0.1).
//!
//! A *plan* is data: a sorted list of file operations with preconditions and
//! provenance, content held by SHA-256. A *kernel* checks that a plan is well
//! formed and safe, then decides, against an observed target directory,
//! either a refusal or the exact list of effects. See this crate's README for
//! where the plan format v0.1 text lives.
//!
//! Everything here is a total function over already-parsed data: no I/O, no
//! clock, no randomness, no dependency beyond `std`. The two primitives that
//! need large tables or a cryptographic implementation (Unicode NFC and
//! SHA-256) are injected through [`Prims`]; for verification they are
//! `ASSUMED` boundary specifications (see `PROOF-PLAN.adoc`).
//!
//! The pipeline the edge drives is:
//!
//! 1. [`json::is_j1`] — the bytes are the canonical (JCS) form plus one LF;
//! 2. [`plan::parse_plan`] — well-formedness, version, pins;
//! 3. [`check::check_paths`] — path safety, order, uniqueness, collisions;
//! 4. [`check::check_blobs`] — every blob present with the right hash;
//! 5. [`apply::decide`] — refuse, or the exact list of effects;
//!    or [`instantiate::instantiate`] — open plan to closed plan.

#![forbid(unsafe_code)]

pub mod apply;
pub mod check;
pub mod instantiate;
pub mod json;
pub mod model;
pub mod path;
pub mod plan;
pub mod refusal;

/// Primitives the core needs but does not implement. The edge provides them.
pub trait Prims {
    /// `true` when `s` is in Unicode Normalization Form C.
    fn is_nfc(&self, s: &str) -> bool;
    /// Lowercase hexadecimal SHA-256 of `bytes` (64 characters).
    fn sha256_hex(&self, bytes: &[u8]) -> String;
}
