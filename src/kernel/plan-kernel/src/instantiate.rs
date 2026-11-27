// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! Instantiate: open plan to closed plan (plan format v0.1 specification, "Instantiate").
//!
//! A *token* is `{{` NAME `}}` with NAME matching `[A-Z][A-Z0-9_]*` — the hole
//! name syntax, nothing looser. So `${{ github.sha }}` (GitHub Actions) and
//! `{{project}}` (lowercase) are not tokens and are left alone; `{{OWNER}}`
//! is a token wherever it appears, including in a Justfile (see
//! the specification's open question on delimiter collisions).
//!
//! Substitution is one left-to-right pass over the original blob: a value is
//! never rescanned, so an answer cannot expand another answer. Residue is then
//! looked for in the *result*, so a token that an answer introduces, or that
//! substitution assembles from adjacent text, is still refused.

use std::collections::BTreeMap;

use crate::plan::{is_hole_name, Action, Plan};
use crate::refusal::{Code, Refusal};
use crate::Prims;

/// Every hole has an answer (`HOLE_UNFILLED`), and every answer names a hole
/// (`HOLE_UNKNOWN`), checked in that order.
pub fn check_answers(holes: &[String], answers: &BTreeMap<String, String>) -> Result<(), Refusal> {
    for h in holes {
        if !answers.contains_key(h) {
            return Err(Refusal::new(
                Code::HoleUnfilled,
                format!("hole {h} has no answer"),
            ));
        }
    }
    for k in answers.keys() {
        if !holes.contains(k) {
            return Err(Refusal::new(
                Code::HoleUnknown,
                "an answer names no hole of this plan".to_string(),
            ));
        }
    }
    Ok(())
}

/// If a token starts at `b[i]`, its name and the index just past `}}`.
fn token_at(b: &[u8], i: usize) -> Option<(&str, usize)> {
    if !b[i..].starts_with(b"{{") {
        return None;
    }
    let start = i + 2;
    let mut end = start;
    while end < b.len()
        && (b[end].is_ascii_uppercase() || b[end].is_ascii_digit() || b[end] == b'_')
    {
        end += 1;
    }
    if !b[end..].starts_with(b"}}") {
        return None;
    }
    let name = std::str::from_utf8(&b[start..end]).ok()?;
    if is_hole_name(name) {
        Some((name, end + 2))
    } else {
        None
    }
}

/// Replace `{{NAME}}` by `answers[NAME]` for the given names only.
pub fn substitute(blob: &[u8], answers: &BTreeMap<String, String>) -> Vec<u8> {
    let mut out = Vec::with_capacity(blob.len());
    let mut i = 0;
    while i < blob.len() {
        if let Some((name, next)) = token_at(blob, i) {
            if let Some(v) = answers.get(name) {
                out.extend_from_slice(v.as_bytes());
                i = next;
                continue;
            }
        }
        out.push(blob[i]);
        i += 1;
    }
    out
}

/// The first token left in `blob`, if any.
pub fn find_residue(blob: &[u8]) -> Option<String> {
    (0..blob.len()).find_map(|i| token_at(blob, i).map(|(n, _)| n.to_string()))
}

/// A closed plan and the blobs it needs, keyed by digest (64 lowercase hex).
pub struct Instantiated {
    /// The closed plan (holes empty, blob digests recomputed).
    pub plan: Plan,
    /// Content of every blob the closed plan writes.
    pub blobs: BTreeMap<String, Vec<u8>>,
}

/// Instantiate a checked plan. `blobs` must hold every blob the plan writes
/// (the caller has run `check::check_blobs`); a missing one is `BLOB_MISSING`.
pub fn instantiate<P: Prims + ?Sized>(
    plan: &Plan,
    answers: &BTreeMap<String, String>,
    blobs: &BTreeMap<String, Vec<u8>>,
    prims: &P,
) -> Result<Instantiated, Refusal> {
    check_answers(&plan.holes, answers)?;
    let mut out = plan.clone();
    out.holes.clear();
    let mut out_blobs = BTreeMap::new();
    for (i, op) in out.ops.iter_mut().enumerate() {
        if let Action::Write { blob, .. } = &mut op.action {
            let Some(content) = blobs.get(blob.as_str()) else {
                return Err(Refusal::new(
                    Code::BlobMissing,
                    format!("ops[{i}] blob sha256:{blob} is absent"),
                ));
            };
            let filled = substitute(content, answers);
            if let Some(name) = find_residue(&filled) {
                return Err(Refusal::new(
                    Code::Residue,
                    format!("ops[{i}]: token {{{{{name}}}}} remains after instantiation"),
                ));
            }
            let h = prims.sha256_hex(&filled);
            *blob = h.clone();
            out_blobs.insert(h, filled);
        }
    }
    Ok(Instantiated {
        plan: out,
        blobs: out_blobs,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn answers(kv: &[(&str, &str)]) -> BTreeMap<String, String> {
        kv.iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect()
    }

    #[test]
    fn substitution_is_single_pass() {
        let a = answers(&[("OWNER", "{{REPO}}"), ("REPO", "r")]);
        assert_eq!(
            substitute(b"{{OWNER}}/{{REPO}}", &a),
            b"{{REPO}}/r".to_vec()
        );
        assert_eq!(
            find_residue(&substitute(b"{{OWNER}}", &a)),
            Some("REPO".to_string())
        );
    }

    #[test]
    fn foreign_template_syntax_is_not_a_token() {
        let a = answers(&[("OWNER", "o")]);
        let text = b"${{ github.sha }} {{project}} {{ OWNER }} {{{OWNER}}";
        assert_eq!(
            substitute(text, &a),
            b"${{ github.sha }} {{project}} {{ OWNER }} {o".to_vec()
        );
        assert_eq!(find_residue(b"${{ github.sha }} {{project}}"), None);
        assert_eq!(find_residue(b"x {{OTHER}} y"), Some("OTHER".to_string()));
    }

    #[test]
    fn answers_match_holes() {
        let holes = vec!["OWNER".to_string(), "REPO".to_string()];
        assert_eq!(
            check_answers(&holes, &answers(&[("OWNER", "o")]))
                .unwrap_err()
                .code,
            Code::HoleUnfilled
        );
        assert_eq!(
            check_answers(
                &holes,
                &answers(&[("OWNER", "o"), ("REPO", "r"), ("X", "x")])
            )
            .unwrap_err()
            .code,
            Code::HoleUnknown
        );
        assert!(check_answers(&holes, &answers(&[("OWNER", "o"), ("REPO", "r")])).is_ok());
    }
}
