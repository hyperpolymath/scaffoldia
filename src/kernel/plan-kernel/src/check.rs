// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! Plan-level checks: closed, safe, blobs.
//!
//! [`check_plan`] is the whole "Check the plan" step of Apply (plan format v0.1
//! specification): well formed, closed (when required), safe, in that order.
//! [`check_blobs`] then needs the edge to have looked at the blob directory.

use std::collections::{BTreeMap, BTreeSet};

use crate::json::Json;
use crate::path::{ancestors, fold, path_fault};
use crate::plan::{parse_plan, Action, Plan};
use crate::refusal::{Code, Refusal};
use crate::Prims;

/// Path safety of a plan: every path safe; ops strictly sorted bytewise (so
/// at most one per path); no two paths equal after case folding; no path a
/// proper directory prefix of another (also after folding).
pub fn check_paths<P: Prims + ?Sized>(plan: &Plan, prims: &P) -> Result<(), Refusal> {
    for (i, op) in plan.ops.iter().enumerate() {
        if let Some(f) = path_fault(&op.path, prims) {
            return Err(Refusal::new(
                Code::PathUnsafe,
                format!("ops[{i}].path {}", f.describe()),
            ));
        }
    }
    for i in 1..plan.ops.len() {
        let (a, b) = (&plan.ops[i - 1].path, &plan.ops[i].path);
        match a.as_bytes().cmp(b.as_bytes()) {
            std::cmp::Ordering::Less => {}
            std::cmp::Ordering::Equal => {
                return Err(Refusal::new(
                    Code::PathDuplicate,
                    format!("ops[{}] and ops[{i}] name the same path", i - 1),
                ))
            }
            std::cmp::Ordering::Greater => {
                return Err(Refusal::new(
                    Code::PathDuplicate,
                    format!("ops[{i}] is not sorted after ops[{}]", i - 1),
                ))
            }
        }
    }
    let mut keys: BTreeMap<String, usize> = BTreeMap::new();
    for (i, op) in plan.ops.iter().enumerate() {
        if let Some(j) = keys.insert(fold(&op.path), i) {
            return Err(Refusal::new(
                Code::PathCollision,
                format!("ops[{j}] and ops[{i}] are equal after case folding"),
            ));
        }
    }
    let set: BTreeSet<&String> = keys.keys().collect();
    for (i, op) in plan.ops.iter().enumerate() {
        let k = fold(&op.path);
        for a in ancestors(&k) {
            if set.contains(&a.to_string()) {
                return Err(Refusal::new(
                    Code::PathCollision,
                    format!("ops[{i}].path lies under another planned path (file/directory clash)"),
                ));
            }
        }
    }
    Ok(())
}

/// Well formed, then closed (when `require_closed`), then safe.
pub fn check_plan<P: Prims + ?Sized>(
    doc: &Json,
    prims: &P,
    require_closed: bool,
) -> Result<Plan, Refusal> {
    let plan = parse_plan(doc)?;
    if require_closed && !plan.holes.is_empty() {
        return Err(Refusal::new(
            Code::OpenPlan,
            format!(
                "plan has {} open hole(s); instantiate it first",
                plan.holes.len()
            ),
        ));
    }
    check_paths(&plan, prims)?;
    Ok(plan)
}

/// What the edge found at `blobs/sha256/<hex>`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BlobObs {
    /// No regular file there.
    Missing,
    /// A file whose content has this SHA-256 (64 lowercase hex).
    Present(String),
}

/// Distinct blob digests the plan writes, in first-use order.
pub fn blobs_needed(plan: &Plan) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for op in &plan.ops {
        if let Action::Write { blob, .. } = &op.action {
            if !out.contains(blob) {
                out.push(blob.clone());
            }
        }
    }
    out
}

/// Every written blob is present with the right hash.
pub fn check_blobs(plan: &Plan, found: &BTreeMap<String, BlobObs>) -> Result<(), Refusal> {
    for (i, op) in plan.ops.iter().enumerate() {
        if let Action::Write { blob, .. } = &op.action {
            match found.get(blob) {
                None | Some(BlobObs::Missing) => {
                    return Err(Refusal::new(
                        Code::BlobMissing,
                        format!("ops[{i}] blob sha256:{blob} is absent"),
                    ))
                }
                Some(BlobObs::Present(h)) if h == blob => {}
                Some(BlobObs::Present(_)) => {
                    return Err(Refusal::new(
                        Code::BlobMismatch,
                        format!("ops[{i}] blob sha256:{blob} has different content"),
                    ))
                }
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::path::tests::TestPrims;
    use crate::plan::tests::{plan_doc, write_op};

    fn code(paths: &[&str]) -> Option<Code> {
        let doc = plan_doc(paths.iter().map(|p| write_op(p)).collect());
        check_plan(&doc, &TestPrims, true).err().map(|r| r.code)
    }

    #[test]
    fn path_rules() {
        assert_eq!(code(&["a", "b/c", "d"]), None);
        assert_eq!(code(&["../a"]), Some(Code::PathUnsafe));
        assert_eq!(code(&["a", "a"]), Some(Code::PathDuplicate));
        assert_eq!(code(&["b", "a"]), Some(Code::PathDuplicate));
        assert_eq!(code(&["README", "readme"]), Some(Code::PathCollision));
        assert_eq!(code(&["docs", "docs/a"]), Some(Code::PathCollision));
        assert_eq!(code(&["DOCS", "docs/a"]), Some(Code::PathCollision));
        // `docs-x` sorts between `docs` and `docs/`, but is not a prefix clash.
        assert_eq!(code(&["docs-x", "docs/a"]), None);
    }

    #[test]
    fn blob_rules() {
        let doc = plan_doc(vec![write_op("a")]);
        let p = check_plan(&doc, &TestPrims, true).unwrap();
        let h = blobs_needed(&p)[0].clone();
        let mut m = BTreeMap::new();
        assert_eq!(check_blobs(&p, &m).unwrap_err().code, Code::BlobMissing);
        m.insert(h.clone(), BlobObs::Present("0".repeat(64)));
        assert_eq!(check_blobs(&p, &m).unwrap_err().code, Code::BlobMismatch);
        m.insert(h.clone(), BlobObs::Present(h));
        assert!(check_blobs(&p, &m).is_ok());
    }
}
