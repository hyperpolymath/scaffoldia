// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! The closed list of kernel refusal codes (plan format v0.1 specification, "Refusal
//! codes"). Planner-only codes are deliberately absent: a kernel never raises
//! them.

use crate::json::{canonical, Json};

/// A refusal code a kernel can raise.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Code {
    /// Not a v0.1 plan, or not JCS.
    Malformed,
    /// A plan version this kernel does not implement.
    PlanVersion,
    /// An input without a full commit id, or an op naming no input.
    SourceUnpinned,
    /// A path that is not safe.
    PathUnsafe,
    /// Two operations on one path, or ops not sorted.
    PathDuplicate,
    /// Case-fold collision, or a file/directory prefix clash.
    PathCollision,
    /// Blob absent.
    BlobMissing,
    /// Blob present with the wrong hash.
    BlobMismatch,
    /// Apply was asked for a plan with holes.
    OpenPlan,
    /// A hole has no answer.
    HoleUnfilled,
    /// An answer names no hole.
    HoleUnknown,
    /// A `{{...}}` token survived instantiation.
    Residue,
    /// An `expect` does not hold on the target.
    Precondition,
    /// A symbolic link on the way to (or at) a planned path.
    TargetSymlink,
    /// The target is missing or not a directory.
    TargetInvalid,
}

impl Code {
    /// The wire spelling of the code.
    pub fn as_str(self) -> &'static str {
        match self {
            Code::Malformed => "MALFORMED",
            Code::PlanVersion => "PLAN_VERSION",
            Code::SourceUnpinned => "SOURCE_UNPINNED",
            Code::PathUnsafe => "PATH_UNSAFE",
            Code::PathDuplicate => "PATH_DUPLICATE",
            Code::PathCollision => "PATH_COLLISION",
            Code::BlobMissing => "BLOB_MISSING",
            Code::BlobMismatch => "BLOB_MISMATCH",
            Code::OpenPlan => "OPEN_PLAN",
            Code::HoleUnfilled => "HOLE_UNFILLED",
            Code::HoleUnknown => "HOLE_UNKNOWN",
            Code::Residue => "RESIDUE",
            Code::Precondition => "PRECONDITION",
            Code::TargetSymlink => "TARGET_SYMLINK",
            Code::TargetInvalid => "TARGET_INVALID",
        }
    }
}

/// A refusal: a correct, expected outcome, not an error.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Refusal {
    /// The code.
    pub code: Code,
    /// Human-readable detail (not part of the wire contract).
    pub detail: String,
}

impl Refusal {
    /// Build a refusal.
    pub fn new(code: Code, detail: impl Into<String>) -> Self {
        Refusal {
            code,
            detail: detail.into(),
        }
    }

    /// The wire form: `{"detail":"...","refused":"<CODE>"}` in JCS, no LF.
    pub fn to_jcs(&self) -> String {
        canonical(&Json::Obj(vec![
            (
                "refused".to_string(),
                Json::Str(self.code.as_str().to_string()),
            ),
            ("detail".to_string(), Json::Str(self.detail.clone())),
        ]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wire_form_is_sorted_and_escaped() {
        let r = Refusal::new(Code::PathUnsafe, "bad \"a\u{1}b\"");
        assert_eq!(
            r.to_jcs(),
            "{\"detail\":\"bad \\\"a\\u0001b\\\"\",\"refused\":\"PATH_UNSAFE\"}"
        );
    }
}
