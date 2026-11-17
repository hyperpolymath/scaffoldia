// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! Safe paths (plan format v0.1 specification, "Safe paths").
//!
//! A path is safe when: it is non-empty and relative; its segments are
//! separated by `/`; no segment is empty, `.` or `..`; it contains no
//! backslash and no control character (NUL, C0, DEL, C1); it is in Unicode
//! NFC; and no segment is `.git` in any ASCII case (the README says "first
//! segment is not `.git`"; this kernel refuses `.git` at any depth and in any
//! case; a stricter reading than the specification text).
//!
//! A safe path, joined to a target directory, names an entry strictly inside
//! that directory: it cannot be absolute, cannot climb (`..`), and cannot be
//! re-interpreted by a separator other than `/`.

use crate::Prims;

/// Why a path is not safe. The first applicable fault in this order is
/// reported: empty, absolute, backslash, control, then per segment (empty,
/// `.`, `..`, `.git`), then NFC.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PathFault {
    /// The empty string.
    Empty,
    /// Starts with `/`.
    Absolute,
    /// Contains `\`.
    Backslash,
    /// Contains a control character.
    Control(char),
    /// Has an empty segment (`a//b`, trailing `/`).
    EmptySegment,
    /// Has a `.` segment.
    DotSegment,
    /// Has a `..` segment.
    DotDotSegment,
    /// Has a segment equal to `.git` ignoring ASCII case.
    GitDir,
    /// Not in Unicode NFC.
    NotNfc,
}

impl PathFault {
    /// A short English description.
    pub fn describe(&self) -> String {
        match self {
            PathFault::Empty => "empty path".to_string(),
            PathFault::Absolute => "absolute path".to_string(),
            PathFault::Backslash => "contains a backslash".to_string(),
            PathFault::Control(c) => format!("contains control character U+{:04X}", *c as u32),
            PathFault::EmptySegment => "has an empty segment".to_string(),
            PathFault::DotSegment => "has a '.' segment".to_string(),
            PathFault::DotDotSegment => "has a '..' segment".to_string(),
            PathFault::GitDir => "has a '.git' segment".to_string(),
            PathFault::NotNfc => "is not in Unicode NFC".to_string(),
        }
    }
}

/// `true` for NUL, C0 controls, DEL and C1 controls (Unicode category Cc).
pub fn is_control(c: char) -> bool {
    let v = c as u32;
    v < 0x20 || (0x7f..=0x9f).contains(&v)
}

/// The first fault of `p`, or `None` when `p` is safe.
pub fn path_fault<P: Prims + ?Sized>(p: &str, prims: &P) -> Option<PathFault> {
    if p.is_empty() {
        return Some(PathFault::Empty);
    }
    if p.starts_with('/') {
        return Some(PathFault::Absolute);
    }
    if p.contains('\\') {
        return Some(PathFault::Backslash);
    }
    if let Some(c) = p.chars().find(|c| is_control(*c)) {
        return Some(PathFault::Control(c));
    }
    for seg in p.split('/') {
        if seg.is_empty() {
            return Some(PathFault::EmptySegment);
        }
        if seg == "." {
            return Some(PathFault::DotSegment);
        }
        if seg == ".." {
            return Some(PathFault::DotDotSegment);
        }
        if seg.eq_ignore_ascii_case(".git") {
            return Some(PathFault::GitDir);
        }
    }
    if !prims.is_nfc(p) {
        return Some(PathFault::NotNfc);
    }
    None
}

/// The collision key of a path: an approximation of Unicode full case folding
/// built from `std` only (`lower(upper(lower(s)))`). It maps `ß`, `ẞ` and
/// `SS` to `ss` and `K` (Kelvin) to `k`. It is not proved equal to
/// `CaseFolding.txt`; where it differs it should only merge more strings
/// (refuse more), which is the safe direction. Listed as ASSUMED in the proof
/// plan.
pub fn fold(p: &str) -> String {
    p.to_lowercase().to_uppercase().to_lowercase()
}

/// The proper directory prefixes of `p`, shortest first:
/// `a/b/c` gives `a`, `a/b`.
pub fn ancestors(p: &str) -> Vec<&str> {
    p.match_indices('/').map(|(i, _)| &p[..i]).collect()
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    /// Test primitives: NFC is approximated as "no combining acute accent",
    /// enough for the unit tests; the real oracle lives in the edge crate.
    pub struct TestPrims;
    impl Prims for TestPrims {
        fn is_nfc(&self, s: &str) -> bool {
            !s.contains('\u{301}')
        }
        fn sha256_hex(&self, bytes: &[u8]) -> String {
            // Not SHA-256: a stable, injective-enough stand-in for tests.
            let mut h: u64 = 0xcbf29ce484222325;
            for b in bytes {
                h ^= u64::from(*b);
                h = h.wrapping_mul(0x100000001b3);
            }
            format!("{:064x}", h)
        }
    }

    #[test]
    fn safe_paths() {
        for p in [
            "README.adoc",
            "docs/a.adoc",
            ".github/workflows/x.yml",
            "café.txt",
            ".gitignore",
            "a/.github",
        ] {
            assert_eq!(path_fault(p, &TestPrims), None, "{p}");
        }
    }

    #[test]
    fn unsafe_paths() {
        let cases: &[(&str, PathFault)] = &[
            ("", PathFault::Empty),
            ("/etc/passwd", PathFault::Absolute),
            ("a\\b", PathFault::Backslash),
            ("a\u{0}b", PathFault::Control('\u{0}')),
            ("a\u{1}b", PathFault::Control('\u{1}')),
            ("a\u{7f}", PathFault::Control('\u{7f}')),
            ("a\u{85}", PathFault::Control('\u{85}')),
            ("a//b", PathFault::EmptySegment),
            ("a/", PathFault::EmptySegment),
            ("./a", PathFault::DotSegment),
            ("../a", PathFault::DotDotSegment),
            ("a/../../b", PathFault::DotDotSegment),
            (".git/config", PathFault::GitDir),
            (".GIT/config", PathFault::GitDir),
            ("sub/.git/hooks/pre-commit", PathFault::GitDir),
            ("cafe\u{301}.txt", PathFault::NotNfc),
        ];
        for (p, f) in cases {
            assert_eq!(path_fault(p, &TestPrims).as_ref(), Some(f), "{p:?}");
        }
    }

    #[test]
    fn folding() {
        assert_eq!(fold("README.adoc"), fold("readme.ADOC"));
        assert_eq!(fold("stra\u{df}e"), fold("STRASSE"));
        assert_eq!(fold("\u{1e9e}"), fold("ss"));
        assert_eq!(fold("\u{212a}"), fold("k"));
        assert_ne!(fold("a"), fold("b"));
    }

    #[test]
    fn ancestor_list() {
        assert_eq!(ancestors("a/b/c"), vec!["a", "a/b"]);
        assert!(ancestors("a").is_empty());
    }
}
