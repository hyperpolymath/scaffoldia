// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! Plan format v0.1: typed form and well-formedness.
//!
//! [`parse_plan`] is closed-world: an unknown member anywhere is `MALFORMED`,
//! because a kernel must not silently ignore a field it does not understand.
//! Order of refusals: structure (`MALFORMED`, with `PLAN_VERSION` as soon as
//! the version is read), then pins (`SOURCE_UNPINNED`).

use crate::json::{canonical, Json};
use crate::refusal::{Code, Refusal};

/// The only plan version this kernel implements.
pub const PLAN_VERSION: &str = "0.1";

/// A pinned source.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Input {
    /// Name operations refer to.
    pub name: String,
    /// `owner/name` on the forge.
    pub repo: String,
    /// Full commit id (40 lowercase hex).
    pub rev: String,
}

/// Provenance of an operation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Source {
    /// Name of an input.
    pub input: String,
    /// Path inside that input.
    pub path: String,
}

/// File mode of a write.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    /// `644`
    Regular,
    /// `755`
    Executable,
}

impl Mode {
    /// The wire spelling.
    pub fn as_str(self) -> &'static str {
        match self {
            Mode::Regular => "644",
            Mode::Executable => "755",
        }
    }
    /// `true` for `755`.
    pub fn is_exec(self) -> bool {
        self == Mode::Executable
    }
}

/// Precondition of a write.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expect {
    /// Nothing is at the path.
    Absent,
    /// A regular file with this SHA-256 (64 lowercase hex) is at the path.
    Sha256(String),
}

/// What an operation does.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Action {
    /// Write blob `blob` (64 lowercase hex, without the `sha256:` prefix).
    Write {
        /// Blob digest.
        blob: String,
        /// File mode.
        mode: Mode,
        /// Precondition.
        expect: Expect,
    },
    /// Delete a regular file whose content has this SHA-256.
    Delete {
        /// Precondition (never "absent").
        expect: String,
    },
}

/// One operation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Op {
    /// Target-relative path.
    pub path: String,
    /// What to do.
    pub action: Action,
    /// Provenance.
    pub source: Source,
    /// Authorised replacement, when an input contract names one.
    pub replaces: Option<Source>,
}

/// A parsed plan.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Plan {
    /// Open holes (empty: closed plan).
    pub holes: Vec<String>,
    /// Pinned inputs.
    pub inputs: Vec<Input>,
    /// Operations, in file order.
    pub ops: Vec<Op>,
}

/// `true` when `s` is exactly `n` lowercase hexadecimal digits.
pub fn is_lower_hex(s: &str, n: usize) -> bool {
    s.len() == n
        && s.bytes()
            .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
}

/// `true` when `s` matches `[A-Z][A-Z0-9_]*`.
pub fn is_hole_name(s: &str) -> bool {
    let mut it = s.bytes();
    match it.next() {
        Some(b) if b.is_ascii_uppercase() => {}
        _ => return false,
    }
    it.all(|b| b.is_ascii_uppercase() || b.is_ascii_digit() || b == b'_')
}

fn malformed(detail: impl Into<String>) -> Refusal {
    Refusal::new(Code::Malformed, detail)
}

/// Members of an object, checking that every required key is present and no
/// key outside `required ∪ optional` occurs.
fn members<'a>(
    v: &'a Json,
    what: &str,
    required: &[&str],
    optional: &[&str],
) -> Result<&'a [(String, Json)], Refusal> {
    let Json::Obj(ms) = v else {
        return Err(malformed(format!("{what} is not an object")));
    };
    for (k, _) in ms {
        if !required.contains(&k.as_str()) && !optional.contains(&k.as_str()) {
            return Err(malformed(format!(
                "{what} has unknown member {}",
                canonical(&Json::Str(k.clone()))
            )));
        }
    }
    for r in required {
        if !ms.iter().any(|(k, _)| k == r) {
            return Err(malformed(format!("{what} is missing member \"{r}\"")));
        }
    }
    Ok(ms)
}

fn string(v: &Json, what: &str) -> Result<String, Refusal> {
    match v {
        Json::Str(s) => Ok(s.clone()),
        _ => Err(malformed(format!("{what} is not a string"))),
    }
}

fn field<'a>(ms: &'a [(String, Json)], key: &str) -> &'a Json {
    // Callers only ask for keys `members` proved present.
    ms.iter()
        .find(|(k, _)| k == key)
        .map(|(_, v)| v)
        .unwrap_or(&Json::Null)
}

fn source(v: &Json, what: &str) -> Result<Source, Refusal> {
    let ms = members(v, what, &["input", "path"], &[])?;
    Ok(Source {
        input: string(field(ms, "input"), &format!("{what}.input"))?,
        path: string(field(ms, "path"), &format!("{what}.path"))?,
    })
}

fn digest_field(v: &Json, what: &str) -> Result<String, Refusal> {
    let ms = members(v, what, &["sha256"], &[])?;
    let h = string(field(ms, "sha256"), &format!("{what}.sha256"))?;
    if !is_lower_hex(&h, 64) {
        return Err(malformed(format!(
            "{what}.sha256 is not 64 lowercase hex digits"
        )));
    }
    Ok(h)
}

fn parse_op(v: &Json, i: usize) -> Result<Op, Refusal> {
    let what = format!("ops[{i}]");
    let Json::Obj(_) = v else {
        return Err(malformed(format!("{what} is not an object")));
    };
    let kind = match v.get("op") {
        Some(Json::Str(k)) => k.clone(),
        Some(_) => return Err(malformed(format!("{what}.op is not a string"))),
        None => return Err(malformed(format!("{what} is missing member \"op\""))),
    };
    let (ms, action) = match kind.as_str() {
        "write" => {
            let ms = members(
                v,
                &what,
                &["blob", "expect", "mode", "op", "path", "source"],
                &["replaces"],
            )?;
            let blob = string(field(ms, "blob"), &format!("{what}.blob"))?;
            let blob = match blob.strip_prefix("sha256:") {
                Some(h) if is_lower_hex(h, 64) => h.to_string(),
                _ => {
                    return Err(malformed(format!(
                        "{what}.blob is not sha256:<64 lowercase hex>"
                    )))
                }
            };
            let mode = match string(field(ms, "mode"), &format!("{what}.mode"))?.as_str() {
                "644" => Mode::Regular,
                "755" => Mode::Executable,
                _ => return Err(malformed(format!("{what}.mode is not \"644\" or \"755\""))),
            };
            let expect = match field(ms, "expect") {
                Json::Str(s) if s == "absent" => Expect::Absent,
                e @ Json::Obj(_) => Expect::Sha256(digest_field(e, &format!("{what}.expect"))?),
                _ => {
                    return Err(malformed(format!(
                        "{what}.expect is neither \"absent\" nor {{\"sha256\":...}}"
                    )))
                }
            };
            (ms, Action::Write { blob, mode, expect })
        }
        "delete" => {
            let ms = members(v, &what, &["expect", "op", "path", "source"], &["replaces"])?;
            let expect = match field(ms, "expect") {
                e @ Json::Obj(_) => digest_field(e, &format!("{what}.expect"))?,
                _ => {
                    return Err(malformed(format!(
                        "{what}.expect of a delete must be {{\"sha256\":...}}"
                    )))
                }
            };
            (ms, Action::Delete { expect })
        }
        other => {
            return Err(malformed(format!(
                "{what}.op {} is not \"write\" or \"delete\"",
                canonical(&Json::Str(other.to_string()))
            )))
        }
    };
    let path = string(field(ms, "path"), &format!("{what}.path"))?;
    let src = source(field(ms, "source"), &format!("{what}.source"))?;
    let replaces = match ms.iter().find(|(k, _)| k == "replaces") {
        Some((_, r)) => Some(source(r, &format!("{what}.replaces"))?),
        None => None,
    };
    Ok(Op {
        path,
        action,
        source: src,
        replaces,
    })
}

/// Parse and check a plan document. Does not check paths or blobs.
pub fn parse_plan(doc: &Json) -> Result<Plan, Refusal> {
    let Json::Obj(_) = doc else {
        return Err(malformed("plan is not an object"));
    };
    match doc.get("plan_version") {
        None => return Err(malformed("plan is missing member \"plan_version\"")),
        Some(Json::Str(v)) if v == PLAN_VERSION => {}
        Some(Json::Str(v)) => {
            return Err(Refusal::new(
                Code::PlanVersion,
                format!(
                    "plan_version {} is not {PLAN_VERSION}",
                    canonical(&Json::Str(v.clone()))
                ),
            ))
        }
        Some(_) => return Err(malformed("plan_version is not a string")),
    }
    let ms = members(
        doc,
        "plan",
        &["holes", "inputs", "ops", "plan_version"],
        &[],
    )?;

    let Json::Arr(hs) = field(ms, "holes") else {
        return Err(malformed("holes is not an array"));
    };
    let mut holes = Vec::new();
    for (i, h) in hs.iter().enumerate() {
        let h = string(h, &format!("holes[{i}]"))?;
        if !is_hole_name(&h) {
            return Err(malformed(format!(
                "holes[{i}] does not match [A-Z][A-Z0-9_]*"
            )));
        }
        if holes.contains(&h) {
            return Err(malformed(format!("hole {h} is declared twice")));
        }
        holes.push(h);
    }

    let Json::Arr(is) = field(ms, "inputs") else {
        return Err(malformed("inputs is not an array"));
    };
    let mut inputs: Vec<Input> = Vec::new();
    for (i, v) in is.iter().enumerate() {
        let what = format!("inputs[{i}]");
        let ims = members(v, &what, &["name", "repo", "rev"], &[])?;
        let input = Input {
            name: string(field(ims, "name"), &format!("{what}.name"))?,
            repo: string(field(ims, "repo"), &format!("{what}.repo"))?,
            rev: string(field(ims, "rev"), &format!("{what}.rev"))?,
        };
        if input.name.is_empty() || input.repo.is_empty() {
            return Err(malformed(format!("{what} has an empty name or repo")));
        }
        if inputs.iter().any(|x| x.name == input.name) {
            return Err(malformed(format!("{what}.name is declared twice")));
        }
        inputs.push(input);
    }

    let Json::Arr(os) = field(ms, "ops") else {
        return Err(malformed("ops is not an array"));
    };
    let ops = os
        .iter()
        .enumerate()
        .map(|(i, v)| parse_op(v, i))
        .collect::<Result<Vec<_>, _>>()?;

    // Pins: every input carries a full commit id; every op names an input.
    for (i, input) in inputs.iter().enumerate() {
        if !is_lower_hex(&input.rev, 40) {
            return Err(Refusal::new(
                Code::SourceUnpinned,
                format!("inputs[{i}].rev is not a full commit id (40 lowercase hex)"),
            ));
        }
    }
    for (i, op) in ops.iter().enumerate() {
        for (role, s) in [
            ("source", Some(&op.source)),
            ("replaces", op.replaces.as_ref()),
        ] {
            if let Some(s) = s {
                if !inputs.iter().any(|x| x.name == s.input) {
                    return Err(Refusal::new(
                        Code::SourceUnpinned,
                        format!("ops[{i}].{role}.input names no declared input"),
                    ));
                }
            }
        }
    }
    Ok(Plan { holes, inputs, ops })
}

/// The JSON document of a plan (for writing an instantiated plan).
pub fn plan_to_json(p: &Plan) -> Json {
    fn src(s: &Source) -> Json {
        Json::Obj(vec![
            ("input".into(), Json::Str(s.input.clone())),
            ("path".into(), Json::Str(s.path.clone())),
        ])
    }
    let ops = p
        .ops
        .iter()
        .map(|op| {
            let mut ms = vec![
                ("path".to_string(), Json::Str(op.path.clone())),
                ("source".to_string(), src(&op.source)),
            ];
            if let Some(r) = &op.replaces {
                ms.push(("replaces".into(), src(r)));
            }
            match &op.action {
                Action::Write { blob, mode, expect } => {
                    ms.push(("op".into(), Json::Str("write".into())));
                    ms.push(("blob".into(), Json::Str(format!("sha256:{blob}"))));
                    ms.push(("mode".into(), Json::Str(mode.as_str().into())));
                    ms.push((
                        "expect".into(),
                        match expect {
                            Expect::Absent => Json::Str("absent".into()),
                            Expect::Sha256(h) => {
                                Json::Obj(vec![("sha256".into(), Json::Str(h.clone()))])
                            }
                        },
                    ));
                }
                Action::Delete { expect } => {
                    ms.push(("op".into(), Json::Str("delete".into())));
                    ms.push((
                        "expect".into(),
                        Json::Obj(vec![("sha256".into(), Json::Str(expect.clone()))]),
                    ));
                }
            }
            Json::Obj(ms)
        })
        .collect();
    Json::Obj(vec![
        (
            "holes".into(),
            Json::Arr(p.holes.iter().map(|h| Json::Str(h.clone())).collect()),
        ),
        (
            "inputs".into(),
            Json::Arr(
                p.inputs
                    .iter()
                    .map(|i| {
                        Json::Obj(vec![
                            ("name".into(), Json::Str(i.name.clone())),
                            ("repo".into(), Json::Str(i.repo.clone())),
                            ("rev".into(), Json::Str(i.rev.clone())),
                        ])
                    })
                    .collect(),
            ),
        ),
        ("ops".into(), Json::Arr(ops)),
        ("plan_version".into(), Json::Str(PLAN_VERSION.into())),
    ])
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    pub const REV: &str = "0123456789abcdef0123456789abcdef01234567";
    pub const H1: &str = "1111111111111111111111111111111111111111111111111111111111111111";

    pub fn s(x: &str) -> Json {
        Json::Str(x.to_string())
    }
    pub fn obj(ms: &[(&str, Json)]) -> Json {
        Json::Obj(ms.iter().map(|(k, v)| (k.to_string(), v.clone())).collect())
    }
    pub fn write_op(path: &str) -> Json {
        obj(&[
            ("op", s("write")),
            ("path", s(path)),
            ("blob", s(&format!("sha256:{H1}"))),
            ("mode", s("644")),
            ("expect", s("absent")),
            ("source", obj(&[("input", s("spine")), ("path", s(path))])),
        ])
    }
    pub fn plan_doc(ops: Vec<Json>) -> Json {
        obj(&[
            ("plan_version", s("0.1")),
            ("holes", Json::Arr(vec![])),
            (
                "inputs",
                Json::Arr(vec![obj(&[
                    ("name", s("spine")),
                    ("repo", s("o/r")),
                    ("rev", s(REV)),
                ])]),
            ),
            ("ops", Json::Arr(ops)),
        ])
    }

    fn code(doc: &Json) -> Option<Code> {
        parse_plan(doc).err().map(|r| r.code)
    }

    #[test]
    fn minimal_plan_parses_and_round_trips() {
        let doc = plan_doc(vec![write_op("README.adoc")]);
        let p = parse_plan(&doc).unwrap();
        assert_eq!(p.ops.len(), 1);
        assert_eq!(canonical(&plan_to_json(&p)), canonical(&doc));
    }

    #[test]
    fn refusals() {
        assert_eq!(code(&s("x")), Some(Code::Malformed));
        let mut d = plan_doc(vec![]);
        if let Json::Obj(ms) = &mut d {
            ms.retain(|(k, _)| k != "ops");
        }
        assert_eq!(code(&d), Some(Code::Malformed));
        let mut d = plan_doc(vec![]);
        if let Json::Obj(ms) = &mut d {
            ms[0].1 = s("0.2");
        }
        assert_eq!(code(&d), Some(Code::PlanVersion));
        let mut d = plan_doc(vec![write_op("a")]);
        if let Json::Obj(ms) = &mut d {
            ms[2].1 = Json::Arr(vec![obj(&[
                ("name", s("spine")),
                ("repo", s("o/r")),
                ("rev", s("abc1234")),
            ])]);
        }
        assert_eq!(code(&d), Some(Code::SourceUnpinned));
        let mut op = write_op("a");
        if let Json::Obj(ms) = &mut op {
            ms[5].1 = obj(&[("input", s("other")), ("path", s("a"))]);
        }
        assert_eq!(code(&plan_doc(vec![op])), Some(Code::SourceUnpinned));
        let mut op = write_op("a");
        if let Json::Obj(ms) = &mut op {
            ms.push(("colour".into(), s("red")));
        }
        assert_eq!(code(&plan_doc(vec![op])), Some(Code::Malformed));
        let del = obj(&[
            ("op", s("delete")),
            ("path", s("a")),
            ("expect", s("absent")),
            ("source", obj(&[("input", s("spine")), ("path", s("a"))])),
        ]);
        assert_eq!(code(&plan_doc(vec![del])), Some(Code::Malformed));
    }

    #[test]
    fn hole_names() {
        assert!(is_hole_name("OWNER"));
        assert!(is_hole_name("A_1"));
        assert!(!is_hole_name("owner"));
        assert!(!is_hole_name("1A"));
        assert!(!is_hole_name(""));
    }
}
