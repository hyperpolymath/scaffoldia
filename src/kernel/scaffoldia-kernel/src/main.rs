// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! `scaffoldia-kernel`: the I/O edge over the pure `plan-kernel` crate.
//!
//! ```text
//! scaffoldia-kernel check       --plan F --blobs D
//! scaffoldia-kernel instantiate --plan F --blobs D --answers F --out F --out-blobs D
//! scaffoldia-kernel apply       --plan F --blobs D --target D --receipt F
//! ```
//!
//! Exit 0: done. Exit 10: refused, with one JCS object (plus a line feed) on
//! standard output. Exit 1: an I/O failure (a crash). Exit 2: usage error.
//!
//! This file only reads, hashes, observes and writes. Every decision —
//! well-formedness, path safety, blob integrity, preconditions, which writes
//! happen — is made by `plan-kernel` before the first write.

#![forbid(unsafe_code)]

use std::collections::BTreeMap;
use std::ffi::OsString;
use std::fs;
use std::io::{ErrorKind, Write};
use std::os::unix::fs::{MetadataExt, OpenOptionsExt, PermissionsExt};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use plan_kernel::apply::{decide, receipt, Effect, Node, OpObs, Step, TargetRoot};
use plan_kernel::check::{blobs_needed, check_blobs, check_plan, BlobObs};
use plan_kernel::instantiate::instantiate;
use plan_kernel::json::{is_j1, j1, Json};
use plan_kernel::path::ancestors;
use plan_kernel::plan::{plan_to_json, Action, Plan};
use plan_kernel::refusal::{Code, Refusal};
use plan_kernel::Prims;
use sha2::{Digest, Sha256};

/// Blob contents by digest (64 lowercase hex).
type Blobs = BTreeMap<String, Vec<u8>>;

const EXIT_CRASH: u8 = 1;
const EXIT_USAGE: u8 = 2;
const EXIT_REFUSED: u8 = 10;

const USAGE: &str = "usage:
  scaffoldia-kernel check       --plan F --blobs D
  scaffoldia-kernel instantiate --plan F --blobs D --answers F --out F --out-blobs D
  scaffoldia-kernel apply       --plan F --blobs D --target D --receipt F";

enum Fail {
    Refused(Refusal),
    Crash(String),
    Usage(String),
}

impl From<Refusal> for Fail {
    fn from(r: Refusal) -> Self {
        Fail::Refused(r)
    }
}

fn crash(what: &str, p: &Path, e: std::io::Error) -> Fail {
    Fail::Crash(format!("{what} {}: {e}", p.display()))
}

/// The edge's primitives: real SHA-256 (`sha2`) and real NFC
/// (`unicode-normalization`).
struct EdgePrims;

impl Prims for EdgePrims {
    fn is_nfc(&self, s: &str) -> bool {
        unicode_normalization::is_nfc(s)
    }
    fn sha256_hex(&self, bytes: &[u8]) -> String {
        sha256_hex(bytes)
    }
}

fn sha256_hex(bytes: &[u8]) -> String {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    let d = Sha256::digest(bytes);
    let mut s = String::with_capacity(64);
    for b in d.iter() {
        s.push(HEX[(b >> 4) as usize] as char);
        s.push(HEX[(b & 0xf) as usize] as char);
    }
    s
}

// ── arguments ───────────────────────────────────────────────────────────────

fn parse_args(args: &[OsString], allowed: &[&str]) -> Result<BTreeMap<String, PathBuf>, Fail> {
    let mut out = BTreeMap::new();
    let mut it = args.iter();
    while let Some(flag) = it.next() {
        let flag = flag.to_str().unwrap_or("");
        let Some(name) = flag.strip_prefix("--").filter(|n| allowed.contains(n)) else {
            return Err(Fail::Usage(format!("unexpected argument {flag:?}")));
        };
        let Some(value) = it.next() else {
            return Err(Fail::Usage(format!("--{name} needs a value")));
        };
        if out.insert(name.to_string(), PathBuf::from(value)).is_some() {
            return Err(Fail::Usage(format!("--{name} given twice")));
        }
    }
    for a in allowed {
        if !out.contains_key(*a) {
            return Err(Fail::Usage(format!("--{a} is required")));
        }
    }
    Ok(out)
}

// ── JSON at the edge ────────────────────────────────────────────────────────

fn to_json(v: serde_json::Value) -> Result<Json, Refusal> {
    use serde_json::Value as V;
    Ok(match v {
        V::Null => Json::Null,
        V::Bool(b) => Json::Bool(b),
        V::Number(_) => {
            return Err(Refusal::new(
                Code::Malformed,
                "numbers do not occur in plan format v0.1",
            ));
        }
        V::String(s) => Json::Str(s),
        V::Array(a) => Json::Arr(a.into_iter().map(to_json).collect::<Result<_, _>>()?),
        V::Object(m) => Json::Obj(
            m.into_iter()
                .map(|(k, v)| Ok((k, to_json(v)?)))
                .collect::<Result<_, Refusal>>()?,
        ),
    })
}

/// Read a JSON document that must be in J-1 form (JCS + one line feed).
fn read_j1(path: &Path, what: &str) -> Result<(Vec<u8>, Json), Fail> {
    let bytes = fs::read(path).map_err(|e| crash("cannot read", path, e))?;
    let value: serde_json::Value = serde_json::from_slice(&bytes)
        .map_err(|e| Refusal::new(Code::Malformed, format!("{what} is not JSON: {e}")))?;
    let doc = to_json(value)?;
    if !is_j1(&bytes, &doc) {
        return Err(Refusal::new(
            Code::Malformed,
            format!("{what} is not JCS-serialised followed by exactly one line feed"),
        )
        .into());
    }
    Ok((bytes, doc))
}

// ── observation (never follows symbolic links) ──────────────────────────────

fn lstat(p: &Path) -> Result<Option<fs::Metadata>, Fail> {
    match fs::symlink_metadata(p) {
        Ok(m) => Ok(Some(m)),
        Err(e) if e.kind() == ErrorKind::NotFound => Ok(None),
        Err(e) => Err(crash("cannot inspect", p, e)),
    }
}

fn load_blobs(dir: &Path, plan: &Plan) -> Result<(BTreeMap<String, BlobObs>, Blobs), Fail> {
    let mut obs = BTreeMap::new();
    let mut content = BTreeMap::new();
    for h in blobs_needed(plan) {
        let p = dir.join("sha256").join(&h);
        match lstat(&p)? {
            Some(m) if m.file_type().is_file() => {
                let bytes = fs::read(&p).map_err(|e| crash("cannot read", &p, e))?;
                obs.insert(h.clone(), BlobObs::Present(sha256_hex(&bytes)));
                content.insert(h, bytes);
            }
            _ => {
                obs.insert(h, BlobObs::Missing);
            }
        }
    }
    Ok((obs, content))
}

fn node(p: &Path, hash: bool) -> Result<Node, Fail> {
    let Some(m) = lstat(p)? else {
        return Ok(Node::Absent);
    };
    let t = m.file_type();
    Ok(if t.is_symlink() {
        Node::Symlink
    } else if t.is_dir() {
        Node::Dir
    } else if t.is_file() && hash {
        let bytes = fs::read(p).map_err(|e| crash("cannot read", p, e))?;
        Node::File {
            sha256: sha256_hex(&bytes),
            exec: m.mode() & 0o100 != 0,
        }
    } else {
        Node::Other
    })
}

fn observe(target: &Path, plan: &Plan) -> Result<(TargetRoot, Vec<OpObs>), Fail> {
    let root = match lstat(target)? {
        None => TargetRoot::Absent,
        Some(m) if m.file_type().is_symlink() => TargetRoot::Symlink,
        Some(m) if m.file_type().is_dir() => TargetRoot::Dir,
        Some(_) => TargetRoot::NotDir,
    };
    if root != TargetRoot::Dir {
        return Ok((root, Vec::new()));
    }
    let mut all = Vec::with_capacity(plan.ops.len());
    for op in &plan.ops {
        let mut anc = Vec::new();
        let mut blocked = false;
        for a in ancestors(&op.path) {
            // An ancestor that is a file is not hashed: it is refused anyway.
            let n = node(&target.join(a), false)?;
            blocked = n != Node::Dir;
            anc.push(n);
            if blocked {
                break;
            }
        }
        let leaf = if blocked {
            Node::Absent
        } else {
            node(&target.join(&op.path), true)?
        };
        all.push(OpObs {
            ancestors: anc,
            leaf,
        });
    }
    Ok((root, all))
}

// ── effects ─────────────────────────────────────────────────────────────────

/// Write `bytes` to `dest` through a fresh temporary file in the same
/// directory and a rename. The temporary is opened with `O_CREAT|O_EXCL`, so
/// it cannot be a pre-existing link; the rename replaces a directory entry
/// and never follows a link at `dest`.
fn write_atomic(dest: &Path, bytes: &[u8], mode: u32) -> Result<(), Fail> {
    let dir = dest.parent().unwrap_or(Path::new("."));
    let name = dest
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    let tmp = dir.join(format!(
        ".{name}.plan-kernel-{}.partial",
        std::process::id()
    ));
    let result = (|| {
        let mut f = fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .mode(0o600)
            .open(&tmp)
            .map_err(|e| crash("cannot create", &tmp, e))?;
        f.write_all(bytes)
            .map_err(|e| crash("cannot write", &tmp, e))?;
        f.set_permissions(fs::Permissions::from_mode(mode))
            .map_err(|e| crash("cannot set mode on", &tmp, e))?;
        f.sync_all().map_err(|e| crash("cannot sync", &tmp, e))?;
        fs::rename(&tmp, dest).map_err(|e| crash("cannot rename onto", dest, e))
    })();
    if result.is_err() {
        let _ = fs::remove_file(&tmp);
    }
    result
}

/// Create the missing ancestors of `rel` under `target`, one component at a
/// time, re-checking each with `lstat`: if one has become a link or a
/// non-directory since the check, stop (the plan was decided on other state).
fn ensure_dirs(target: &Path, rel: &str) -> Result<(), Fail> {
    for a in ancestors(rel) {
        let p = target.join(a);
        match lstat(&p)? {
            None => match fs::create_dir(&p) {
                Ok(()) => {}
                Err(e) if e.kind() == ErrorKind::AlreadyExists => {
                    if !matches!(lstat(&p)?, Some(m) if m.file_type().is_dir()) {
                        return Err(Fail::Crash(format!(
                            "target changed during apply at {}",
                            p.display()
                        )));
                    }
                }
                Err(e) => return Err(crash("cannot create directory", &p, e)),
            },
            Some(m) if m.file_type().is_dir() => {}
            Some(_) => {
                return Err(Fail::Crash(format!(
                    "target changed during apply at {}",
                    p.display()
                )))
            }
        }
    }
    Ok(())
}

fn perform(target: &Path, plan: &Plan, steps: &[Step], blobs: &Blobs) -> Result<(), Fail> {
    for s in steps {
        let op = &plan.ops[s.op];
        let dest = target.join(&op.path);
        match (&s.effect, &op.action) {
            (Effect::Write, Action::Write { blob, mode, .. }) => {
                ensure_dirs(target, &op.path)?;
                if matches!(lstat(&dest)?, Some(m) if m.file_type().is_symlink() || m.file_type().is_dir())
                {
                    return Err(Fail::Crash(format!(
                        "target changed during apply at {}",
                        dest.display()
                    )));
                }
                let bytes = blobs
                    .get(blob)
                    .ok_or_else(|| Fail::Crash(format!("internal: blob {blob} not loaded")))?;
                write_atomic(&dest, bytes, if mode.is_exec() { 0o755 } else { 0o644 })?;
            }
            (Effect::Delete, _) => match lstat(&dest)? {
                Some(m) if m.file_type().is_file() => {
                    fs::remove_file(&dest).map_err(|e| crash("cannot delete", &dest, e))?;
                }
                None => {}
                Some(_) => {
                    return Err(Fail::Crash(format!(
                        "target changed during apply at {}",
                        dest.display()
                    )))
                }
            },
            _ => {}
        }
    }
    Ok(())
}

// ── commands ────────────────────────────────────────────────────────────────

fn checked_plan(
    a: &BTreeMap<String, PathBuf>,
    closed: bool,
) -> Result<(Vec<u8>, Plan, Blobs), Fail> {
    let (bytes, doc) = read_j1(&a["plan"], "the plan")?;
    let plan = check_plan(&doc, &EdgePrims, closed)?;
    let (obs, content) = load_blobs(&a["blobs"], &plan)?;
    check_blobs(&plan, &obs)?;
    Ok((bytes, plan, content))
}

fn cmd_check(args: &[OsString]) -> Result<(), Fail> {
    let a = parse_args(args, &["plan", "blobs"])?;
    checked_plan(&a, false).map(|_| ())
}

fn cmd_instantiate(args: &[OsString]) -> Result<(), Fail> {
    let a = parse_args(args, &["plan", "blobs", "answers", "out", "out-blobs"])?;
    let (_, plan, content) = checked_plan(&a, false)?;
    let (_, doc) = read_j1(&a["answers"], "the answers")?;
    let Json::Obj(ms) = doc else {
        return Err(Refusal::new(Code::Malformed, "the answers are not an object").into());
    };
    let mut answers = BTreeMap::new();
    for (k, v) in ms {
        let Json::Str(v) = v else {
            return Err(Refusal::new(Code::Malformed, "every answer must be a string").into());
        };
        answers.insert(k, v);
    }
    let done = instantiate(&plan, &answers, &content, &EdgePrims)?;
    // Decided; now write. Blobs first, plan last, so a reader never sees a
    // plan whose blobs are not there yet.
    let sha_dir = a["out-blobs"].join("sha256");
    fs::create_dir_all(&sha_dir).map_err(|e| crash("cannot create", &sha_dir, e))?;
    for (h, bytes) in &done.blobs {
        write_atomic(&sha_dir.join(h), bytes, 0o644)?;
    }
    write_atomic(&a["out"], j1(&plan_to_json(&done.plan)).as_bytes(), 0o644)
}

fn cmd_apply(args: &[OsString]) -> Result<(), Fail> {
    let a = parse_args(args, &["plan", "blobs", "target", "receipt"])?;
    let (bytes, plan, content) = checked_plan(&a, true)?;
    let target = &a["target"];
    let (root, obs) = observe(target, &plan)?;
    let steps = decide(&plan, root, &obs)?;
    // Every operation is decided; only now does anything get written.
    perform(target, &plan, &steps, &content)?;
    write_atomic(
        &a["receipt"],
        receipt(&plan, &steps, &sha256_hex(&bytes)).as_bytes(),
        0o644,
    )
}

fn main() -> ExitCode {
    let args: Vec<OsString> = std::env::args_os().skip(1).collect();
    let (cmd, rest) = match args.split_first() {
        Some((c, r)) => (c.to_str().unwrap_or(""), r),
        None => ("", &args[..]),
    };
    let result = match cmd {
        "check" => cmd_check(rest),
        "instantiate" => cmd_instantiate(rest),
        "apply" => cmd_apply(rest),
        _ => Err(Fail::Usage(format!("unknown command {cmd:?}"))),
    };
    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(Fail::Refused(r)) => {
            println!("{}", r.to_jcs());
            eprintln!(
                "scaffoldia-kernel: refused {}: {}",
                r.code.as_str(),
                r.detail
            );
            ExitCode::from(EXIT_REFUSED)
        }
        Err(Fail::Crash(m)) => {
            eprintln!("scaffoldia-kernel: error: {m}");
            ExitCode::from(EXIT_CRASH)
        }
        Err(Fail::Usage(m)) => {
            eprintln!("scaffoldia-kernel: {m}\n{USAGE}");
            ExitCode::from(EXIT_USAGE)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numbers_are_refused_at_the_edge() {
        let v: serde_json::Value = serde_json::from_str(r#"{"a":[1]}"#).unwrap();
        assert_eq!(to_json(v).unwrap_err().code, Code::Malformed);
    }

    #[test]
    fn duplicate_names_fail_the_j1_check() {
        let bytes = b"{\"a\":\"x\",\"a\":\"x\"}\n";
        let v: serde_json::Value = serde_json::from_slice(bytes).unwrap();
        assert!(!is_j1(bytes, &to_json(v).unwrap()));
    }

    #[test]
    fn edge_prims() {
        assert_eq!(
            sha256_hex(b""),
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        );
        assert!(EdgePrims.is_nfc("caf\u{e9}"));
        assert!(!EdgePrims.is_nfc("cafe\u{301}"));
    }
}
