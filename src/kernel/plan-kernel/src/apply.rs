// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! The apply decision: refuse, or the exact list of effects.
//!
//! The edge observes the target without following symbolic links and hands
//! the observations here. [`decide`] either refuses, or returns one [`Step`]
//! per operation. The edge performs the steps only after `decide` has
//! returned `Ok` for *every* operation, which is what makes a refusal write
//! nothing.
//!
//! Independence: in a checked plan no path is a directory prefix of another
//! (`check::check_paths`), and a step never removes a directory. So
//! performing one step cannot change the observation of another operation
//! except by creating an absent ancestor directory, which `decide` accepts in
//! both states. Deciding every operation against the initial observation is
//! therefore the same as deciding them in sequence.

use crate::json::{j1, Json};
use crate::path::ancestors;
use crate::plan::{Action, Expect, Plan};
use crate::refusal::{Code, Refusal};

/// What is at a path, observed with `lstat` (links not followed).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Node {
    /// Nothing.
    Absent,
    /// A directory.
    Dir,
    /// A regular file.
    File {
        /// SHA-256 of its content (64 lowercase hex).
        sha256: String,
        /// Owner-execute bit set.
        exec: bool,
    },
    /// A symbolic link (not followed).
    Symlink,
    /// Anything else (device, FIFO, socket).
    Other,
}

/// What the target directory itself is.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TargetRoot {
    /// An existing directory (not a link).
    Dir,
    /// Nothing there.
    Absent,
    /// A symbolic link.
    Symlink,
    /// Something that is not a directory.
    NotDir,
}

/// Observation for one operation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OpObs {
    /// One node per proper ancestor of the path, shortest first
    /// ([`crate::path::ancestors`]). The edge may stop after the first
    /// non-directory; missing trailing entries are read as `Absent`.
    pub ancestors: Vec<Node>,
    /// The path itself.
    pub leaf: Node,
}

/// The effect of one operation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Effect {
    /// Write the blob with this mode (via temp file and rename).
    Write,
    /// Remove the regular file.
    Delete,
    /// The target already has the desired state.
    Unchanged,
    /// Delete of a path that is not there.
    AlreadyAbsent,
}

impl Effect {
    /// The receipt spelling.
    pub fn as_str(&self) -> &'static str {
        match self {
            Effect::Write => "written",
            Effect::Delete => "deleted",
            Effect::Unchanged => "unchanged",
            Effect::AlreadyAbsent => "already-absent",
        }
    }
}

/// One decided step: operation index and effect.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Step {
    /// Index into `plan.ops`.
    pub op: usize,
    /// What to do.
    pub effect: Effect,
}

/// Decide the effect of one operation, or refuse.
pub fn decide_op(plan: &Plan, i: usize, obs: &OpObs) -> Result<Effect, Refusal> {
    let op = &plan.ops[i];
    let names = ancestors(&op.path);
    for (j, node) in obs.ancestors.iter().enumerate() {
        match node {
            Node::Absent => break,
            Node::Dir => {}
            Node::Symlink => {
                return Err(Refusal::new(
                    Code::TargetSymlink,
                    format!(
                        "ops[{i}]: ancestor {} is a symbolic link",
                        names.get(j).copied().unwrap_or("?")
                    ),
                ))
            }
            Node::File { .. } | Node::Other => {
                return Err(Refusal::new(
                    Code::Precondition,
                    format!(
                        "ops[{i}]: ancestor {} is not a directory",
                        names.get(j).copied().unwrap_or("?")
                    ),
                ))
            }
        }
    }
    let fail = |why: &str| Err(Refusal::new(Code::Precondition, format!("ops[{i}]: {why}")));
    match (&op.action, &obs.leaf) {
        (_, Node::Symlink) => Err(Refusal::new(
            Code::TargetSymlink,
            format!("ops[{i}]: the path is a symbolic link"),
        )),
        (Action::Write { blob, mode, .. }, Node::File { sha256, exec })
            if sha256 == blob && *exec == mode.is_exec() =>
        {
            Ok(Effect::Unchanged)
        }
        (
            Action::Write {
                expect: Expect::Absent,
                ..
            },
            Node::Absent,
        ) => Ok(Effect::Write),
        (
            Action::Write {
                expect: Expect::Sha256(_),
                ..
            },
            Node::Absent,
        ) => fail("expected existing content, but the path is absent"),
        (
            Action::Write {
                expect: Expect::Absent,
                ..
            },
            Node::File { .. },
        ) => fail("expected absent, but a file with other content is there"),
        (
            Action::Write {
                expect: Expect::Sha256(e),
                ..
            },
            Node::File { sha256, .. },
        ) => {
            if e == sha256 {
                Ok(Effect::Write)
            } else {
                fail("the file's content differs from the expected sha256")
            }
        }
        (Action::Delete { .. }, Node::Absent) => Ok(Effect::AlreadyAbsent),
        (Action::Delete { expect }, Node::File { sha256, .. }) => {
            if expect == sha256 {
                Ok(Effect::Delete)
            } else {
                fail("the file's content differs from the expected sha256")
            }
        }
        (_, Node::Dir) => fail("a directory is at the path"),
        (_, Node::Other) => fail("a special file is at the path"),
    }
}

/// Refuse, or return one step per operation, in plan order. `obs` must have
/// one entry per operation (a mismatch is refused as `TARGET_INVALID`).
pub fn decide(plan: &Plan, root: TargetRoot, obs: &[OpObs]) -> Result<Vec<Step>, Refusal> {
    match root {
        TargetRoot::Dir => {}
        TargetRoot::Absent => {
            return Err(Refusal::new(
                Code::TargetInvalid,
                "the target does not exist",
            ))
        }
        TargetRoot::NotDir => {
            return Err(Refusal::new(
                Code::TargetInvalid,
                "the target is not a directory",
            ))
        }
        TargetRoot::Symlink => {
            return Err(Refusal::new(
                Code::TargetSymlink,
                "the target is a symbolic link",
            ))
        }
    }
    if obs.len() != plan.ops.len() {
        return Err(Refusal::new(
            Code::TargetInvalid,
            "internal: one observation per operation is required",
        ));
    }
    let mut steps = Vec::with_capacity(obs.len());
    for (i, o) in obs.iter().enumerate() {
        steps.push(Step {
            op: i,
            effect: decide_op(plan, i, o)?,
        });
    }
    Ok(steps)
}

/// The receipt document: `{"ops":[{"path":...,"result":...}],"plan":"sha256:<hex>"}`
/// in J-1 form. `plan_sha256` is the SHA-256 of the plan file's bytes.
pub fn receipt(plan: &Plan, steps: &[Step], plan_sha256: &str) -> String {
    let ops = steps
        .iter()
        .map(|s| {
            Json::Obj(vec![
                ("path".into(), Json::Str(plan.ops[s.op].path.clone())),
                ("result".into(), Json::Str(s.effect.as_str().into())),
            ])
        })
        .collect();
    j1(&Json::Obj(vec![
        ("ops".into(), Json::Arr(ops)),
        ("plan".into(), Json::Str(format!("sha256:{plan_sha256}"))),
    ]))
}
