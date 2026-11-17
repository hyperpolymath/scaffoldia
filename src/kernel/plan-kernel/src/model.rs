// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! An in-memory model of a target directory, used to state and test
//! idempotence (`apply ∘ apply = apply`) and all-or-nothing, and intended as
//! the abstract state for the Creusot/Verus contracts (see `PROOF-PLAN.adoc`).
//!
//! The model is a map from safe path to node. A file is its content digest
//! and execute bit. Directories are explicit entries; links are opaque.

use std::collections::BTreeMap;

use crate::apply::{decide, Effect, Node, OpObs, Step, TargetRoot};
use crate::path::ancestors;
use crate::plan::{Action, Plan};
use crate::refusal::Refusal;

/// A modelled target directory.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ModelFs {
    /// Entries by path. A missing key is `Absent`.
    pub entries: BTreeMap<String, Node>,
}

impl ModelFs {
    fn node(&self, p: &str) -> Node {
        self.entries.get(p).cloned().unwrap_or(Node::Absent)
    }

    /// Observe the target for a plan, as the edge does with `lstat`: stop at
    /// the first ancestor that is not a directory.
    pub fn observe(&self, plan: &Plan) -> Vec<OpObs> {
        plan.ops
            .iter()
            .map(|op| {
                let mut anc = Vec::new();
                let mut blocked = false;
                for a in ancestors(&op.path) {
                    let n = self.node(a);
                    blocked = n != Node::Dir;
                    anc.push(n);
                    if blocked {
                        break;
                    }
                }
                let leaf = if blocked {
                    Node::Absent
                } else {
                    self.node(&op.path)
                };
                OpObs {
                    ancestors: anc,
                    leaf,
                }
            })
            .collect()
    }

    /// Perform decided steps (creating absent ancestors as directories).
    pub fn perform(&mut self, plan: &Plan, steps: &[Step]) {
        for s in steps {
            let op = &plan.ops[s.op];
            match (&s.effect, &op.action) {
                (Effect::Write, Action::Write { blob, mode, .. }) => {
                    for a in ancestors(&op.path) {
                        self.entries.entry(a.to_string()).or_insert(Node::Dir);
                    }
                    self.entries.insert(
                        op.path.clone(),
                        Node::File {
                            sha256: blob.clone(),
                            exec: mode.is_exec(),
                        },
                    );
                }
                (Effect::Delete, _) => {
                    self.entries.remove(&op.path);
                }
                _ => {}
            }
        }
    }

    /// Decide and, only when every operation is accepted, perform.
    pub fn apply(&mut self, plan: &Plan) -> Result<Vec<Step>, Refusal> {
        let steps = decide(plan, TargetRoot::Dir, &self.observe(plan))?;
        self.perform(plan, &steps);
        Ok(steps)
    }
}

#[cfg(test)]
mod tests {
    //! Property-style tests with a fixed-seed generator (no property-testing
    //! crate: a 20-line xorshift keeps the core dependency-free and the runs
    //! reproducible).

    use super::*;
    use crate::check::check_plan;
    use crate::path::tests::TestPrims;
    use crate::plan::{plan_to_json, Expect, Input, Mode, Op, Source};
    use crate::refusal::Code;

    struct Rng(u64);
    impl Rng {
        fn next(&mut self) -> u64 {
            let mut x = self.0;
            x ^= x << 13;
            x ^= x >> 7;
            x ^= x << 17;
            self.0 = x;
            x
        }
        fn below(&mut self, n: usize) -> usize {
            (self.next() % n as u64) as usize
        }
    }

    const NAMES: &[&str] = &["a", "b", "B", "c.txt", "d", "e-f", "x", "z"];
    const DIGESTS: &[&str] = &[
        "0000000000000000000000000000000000000000000000000000000000000000",
        "1111111111111111111111111111111111111111111111111111111111111111",
        "2222222222222222222222222222222222222222222222222222222222222222",
    ];

    fn rand_path(r: &mut Rng) -> String {
        let depth = 1 + r.below(3);
        (0..depth)
            .map(|_| NAMES[r.below(NAMES.len())])
            .collect::<Vec<_>>()
            .join("/")
    }

    fn rand_digest(r: &mut Rng) -> String {
        DIGESTS[r.below(DIGESTS.len())].to_string()
    }

    fn rand_plan(r: &mut Rng) -> Plan {
        let n = r.below(7);
        let ops = (0..n)
            .map(|_| {
                let path = rand_path(r);
                let action = if r.below(4) == 0 {
                    Action::Delete {
                        expect: rand_digest(r),
                    }
                } else {
                    Action::Write {
                        blob: rand_digest(r),
                        mode: if r.below(3) == 0 {
                            Mode::Executable
                        } else {
                            Mode::Regular
                        },
                        expect: if r.below(2) == 0 {
                            Expect::Absent
                        } else {
                            Expect::Sha256(rand_digest(r))
                        },
                    }
                };
                Op {
                    source: Source {
                        input: "spine".into(),
                        path: path.clone(),
                    },
                    path,
                    action,
                    replaces: None,
                }
            })
            .collect();
        Plan {
            holes: vec![],
            inputs: vec![Input {
                name: "spine".into(),
                repo: "o/r".into(),
                rev: "0123456789abcdef0123456789abcdef01234567".into(),
            }],
            ops,
        }
    }

    fn rand_fs(r: &mut Rng) -> ModelFs {
        let mut fs = ModelFs::default();
        for _ in 0..r.below(6) {
            let p = rand_path(r);
            let anc: Vec<String> = ancestors(&p).iter().map(|s| s.to_string()).collect();
            if anc
                .iter()
                .any(|a| matches!(fs.entries.get(a), Some(n) if *n != Node::Dir))
                || fs.entries.contains_key(&p)
            {
                continue;
            }
            for a in anc {
                fs.entries.insert(a, Node::Dir);
            }
            let leaf = match r.below(5) {
                0 => Node::Symlink,
                1 => Node::Dir,
                _ => Node::File {
                    sha256: rand_digest(r),
                    exec: r.below(2) == 0,
                },
            };
            fs.entries.insert(p, leaf);
        }
        fs
    }

    fn sorted_dedup(mut p: Plan) -> Plan {
        p.ops
            .sort_by(|a, b| a.path.as_bytes().cmp(b.path.as_bytes()));
        p.ops.dedup_by(|a, b| a.path == b.path);
        p
    }

    /// apply ∘ apply = apply, and a refusal leaves the model unchanged.
    #[test]
    fn idempotence_and_all_or_nothing() {
        let mut r = Rng(0x9e3779b97f4a7c15);
        let (mut accepted, mut refused) = (0, 0);
        for _ in 0..20_000 {
            let plan = sorted_dedup(rand_plan(&mut r));
            if check_plan(&plan_to_json(&plan), &TestPrims, true).is_err() {
                continue;
            }
            let fs0 = rand_fs(&mut r);
            let mut fs1 = fs0.clone();
            match fs1.apply(&plan) {
                Err(_) => {
                    refused += 1;
                    assert_eq!(fs1, fs0, "a refusal changed the target");
                }
                Ok(_) => {
                    accepted += 1;
                    let mut fs2 = fs1.clone();
                    let steps2 = fs2.apply(&plan).expect("second application refused");
                    assert_eq!(fs2, fs1, "second application changed the target");
                    assert!(steps2
                        .iter()
                        .all(|s| matches!(s.effect, Effect::Unchanged | Effect::AlreadyAbsent)));
                }
            }
        }
        // The generator must exercise both branches, or the test proves nothing.
        assert!(
            accepted > 1000 && refused > 1000,
            "accepted {accepted}, refused {refused}"
        );
    }

    /// A safe plan stays safe under permutation-then-sort, and sorting gives
    /// back the same plan.
    #[test]
    fn safety_is_stable_under_permutation_then_sort() {
        let mut r = Rng(0x2545f4914f6cdd1d);
        let mut safe = 0;
        for _ in 0..20_000 {
            let plan = sorted_dedup(rand_plan(&mut r));
            if check_plan(&plan_to_json(&plan), &TestPrims, true).is_err() {
                continue;
            }
            safe += 1;
            let mut shuffled = plan.clone();
            for i in (1..shuffled.ops.len()).rev() {
                let j = r.below(i + 1);
                shuffled.ops.swap(i, j);
            }
            let resorted = sorted_dedup(shuffled);
            assert_eq!(resorted, plan);
            assert!(check_plan(&plan_to_json(&resorted), &TestPrims, true).is_ok());
        }
        assert!(safe > 1000, "only {safe} safe plans generated");
    }

    /// A refusal on the last operation means nothing is performed.
    #[test]
    fn last_op_precondition_refuses_whole_plan() {
        let mut plan = rand_plan(&mut Rng(1));
        plan.ops = ["a", "b", "c.txt"]
            .iter()
            .map(|p| Op {
                path: p.to_string(),
                action: Action::Write {
                    blob: DIGESTS[1].into(),
                    mode: Mode::Regular,
                    expect: Expect::Absent,
                },
                source: Source {
                    input: "spine".into(),
                    path: p.to_string(),
                },
                replaces: None,
            })
            .collect();
        let mut fs = ModelFs::default();
        fs.entries.insert(
            "c.txt".into(),
            Node::File {
                sha256: DIGESTS[2].into(),
                exec: false,
            },
        );
        let before = fs.clone();
        assert_eq!(fs.apply(&plan).unwrap_err().code, Code::Precondition);
        assert_eq!(fs, before);
    }
}
