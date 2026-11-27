#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Adversarial regression tests for the fail-closed STATE metadata validator.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VALIDATOR="$REPO_ROOT/scripts/validate-state.sh"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

fail() {
    echo "FAIL: $*" >&2
    if [ -f "$WORK/output" ]; then sed 's/^/  /' "$WORK/output" >&2; fi
    exit 1
}

expect_rc() {
    local expected="$1" label="$2" actual=0
    shift 2
    "$@" >"$WORK/output" 2>&1 || actual=$?
    [ "$actual" -eq "$expected" ] || fail "$label: expected exit $expected, got $actual"
    echo "PASS: $label (exit $actual)"
}

mkdir -p "$WORK/valid" "$WORK/no-metadata" "$WORK/no-project" "$WORK/wrong-section" "$WORK/empty-project"
cat > "$WORK/valid/STATE.a2ml" <<'STATE'
# leading comments are harmless
[metadata]
    project   =   "scaffoldia"
version = "0.1.0"

[project-context]
name = "scaffoldia"
STATE

cat > "$WORK/no-metadata/STATE.a2ml" <<'STATE'
[project-context]
project = "not metadata"
STATE

cat > "$WORK/no-project/STATE.a2ml" <<'STATE'
[metadata]
version = "0.1.0"
STATE

cat > "$WORK/wrong-section/STATE.a2ml" <<'STATE'
[metadata]
version = "0.1.0"

[project-context]
project = "outside metadata"
STATE

cat > "$WORK/empty-project/STATE.a2ml" <<'STATE'
[metadata]
project = ""

[project-context]
name = "scaffoldia"
STATE

expect_rc 0 "valid metadata and project declaration" bash "$VALIDATOR" "$WORK/valid/STATE.a2ml"
expect_rc 1 "missing required file is invalid" bash "$VALIDATOR" "$WORK/missing/STATE.a2ml"
expect_rc 1 "missing metadata section is invalid" bash "$VALIDATOR" "$WORK/no-metadata/STATE.a2ml"
expect_rc 1 "missing project declaration is invalid" bash "$VALIDATOR" "$WORK/no-project/STATE.a2ml"
expect_rc 1 "project outside metadata does not satisfy the contract" bash "$VALIDATOR" "$WORK/wrong-section/STATE.a2ml"
expect_rc 1 "empty project declaration is invalid" bash "$VALIDATOR" "$WORK/empty-project/STATE.a2ml"
expect_rc 2 "ambiguous arguments are setup failure" bash "$VALIDATOR" one two

echo "PASS: all STATE metadata failure controls behaved as required"
