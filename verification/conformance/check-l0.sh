#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# The one list of commands behind the `Conformance L0` CI check and
# `just conformance`. Both call this script so they cannot drift.
# Needs: cargo, bash, jq, coreutils. Run from anywhere.
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$root"

kernel_bin="src/kernel/target/release/scaffoldia-kernel"

cargo build --release --locked --manifest-path src/kernel/Cargo.toml
verification/conformance/gen-cases.sh --check
verification/conformance/run.sh --level L0 --kernel "$kernel_bin"
verification/conformance/selftest.sh --level L0
