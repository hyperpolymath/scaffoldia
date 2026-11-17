# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# PLAN KERNEL & CONFORMANCE SUITE (ADR-0003)
#
# Imported by ../../Justfile via `import? "build/just/conformance.just"`.
# The suite is engine-neutral (verification/conformance/README.adoc); the
# Rust spike under src/kernel/ is one engine it is run against. Needs: cargo,
# bash, jq, coreutils. Run via `just conformance`.

kernel_manifest := "src/kernel/Cargo.toml"
kernel_bin := "src/kernel/target/release/scaffoldia-kernel"

# Build the plan kernel (release, locked dependencies)
kernel-build:
    cargo build --release --locked --manifest-path {{kernel_manifest}}

# Test, lint and format-check the plan kernel workspace
kernel-check:
    cargo test --locked --manifest-path {{kernel_manifest}}
    cargo clippy --locked --all-targets --manifest-path {{kernel_manifest}} -- -D warnings
    cargo fmt --manifest-path {{kernel_manifest}} --all -- --check

# Fixtures match their generator; kernel passes L0; every negative control fails
conformance: kernel-build
    verification/conformance/gen-cases.sh --check
    verification/conformance/run.sh --level L0 --kernel {{kernel_bin}}
    verification/conformance/selftest.sh --level L0
