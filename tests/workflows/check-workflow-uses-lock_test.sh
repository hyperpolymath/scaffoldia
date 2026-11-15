#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Offline positive/negative fixtures for the lock-aware workflow pin checker.
set -euo pipefail
root="$(cd "$(dirname "$0")/../.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/.github/workflows"
sha="$(printf 'a%.0s' {1..40})"
cat > "$tmp/.github/workflows/actions.lock" <<EOF
version: 'v0.0.3'
workflows:
    '.github/workflows/fixture.yml':
        - 'actions/checkout@v7.0.1'
dependencies:
    'actions/checkout@v7.0.1':
        ref: 'v7.0.1'
        commit: 'sha1-$sha'
        owner_id: 1
        repo_id: 2
EOF
printf '%s\n' 'name: fixture' 'jobs:' '  run:' '    steps:' '      - uses: actions/checkout@v7.0.1' > "$tmp/.github/workflows/fixture.yml"
bash "$root/scripts/check-workflow-uses-lock.sh" "$tmp"
sed -i 's/checkout@v7.0.1/checkout@v7.0.2/' "$tmp/.github/workflows/fixture.yml"
if bash "$root/scripts/check-workflow-uses-lock.sh" "$tmp" >/dev/null 2>&1; then
  echo 'FAIL: mismatched workflow tag was wrongly accepted' >&2
  exit 1
fi
printf '%s\n' 'name: fixture' 'jobs:' '  run:' '    steps:' "      - uses: actions/checkout@$sha" > "$tmp/.github/workflows/fixture.yml"
bash "$root/scripts/check-workflow-uses-lock.sh" "$tmp"
printf '%s\n' 'PASS: matching lock, mismatched ref, and direct SHA cases verified'
