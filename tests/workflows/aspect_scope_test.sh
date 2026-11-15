#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Harness/payload controls: silence, firing and NOT_RUN.
set -euo pipefail
checker="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/aspect_tests.sh"
tmp="$(mktemp -d)"
trap 'rm -rf -- "$tmp"' EXIT
mkdir -p "$tmp/subject/src" "$tmp/subject/docs" "$tmp/subject/verification/proofs" "$tmp/subject/src/interface/ffi/.zig-cache"
git -C "$tmp/subject" init -q
printf '// SPDX-License-Identifier: MPL-2.0\npub fn yes() void {}\n' > "$tmp/subject/src/main.zig"
printf '// SPDX-License-Identifier: MPL-2.0\n(* Admitted, sorry, unsafeCoerce are quoted, not executed. *)\nLemma sample : True.\nProof. exact I. Qed.\n' > "$tmp/subject/verification/proofs/Safety.v"
printf 'Admitted in prose is not proof code.\n' > "$tmp/subject/docs/prose.adoc"
printf 'generated zig cache without SPDX\n' > "$tmp/subject/src/interface/ffi/.zig-cache/dependencies.zig"
git -C "$tmp/subject" add src/main.zig verification/proofs/Safety.v docs/prose.adoc
expect() {
  local label="$1" want="$2" actual=0
  bash "$checker" "$3" > "$tmp/actual.log" 2>&1 || actual=$?
  if [ "$actual" -ne "$want" ]; then
    echo "FAIL: $label: wanted exit $want, observed $actual" >&2
    cat "$tmp/actual.log" >&2
    exit 1
  fi
  echo "PASS: $label ($want)"
}
expect 'silence: clean source + generated cache + commented proof terms' 0 "$tmp/subject"
printf '// SPDX missing\npub fn yes() void {}\n' > "$tmp/subject/src/main.zig"
expect 'firing: tracked source without SPDX' 1 "$tmp/subject"
printf '// SPDX-License-Identifier: MPL-2.0\npub fn yes() void {}\n' > "$tmp/subject/src/main.zig"
printf '%s\n' '-- SPDX-License-Identifier: MPL-2.0' '%default total' 'unsafeValue = believe_me 0' > "$tmp/subject/src/Safety.idr"
git -C "$tmp/subject" add src/Safety.idr
expect 'firing: proof escape in Idris2 source' 1 "$tmp/subject"

# Regression: nested Idris {- ... -} comments are NOT executable escapes.
printf '%s\n' \
  '-- SPDX-License-Identifier: MPL-2.0' \
  '{- outer {- believe_me -} assert_total -}' \
  'safeMarker = "--"' > "$tmp/subject/src/Safety.idr"
expect 'silence: nested Idris block comment and quoted marker' 0 "$tmp/subject"

# Regression: "--" inside a quoted string is NOT a line-comment opener.
printf '%s\n' \
  '-- SPDX-License-Identifier: MPL-2.0' \
  'broken = let marker = "--" in believe_me 0' > "$tmp/subject/src/Safety.idr"
expect 'firing: escape after quoted comment marker' 1 "$tmp/subject"

printf '%s\n' \
  '-- SPDX-License-Identifier: MPL-2.0' \
  'safeMarker = "--"' > "$tmp/subject/src/Safety.idr"

# Lean and Agda have nested block comments with different delimiters.
printf '%s\n' \
  '-- SPDX-License-Identifier: MPL-2.0' \
  '/- outer /- sorry -/ sorry -/' \
  'def marker : String := "--"' > "$tmp/subject/src/Proof.lean"
printf '%s\n' \
  '-- SPDX-License-Identifier: MPL-2.0' \
  '{- outer {- believe_me -} -}' > "$tmp/subject/src/Proof.agda"
git -C "$tmp/subject" add src/Proof.lean src/Proof.agda
expect 'silence: nested Lean and Agda comments' 0 "$tmp/subject"
printf '%s\n' \
  '-- SPDX-License-Identifier: MPL-2.0' \
  'def broken : Nat := let marker := "--"; sorry' > "$tmp/subject/src/Proof.lean"
expect 'firing: Lean sorry after quoted marker' 1 "$tmp/subject"
printf '%s\n' \
  '-- SPDX-License-Identifier: MPL-2.0' \
  'def marker : String := "--"' > "$tmp/subject/src/Proof.lean"

# Coq nested comments and string literals are comments/data, not holes.
printf '%s\n' \
  '(* SPDX-License-Identifier: MPL-2.0 *)' \
  '(* outer (* Admitted. *) no actual proof hole *)' \
  'Definition example : string := "Admitted.".' > "$tmp/subject/verification/proofs/Safety.v"
expect 'silence: nested Coq comments and quoted strings' 0 "$tmp/subject"
printf '%s\n' \
  '(* SPDX-License-Identifier: MPL-2.0 *)' \
  'Lemma hole : False.' \
  'Proof. admit; idtac.' > "$tmp/subject/verification/proofs/Safety.v"
expect 'firing: Coq admit tactical' 1 "$tmp/subject"

# Fail closed on input that cannot be safely lexed.
printf '%s\n' \
  '-- SPDX-License-Identifier: MPL-2.0' \
  '{- unterminated block' > "$tmp/subject/src/Safety.idr"
printf '%s\n' '(* SPDX-License-Identifier: MPL-2.0 *)' 'Lemma okay : True. Proof. exact I. Qed.' > "$tmp/subject/verification/proofs/Safety.v"
expect 'void: malformed proof source is not a passing assessment' 1 "$tmp/subject"

# Workflow must run on proof-only pull requests AND proof-only pushes.
wf="$(cd "$(dirname "$checker")/.." && pwd)/.github/workflows/e2e.yml"
count="$(grep -Fc "      - 'verification/proofs/**'" "$wf" || true)"
if [ "$count" -ne 2 ]; then
  echo "FAIL: expected proof-only triggers under push and pull_request; saw $count" >&2
  exit 1
fi
echo 'PASS: proof-only changes trigger both CI paths'

mkdir -p "$tmp/void"
git -C "$tmp/void" init -q
expect 'void: no source means no assessment' 2 "$tmp/void"
echo 'PASS: aspect harness silence/firing/void controls'
