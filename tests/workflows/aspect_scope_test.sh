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
mkdir -p "$tmp/void"
git -C "$tmp/void" init -q
expect 'void: no source means no assessment' 2 "$tmp/void"
echo 'PASS: aspect harness silence/firing/void controls'
