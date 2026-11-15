#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Scoped source assessment: not a formal proof or ABI test.
set -uo pipefail
root="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
if ! command -v git >/dev/null || ! command -v perl >/dev/null; then
  echo 'NOT_RUN: git or perl unavailable' >&2
  exit 2
fi
root="$(cd "$root" 2>/dev/null && pwd)" || { echo 'NOT_RUN: missing target' >&2; exit 2; }
if ! git -C "$root" rev-parse --show-toplevel >/dev/null 2>&1; then
  echo 'NOT_RUN: not a Git checkout' >&2
  exit 2
fi
fail=0
checked=0
proofs=0
while IFS= read -r -d '' path; do
  case "$path" in *.idr|*.zig|*.rs|*.sh|*.v|*.lean|*.agda|*.thy) ;; *) continue ;; esac
  file="$root/$path"
  [ -f "$file" ] || { echo "FAIL: tracked source disappeared: $path" >&2; fail=1; continue; }
  checked=$((checked+1))
  if ! head -6 "$file" | grep -qE '^[[:space:]]*(--|//|#|\(\*)[[:space:]]*SPDX-License-Identifier:'; then
    echo "FAIL: missing SPDX header in tracked source: $path" >&2
    fail=1
  fi
  case "$path" in
    *.idr|*.v|*.lean|*.agda|*.thy)
      proofs=$((proofs+1))
      # Lexical guard only. The language-specific proof checker owns soundness.
      if ! perl -0777 -e '
        local $/;
        my $source = <>;
        $source =~ s/\(\*.*?\*\)//gs;
        $source =~ s/(?:--|\/\/)[^\n]*//g;
        exit ($source =~ /\b(?:believe_me|really_believe_me|assert_total|Admitted|sorry|unsafeCoerce)\b/ ? 1 : 0);
      ' "$file"; then
        echo "FAIL: proof escape in executable source: $path" >&2
        fail=1
      fi
      ;;
  esac
done < <(cd "$root" && git ls-files -z -- src verification/proofs)
if [ "$checked" -eq 0 ]; then
  echo 'NOT_RUN: no tracked source code to assess' >&2
  exit 2
fi
if [ "$proofs" -eq 0 ]; then
  echo 'NOT_APPLICABLE: no proof-language sources; no proof assurance claimed'
else
  echo "CHECKED: $proofs proof-language source files (lexical; not a proof)"
fi
if [ "$fail" -ne 0 ]; then
  echo "FAIL: tracked-source aspect check ($checked source files)" >&2
  exit 1
fi
echo "PASS: $checked tracked source files; generated caches and prose excluded"
