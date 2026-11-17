#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath)
#
# check-plan-laws.sh [--controls]
#
# The gate for the plan-law development, verification/proofs/agda/PlanLaws/
# (ADR-0003, "Composition is a join, not an overwrite"). It checks the graded
# root PlanLaws/All.agda with the canonical theory invocation of standards
# PROOF-POLICY §2.1,
#
#   agda --safe --without-K --cubical-compatible PlanLaws/All.agda
#
# from the source root verification/proofs/agda, and refuses the development
# unless ALL of these hold:
#
#   1. Agda is on PATH (absent is a failure, never a skip) and is the pinned
#      version below (PROOF-POLICY §6: assert the version, fail on a mismatch).
#   2. Every .agda file begins with exactly the canonical OPTIONS line and has
#      no other pragma ({-# ... #-}): no TERMINATING, NON_TERMINATING,
#      NO_POSITIVITY_CHECK, REWRITE, BUILTIN, and no second OPTIONS that could
#      weaken the flags.
#   3. No hole ({! !}) and no trust primitive (primTrustMe, primEraseEquality).
#   4. Agda exits 0 with no warning.
#   5. The comment-aware postulate scan (scripts/scan-postulates.sh, adopted
#      from the PROOF-POLICY §8.4 reference gate) finds no postulate in the
#      first-party modules of the import cone that Agda reports
#      (--dependency-graph).
#   6. The prover-supplied modules of the cone postulate exactly the trusted
#      base below, by name and BUILTIN binding (§8.4); anything else fails.
#   7. Every .agda file under PlanLaws/ is in the import cone of the root, so
#      no file escapes the check.
#
# Agda runs on a fresh copy of the .agda files in a temporary directory, so a
# stale interface file (.agdai) can never stand in for a check and nothing is
# written into the repository.
#
# With --controls it runs the negative controls instead: each file in
# tests/reject/PlanLaws/Controls/ is added to a copy of
# the development and checked as the root by the same gate. Each MUST be
# refused, and for the reason its `-- EXPECT-REFUSED: <ERE>` line names. A
# further generated control adds an orphan module with a postulate that the
# root does not import; check 7 must refuse it. A control that is accepted, or
# refused for another reason, fails the run: a gate that accepts everything
# proves nothing.
#
# On success the gate prints the fields a proof receipt records (PROOF-POLICY
# §8.5.1): checker and version, invocation, import cone, trusted base, axioms
# (none), and the inputs manifest with its SHA-256 for the working tree.
#
# Exit: 0 pass; 1 fail; 2 misuse.

# shellcheck disable=SC2001  # sed reads more plainly here for indenting multi-line output
set -euo pipefail

PINNED_AGDA="2.7.0.1"
CANON='{-# OPTIONS --safe --without-K --cubical-compatible #-}'
FLAGS=(--safe --without-K --cubical-compatible)
SRC="verification/proofs/agda"
DEV="PlanLaws"
ROOT_MODULE="PlanLaws/All.agda"
CONTROLS="tests/reject/PlanLaws/Controls"

# The prover-supplied postulates the cone may reach, for the pinned Agda:
# "<module> <name> BUILTIN <binding>", one per line.
EXPECTED_TRUSTED_BASE="$(LC_ALL=C sort <<'EOF'
Agda.Primitive Level BUILTIN LEVEL
Agda.Primitive lzero BUILTIN LEVELZERO
Agda.Primitive lsuc BUILTIN LEVELSUC
Agda.Primitive _⊔_ BUILTIN LEVELMAX
EOF
)"

MODE=gated
case "${1:-}" in
  "") ;;
  --controls) MODE=controls ;;
  *) echo "usage: $(basename "$0") [--controls]" >&2; exit 2 ;;
esac

cd "$(dirname "${BASH_SOURCE[0]}")/.."
ROOT="$PWD"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# --- toolchain ----------------------------------------------------------------
if ! command -v agda >/dev/null 2>&1; then
  echo "FAIL: agda not found on PATH. A missing prover is a failure, never a skip." >&2
  exit 1
fi
version_line="$(agda --version 2>/dev/null | head -1)"
echo "checker: $version_line ($(command -v agda))"
if [ "${version_line#Agda version }" != "$PINNED_AGDA" ]; then
  echo "FAIL: expected Agda $PINNED_AGDA (the pin this gate's trusted base was read from)." >&2
  echo "      A new Agda changes the trusted base: re-read it and re-earn the receipt." >&2
  exit 1
fi

# Agda keeps the interfaces of its own modules under <data dir>/lib/prim. If
# that directory is read-only and has none (a packaged Agda), give Agda a
# writable copy through Agda_datadir, the variable it reads for it.
DATADIR="${Agda_datadir:-$(agda --print-agda-data-dir 2>/dev/null)}"
if [ ! -d "$DATADIR/lib/prim/_build" ] && [ ! -w "$DATADIR/lib/prim" ]; then
  cp -r "$DATADIR/." "$WORK/datadir/"
  chmod -R u+w "$WORK/datadir"
  DATADIR="$WORK/datadir"
fi
export Agda_datadir="$DATADIR"

# --- the trusted base a prover-supplied module declares ----------------------
# Prints "<module> <name> BUILTIN <binding>" for each name the module
# postulates (UNBOUND if no BUILTIN pragma binds it). Prints
# "UNREADABLE <module>" if the number of postulate blocks it parsed differs
# from the number of postulate keywords the comment-aware scan finds.
prover_postulates() {
  local module="$1" file="$2" blocks keywords
  awk -v mod="$module" '
    /^[ \t]*--/ { next }
    /^[ \t]*postulate[ \t]*$/ { inblock = 1; blocks++; next }
    /^[ \t]*postulate[ \t]+[^ \t]+[ \t]+:/ {
      s = $0; sub(/^[ \t]*postulate[ \t]+/, "", s); sub(/[ \t].*/, "", s)
      names[++n] = s; blocks++; inblock = 0; next }
    inblock && /^[ \t]+[^ \t]+[ \t]+:/ {
      s = $0; sub(/^[ \t]+/, "", s); sub(/[ \t].*/, "", s); names[++n] = s; next }
    inblock && /^[ \t]*$/ { inblock = 0; next }
    inblock && /^[^ \t]/ { inblock = 0 }
    /\{-#[ \t]*BUILTIN[ \t]/ {
      s = $0; sub(/.*\{-#[ \t]*BUILTIN[ \t]+/, "", s); split(s, w, /[ \t]+/)
      bound[w[2]] = w[1] }
    END {
      for (i = 1; i <= n; i++)
        printf "%s %s %s\n", mod, names[i], (names[i] in bound) ? "BUILTIN " bound[names[i]] : "UNBOUND"
      printf "#blocks %d\n", blocks }
  ' "$file" >"$WORK/pp"
  blocks="$(sed -n 's/^#blocks //p' "$WORK/pp")"
  set +e
  keywords="$(bash "$ROOT/scripts/scan-postulates.sh" "$file" 2>/dev/null | sed -n 's/^postulate scan: [0-9]* files, \([0-9]*\) postulate.*/\1/p')"
  set -e
  if [ "$blocks" != "$keywords" ]; then echo "UNREADABLE $module"; fi
  grep -v '^#blocks ' "$WORK/pp" || true
}

# --- the gate ---------------------------------------------------------------------
# gate DIR ROOTREL COVER : checks DIR (a source root) with ROOTREL as the root
# module. COVER=1 also requires every .agda file under DIR/PlanLaws to be in
# the cone. Prints its findings; returns 0 iff every check passes.
gate() {
  local dir="$1" rootrel="$2" cover="$3" fails=0 f out rc m
  local -a files own prover
  mapfile -t files < <(cd "$dir" && find . -name '*.agda' | sed 's|^\./||' | LC_ALL=C sort)

  # Checks 2 and 3: header, pragmas, holes, trust primitives.
  for f in "${files[@]}"; do
    if [ "$(head -n 1 "$dir/$f")" != "$CANON" ]; then
      echo "  gate: first line is not the canonical OPTIONS pragma: $f"; fails=1
    fi
    if [ "$(grep -c '{-#' "$dir/$f")" -ne 1 ]; then
      echo "  gate: pragma other than the canonical OPTIONS line: $f"
      grep -n '{-#' "$dir/$f" | sed 's/^/        /'; fails=1
    fi
    if grep -qE '\{!|!\}|primTrustMe|primEraseEquality' "$dir/$f"; then
      echo "  gate: hole or trust primitive: $f"
      grep -nE '\{!|!\}|primTrustMe|primEraseEquality' "$dir/$f" | sed 's/^/        /'; fails=1
    fi
  done

  # Check 4: Agda, canonical flags, clean.
  rm -f "$WORK/cone.dot"
  set +e
  out="$(cd "$dir" && agda "${FLAGS[@]}" --dependency-graph="$WORK/cone.dot" "$rootrel" 2>&1)"; rc=$?
  set -e
  if [ "$rc" -ne 0 ]; then
    echo "  agda: exit $rc"
    grep -v '^  /' <<<"$out" | grep -v '^ *Checking ' | sed 's/^/  agda: /' | head -30
    return 1
  fi
  if grep -q 'warning' <<<"$out"; then
    echo "  agda: warnings (a clean check is required)"
    sed 's/^/  agda: /' <<<"$out" | head -30
    fails=1
  fi
  if [ ! -s "$WORK/cone.dot" ]; then echo "  gate: agda reported no import cone"; return 1; fi

  # Split the cone into first-party files and prover-supplied modules.
  own=(); prover=()
  while IFS= read -r m; do
    if [ -f "$dir/${m//.//}.agda" ]; then own+=("${m//.//}.agda"); else prover+=("$m"); fi
  done < <(sed -n 's/.*label="\([^"]*\)".*/\1/p' "$WORK/cone.dot" | LC_ALL=C sort -u)

  # Check 5: no postulate in the first-party cone.
  set +e
  out="$(cd "$dir" && bash "$ROOT/scripts/scan-postulates.sh" "${own[@]}" 2>&1)"; rc=$?
  set -e
  if [ "$rc" -ne 0 ]; then echo "  gate: postulate in the import cone"; sed 's/^/        /' <<<"$out"; fails=1; fi

  # Check 6: the prover-supplied postulates are exactly the trusted base.
  local found=""
  for m in "${prover[@]}"; do
    f="$DATADIR/lib/prim/${m//.//}.agda"
    if [ ! -f "$f" ]; then echo "  gate: cannot find prover module $m at $f"; fails=1; continue; fi
    found+="$(prover_postulates "$m" "$f")"$'\n'
  done
  found="$(grep -v '^$' <<<"$found" | LC_ALL=C sort || true)"
  if [ "$found" != "$EXPECTED_TRUSTED_BASE" ]; then
    echo "  gate: the prover-supplied postulates of the cone are not the recorded trusted base"
    diff <(echo "$EXPECTED_TRUSTED_BASE") <(echo "$found") | sed 's/^/        /' || true
    fails=1
  fi

  # Check 7: coverage.
  if [ "$cover" = 1 ]; then
    local unreached
    unreached="$(comm -23 <(cd "$dir" && find "$DEV" -name '*.agda' | LC_ALL=C sort) \
                          <(printf '%s\n' "${own[@]}" | LC_ALL=C sort) || true)"
    if [ -n "$unreached" ]; then
      echo "  gate: files under $DEV/ that the root does not import (unchecked):"
      sed 's/^/        /' <<<"$unreached"; fails=1
    fi
  fi

  GATE_OWN=("${own[@]}"); GATE_PROVER=("${prover[@]}"); GATE_TB="$found"
  return "$fails"
}

# A fresh copy of the development's .agda files at DIR/PlanLaws.
copy_dev() {
  local dir="$1" f
  mkdir -p "$dir"
  while IFS= read -r f; do
    mkdir -p "$dir/$(dirname "$f")"; cp "$SRC/$f" "$dir/$f"
  done < <(cd "$SRC" && find "$DEV" -name '*.agda' | LC_ALL=C sort)
}

if [ "$MODE" = gated ]; then
  echo "=== plan laws: gate (verification/proofs/agda/$DEV) ==="
  copy_dev "$WORK/src"
  if ! gate "$WORK/src" "$ROOT_MODULE" 1; then
    echo; echo "RESULT: FAIL"; exit 1
  fi
  echo "  invocation: cd $SRC && agda ${FLAGS[*]} $ROOT_MODULE"
  echo "  import cone (first party): ${#GATE_OWN[@]} module(s)"
  printf '        %s\n' "${GATE_OWN[@]}"
  echo "  import cone (prover-supplied): ${GATE_PROVER[*]}"
  echo "  trusted base (prover postulates, by name and binding):"
  sed 's/^/        /' <<<"$GATE_TB"
  echo "  axioms: none (no postulate in the first-party cone; --safe in force)"
  echo "  library: none (no agda-stdlib, no .agda-lib)"
  manifest="$(for f in "${GATE_OWN[@]}"; do printf '%s\n' "$SRC/$f"; done | LC_ALL=C sort |
              while IFS= read -r p; do printf '%s  %s\n' "$(sha256sum "$p" | cut -d' ' -f1)" "$p"; done)"
  echo "  inputs manifest (working tree; PROOF-POLICY §8.5.1):"
  sed 's/^/        /' <<<"$manifest"
  echo "  inputs-sha256 (working tree): $(printf '%s\n' "$manifest" | sha256sum | cut -d' ' -f1)"
  echo; echo "RESULT: PASS"
  exit 0
fi

# --- negative controls ------------------------------------------------------------
echo "=== plan laws: negative controls ($CONTROLS) ==="
bad=0; n=0
mapfile -t controls < <(find "$CONTROLS" -name '*.agda' 2>/dev/null | LC_ALL=C sort)
if [ "${#controls[@]}" -eq 0 ]; then echo "FAIL: no negative controls found"; exit 1; fi

run_control() {
  # $1 name, $2 expected ERE, $3 directory holding the prepared source root,
  # $4 root module, $5 cover flag
  local name="$1" expect="$2" dir="$3" rootrel="$4" cover="$5" out rc
  n=$((n + 1))
  set +e; out="$(gate "$dir" "$rootrel" "$cover" 2>&1)"; rc=$?; set -e
  if [ "$rc" -eq 0 ]; then
    echo "FAIL  $name: ACCEPTED by the gate (a control must be refused)"; bad=$((bad + 1))
  elif ! grep -qE "$expect" <<<"$out"; then
    echo "FAIL  $name: refused, but not for the expected reason /$expect/:"
    sed 's/^/      /' <<<"$out"; bad=$((bad + 1))
  else
    echo "ok    $name: refused, as expected (/$expect/)"
    grep -E "$expect" <<<"$out" | head -3 | sed 's/^/      /'
  fi
}

for c in "${controls[@]}"; do
  expect="$(sed -n 's/^-- EXPECT-REFUSED: //p' "$c" | head -1)"
  if [ -z "$expect" ]; then echo "FAIL  $c: no EXPECT-REFUSED line"; bad=$((bad + 1)); continue; fi
  d="$WORK/ctl-$(basename "$c" .agda)"
  copy_dev "$d"
  mkdir -p "$d/$DEV/Controls"; cp "$c" "$d/$DEV/Controls/"
  run_control "$(basename "$c")" "$expect" "$d" "$DEV/Controls/$(basename "$c")" 0
done

# Generated control: an orphan module with a postulate, not imported by the
# root. Agda never sees it; only the coverage check (7) can refuse it.
d="$WORK/ctl-orphan"; copy_dev "$d"
{ echo "$CANON"; echo "module PlanLaws.Orphan where"; echo "postulate"; echo "  anything : (A : Set) → A"; } >"$d/$DEV/Orphan.agda"
run_control "Orphan.agda (generated)" "the root does not import" "$d" "$ROOT_MODULE" 1

echo
if [ "$bad" -gt 0 ]; then echo "RESULT: FAIL ($bad of $n control(s) not refused as expected)"; exit 1; fi
echo "RESULT: PASS ($n of $n control(s) refused, each for its expected reason)"
