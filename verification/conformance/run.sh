#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# run.sh — engine-neutral conformance runner (verification/conformance/README.adoc).
#
#   run.sh --kernel "<cmd>" [--planner "<cmd>"] [--level L0|L1|L2|L3] [--cases DIR]
#
# For each case directory cases/<kernel|planner>/<Lx>/<id>/ at or below the
# requested level (default L3), the runner copies the inputs and before/ into
# a fresh sandbox, runs the case's kind (check, instantiate, apply,
# apply-twice, plan) and asserts the README's contract. It prints one line per
# case (ok / FAIL / SKIP), then totals per level.
#
# Exit 0: every case at or below the level passed. Exit 1: any FAIL, or any
# SKIP at or below the level (a skip is never a pass). Exit 2: the runner could
# not run (usage, missing tool, no case found, malformed case.json).
#
# Command strings are split into words on whitespace with `read -a` (no eval,
# no globbing, no quote processing): a command whose path contains spaces is
# not supported. A first word containing `/` is made absolute, because the
# engine runs with its working directory inside the sandbox.
set -euo pipefail

usage() {
  sed -n '7,7p' "$0" | sed 's/^# *//' >&2
  exit 2
}

KERNEL_CMD=""
PLANNER_CMD=""
LEVEL="L3"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CASES="$HERE/cases"

while [ "$#" -gt 0 ]; do
  case "$1" in
    --kernel) KERNEL_CMD="${2:-}"; shift 2 || usage ;;
    --planner) PLANNER_CMD="${2:-}"; shift 2 || usage ;;
    --level) LEVEL="${2:-}"; shift 2 || usage ;;
    --cases) CASES="${2:-}"; shift 2 || usage ;;
    *) usage ;;
  esac
done

case "$LEVEL" in L0 | L1 | L2 | L3) ;; *) echo "run.sh: --level must be L0..L3" >&2; exit 2 ;; esac
[ -n "$KERNEL_CMD" ] || { echo "run.sh: --kernel is required" >&2; exit 2; }
[ -d "$CASES" ] || { echo "run.sh: cases directory $CASES not found" >&2; exit 2; }
for tool in jq sha256sum find stat sort cmp timeout mktemp readlink; do
  command -v "$tool" >/dev/null 2>&1 || { echo "run.sh: $tool not found" >&2; exit 2; }
done

# Split a command string into an argv array (named by $1) without eval.
split_cmd() {
  local -n _out="$1"
  read -r -a _out <<<"$2"
  if [ "${#_out[@]}" -eq 0 ]; then
    return 1
  fi
  if [[ "${_out[0]}" == */* ]]; then
    _out[0]="$(cd "$(dirname "${_out[0]}")" && pwd)/$(basename "${_out[0]}")"
  fi
}

KERNEL=()
PLANNER=()
split_cmd KERNEL "$KERNEL_CMD" || { echo "run.sh: empty --kernel" >&2; exit 2; }
if [ -n "$PLANNER_CMD" ]; then
  split_cmd PLANNER "$PLANNER_CMD" || { echo "run.sh: empty --planner" >&2; exit 2; }
fi

level_num() { echo "${1#L}"; }
MAXLEVEL="$(level_num "$LEVEL")"
TIMEOUT="${CONFORMANCE_TIMEOUT:-60}"

WORKROOT="$(mktemp -d)"
trap 'rm -rf "$WORKROOT"' EXIT

# ── tree manifests ──────────────────────────────────────────────────────────
# One line per entry, sorted, paths %q-escaped so any byte is representable:
#   f <x|-> <sha256> <path>   regular file (x = owner-execute bit set)
#   l <path> <link target>    symbolic link (never followed)
#   d <path>                  directory (only when $2 = 1)
#   o <path>                  anything else
# Further arguments are paths to prune from the walk.
manifest() {
  local root="$1" dirs="$2"
  shift 2
  if [ -L "$root" ]; then
    printf 'root-link %q\n' "$(readlink -- "$root")"
    return
  fi
  if [ ! -e "$root" ]; then
    echo "root-absent"
    return
  fi
  if [ ! -d "$root" ]; then
    printf 'root-file %s\n' "$(sha256sum <"$root" | cut -c1-64)"
    return
  fi
  local -a expr=()
  if [ "$#" -gt 0 ]; then
    expr+=("(")
    local first=1 p
    for p in "$@"; do
      if [ "$first" = 0 ]; then expr+=(-o); fi
      expr+=(-path "$p")
      first=0
    done
    expr+=(")" -prune -o)
  fi
  find "$root" -mindepth 1 "${expr[@]}" -print0 | LC_ALL=C sort -z | while IFS= read -r -d '' p; do
    local rel="${p#"$root"/}"
    if [ -L "$p" ]; then
      printf 'l %q %q\n' "$rel" "$(readlink -- "$p")"
    elif [ -d "$p" ]; then
      if [ "$dirs" = 1 ]; then printf 'd %q\n' "$rel"; fi
    elif [ -f "$p" ]; then
      local mode x
      mode="$(stat -c %a -- "$p")"
      x="-"
      if (((8#$mode & 8#100) != 0)); then x="x"; fi
      printf 'f %s %s %q\n' "$x" "$(sha256sum <"$p" | cut -c1-64)" "$rel"
    else
      printf 'o %q\n' "$rel"
    fi
  done
}

# Print a short difference between two manifests (for FAIL reasons).
mdiff() {
  diff <(echo "$1") <(echo "$2") | grep '^[<>]' | head -3 | tr '\n' ';' || true
}

# ── per-case state ──────────────────────────────────────────────────────────
REASON=""
fail() { REASON="$1"; return 1; }

# Run an engine with stdout/stderr captured; sets RC. $1 = log name.
run_engine() {
  local name="$1"
  shift
  RC=0
  (cd "$CWD" && timeout "$TIMEOUT" "$@") >"$C/log/$name.out" 2>"$C/log/$name.err" || RC=$?
}

# Is the file's content its own JCS form (as far as jq can tell), with at
# most one final line feed ($2 = "lf" requires exactly one)?
is_jcs_file() {
  local f="$1" need="$2" canon body
  canon="$(jq -cS . <"$f" 2>/dev/null)" || return 1
  body="$(cat -- "$f"; echo x)"
  body="${body%x}"
  if [ "$need" = lf ]; then
    [ "$body" = "$canon"$'\n' ]
  else
    [ "$body" = "$canon"$'\n' ] || [ "$body" = "$canon" ]
  fi
}

assert_refused() { # $1 log name, $2 expected code
  local out="$C/log/$1.out" got
  [ "$RC" = 10 ] || {
    if [ "$RC" = 0 ]; then fail "accepted; expected refusal $2"; else fail "crashed (exit $RC); expected refusal $2"; fi
    return 1
  }
  jq -e 'type == "object" and (.refused | type) == "string" and (.detail | type) == "string"' <"$out" >/dev/null 2>&1 ||
    fail "refusal output is not {\"refused\":...,\"detail\":...}" || return 1
  is_jcs_file "$out" any || fail "refusal output is not JCS" || return 1
  got="$(jq -r .refused <"$out")"
  [ "$got" = "$2" ] || fail "refused $got; expected $2" || return 1
}

assert_escape_free() {
  local now
  now="$(manifest "$C" 1 "$TARGET" "$C/out" "$C/log")"
  [ "$now" = "$ESCAPE0" ] || fail "wrote outside the target: $(mdiff "$ESCAPE0" "$now")"
}

assert_target_unchanged() {
  local now
  now="$(manifest "$TARGET" 1)"
  [ "$now" = "$TARGET0" ] || fail "target changed although refused: $(mdiff "$TARGET0" "$now")"
}

assert_tree_expected() {
  local want got
  want="$(manifest "$CASE/expected" 0)"
  if [ ! -d "$CASE/expected" ]; then want=""; fi
  got="$(manifest "$TARGET" 0)"
  [ "$got" = "$want" ] || fail "tree differs from expected/: $(mdiff "$want" "$got")"
}

# $1 receipt, $2 plan file. Checks shape, digest and paths; with $3 = "rerun"
# also that every result is unchanged/already-absent.
assert_receipt() {
  local r="$1" plan="$2" mode="${3:-}" digest
  [ -f "$r" ] || fail "no receipt written" || return 1
  is_jcs_file "$r" lf || fail "receipt is not JCS followed by one line feed" || return 1
  digest="sha256:$(sha256sum <"$plan" | cut -c1-64)"
  jq -e --arg d "$digest" '.plan == $d' <"$r" >/dev/null || fail "receipt plan digest is not $digest" || return 1
  jq -e --slurpfile p "$plan" '(.ops | map(.path)) == ($p[0].ops | map(.path))' <"$r" >/dev/null ||
    fail "receipt does not list the plan's paths in order" || return 1
  jq -e '.ops | all(.result | IN("written", "unchanged", "deleted", "already-absent"))' <"$r" >/dev/null ||
    fail "receipt has an unknown result" || return 1
  if [ "$mode" = rerun ]; then
    jq -e '.ops | all(.result | IN("unchanged", "already-absent"))' <"$r" >/dev/null ||
      fail "second application did work: $(jq -c '[.ops[] | select(.result | IN("written", "deleted"))]' <"$r")" || return 1
  elif jq -e '.expect | has("results")' "$CASE/case.json" >/dev/null; then
    jq -e --slurpfile c "$CASE/case.json" '(.ops | map({(.path): .result}) | add // {}) == $c[0].expect.results' <"$r" >/dev/null ||
      fail "receipt results $(jq -c '.ops | map({(.path): .result}) | add // {}' <"$r") differ from expected" || return 1
  fi
}

assert_excludes() {
  local s
  while IFS= read -r s; do
    [ -n "$s" ] || continue
    if grep -rqF -- "$s" "$TARGET" 2>/dev/null; then
      fail "the resulting tree contains the excluded string $(printf '%q' "$s")" || return 1
    fi
  done < <(jq -r '.expect.tree_excludes // [] | .[]' "$CASE/case.json")
}

kernel_apply() { # $1 log name, $2 plan, $3 blobs, $4 receipt
  run_engine "$1" "${KERNEL[@]}" apply --plan "$2" --blobs "$3" --target "$TARGET" --receipt "$4"
}

# Check case.json; prints nothing, returns 1 with REASON on a bad case.
validate_case() {
  local dir="$1" half="$2" lvl="$3" id="$4"
  [ -f "$dir/case.json" ] || fail "case.json missing" || return 1
  is_jcs_file "$dir/case.json" lf || fail "case.json is not JCS followed by one line feed" || return 1
  jq -e --arg id "$id" --arg lvl "$lvl" --arg half "$half" '
    (.id == $id) and (.level == $lvl)
    and (.kind | IN("check", "instantiate", "apply", "apply-twice", "plan"))
    and ((.kind == "plan") == ($half == "planner"))
    and (.expect.outcome | IN("ok", "refused"))
    and (if .expect.outcome == "refused" then (.expect.code | type) == "string" else (.expect | has("code") | not) end)
    and ([.principle, .source, .title] | all(type == "string" and length > 0))
    and ((.target // "directory") | IN("directory", "absent", "file"))
  ' "$dir/case.json" >/dev/null || fail "case.json does not follow the case schema" || return 1
}

run_case() {
  local half="$1" lvl="$2" id="$3"
  CASE="$CASES/$half/$lvl/$id"
  validate_case "$CASE" "$half" "$lvl" "$id" || return 1
  local kind outcome code target_kind
  kind="$(jq -r .kind "$CASE/case.json")"
  outcome="$(jq -r .expect.outcome "$CASE/case.json")"
  code="$(jq -r '.expect.code // ""' "$CASE/case.json")"
  target_kind="$(jq -r '.target // "directory"' "$CASE/case.json")"

  if [ "$kind" = plan ] && [ "${#PLANNER[@]}" -eq 0 ]; then
    REASON="needs --planner"
    return 2
  fi

  # Sandbox: inputs are copies, so an engine cannot alter the suite.
  C="$(mktemp -d "$WORKROOT/case.XXXXXX")"
  mkdir -p "$C/box/in" "$C/box/l1/l2/cwd" "$C/box/l1/l2/outside" "$C/out" "$C/log"
  TARGET="$C/box/l1/l2/target"
  CWD="$C/box/l1/l2/cwd"
  local f
  for f in plan.json answers.json; do
    if [ -f "$CASE/$f" ]; then cp -- "$CASE/$f" "$C/box/in/$f"; fi
  done
  for f in blobs inputs; do
    if [ -d "$CASE/$f" ]; then cp -a -- "$CASE/$f" "$C/box/in/$f"; else mkdir -p "$C/box/in/$f"; fi
  done
  case "$target_kind" in
    directory)
      mkdir -p "$TARGET"
      if [ -d "$CASE/before" ]; then cp -a -- "$CASE/before/." "$TARGET/"; fi
      ;;
    file) printf 'not a directory\n' >"$TARGET" ;;
    absent) ;;
  esac
  TARGET0="$(manifest "$TARGET" 1)"
  ESCAPE0="$(manifest "$C" 1 "$TARGET" "$C/out" "$C/log")"
  local plan="$C/box/in/plan.json" blobs="$C/box/in/blobs"

  case "$kind" in
    check)
      run_engine check "${KERNEL[@]}" check --plan "$plan" --blobs "$blobs"
      if [ "$outcome" = refused ]; then assert_refused check "$code" || return 1
      else [ "$RC" = 0 ] || fail "check exited $RC; expected 0" || return 1; fi
      assert_target_unchanged || return 1
      ;;
    apply | apply-twice)
      kernel_apply apply1 "$plan" "$blobs" "$C/out/receipt1.json"
      if [ "$outcome" = refused ]; then
        assert_refused apply1 "$code" || return 1
        assert_target_unchanged || return 1
        [ ! -e "$C/out/receipt1.json" ] || fail "a receipt was written although refused" || return 1
      else
        [ "$RC" = 0 ] || fail "apply exited $RC; expected 0 ($(head -c 200 "$C/log/apply1.out"))" || return 1
        assert_tree_expected || return 1
        assert_receipt "$C/out/receipt1.json" "$plan" || return 1
        assert_excludes || return 1
        if [ "$kind" = apply-twice ]; then
          local after
          after="$(manifest "$TARGET" 1)"
          kernel_apply apply2 "$plan" "$blobs" "$C/out/receipt2.json"
          [ "$RC" = 0 ] || fail "second apply exited $RC; expected 0" || return 1
          [ "$(manifest "$TARGET" 1)" = "$after" ] || fail "second apply changed the tree" || return 1
          assert_receipt "$C/out/receipt2.json" "$plan" rerun || return 1
        fi
      fi
      ;;
    instantiate)
      run_engine inst "${KERNEL[@]}" instantiate --plan "$plan" --blobs "$blobs" \
        --answers "$C/box/in/answers.json" --out "$C/out/closed.json" --out-blobs "$C/out/blobs"
      if [ "$outcome" = refused ]; then
        assert_refused inst "$code" || return 1
        [ ! -e "$C/out/closed.json" ] || fail "an instantiated plan was written although refused" || return 1
        assert_target_unchanged || return 1
      else
        [ "$RC" = 0 ] || fail "instantiate exited $RC; expected 0 ($(head -c 200 "$C/log/inst.out"))" || return 1
        [ -f "$C/out/closed.json" ] || fail "no instantiated plan written" || return 1
        is_jcs_file "$C/out/closed.json" lf || fail "instantiated plan is not JCS followed by one line feed" || return 1
        jq -e '.holes == []' <"$C/out/closed.json" >/dev/null || fail "instantiated plan still has holes" || return 1
        kernel_apply apply1 "$C/out/closed.json" "$C/out/blobs" "$C/out/receipt1.json"
        [ "$RC" = 0 ] || fail "apply of the instantiated plan exited $RC" || return 1
        assert_tree_expected || return 1
        assert_receipt "$C/out/receipt1.json" "$C/out/closed.json" || return 1
        assert_excludes || return 1
      fi
      ;;
    plan)
      run_engine plan "${PLANNER[@]}" plan --inputs "$C/box/in/inputs" --out "$C/out/plan.json" --out-blobs "$C/out/blobs"
      if [ "$outcome" = refused ]; then
        assert_refused plan "$code" || return 1
        [ ! -e "$C/out/plan.json" ] || fail "a plan was written although refused" || return 1
      else
        [ "$RC" = 0 ] || fail "planner exited $RC; expected 0" || return 1
        kernel_apply apply1 "$C/out/plan.json" "$C/out/blobs" "$C/out/receipt1.json"
        [ "$RC" = 0 ] || fail "apply of the planned plan exited $RC" || return 1
        assert_tree_expected || return 1
        assert_receipt "$C/out/receipt1.json" "$C/out/plan.json" || return 1
        assert_excludes || return 1
      fi
      ;;
  esac
  assert_escape_free || return 1
  return 0
}

declare -A OK=() BAD=() SKIPPED=()
TOTAL=0
for half in kernel planner; do
  for n in 0 1 2 3; do
    lvl="L$n"
    [ "$n" -le "$MAXLEVEL" ] || continue
    [ -d "$CASES/$half/$lvl" ] || continue
    while IFS= read -r -d '' dir; do
      id="$(basename "$dir")"
      TOTAL=$((TOTAL + 1))
      REASON=""
      status=0
      run_case "$half" "$lvl" "$id" || status=$?
      case "$status" in
        0) OK[$lvl]=$((${OK[$lvl]:-0} + 1)); printf 'ok   %s/%s/%s\n' "$half" "$lvl" "$id" ;;
        2) SKIPPED[$lvl]=$((${SKIPPED[$lvl]:-0} + 1)); printf 'SKIP %s/%s/%s: %s\n' "$half" "$lvl" "$id" "$REASON" ;;
        *) BAD[$lvl]=$((${BAD[$lvl]:-0} + 1)); printf 'FAIL %s/%s/%s: %s\n' "$half" "$lvl" "$id" "${REASON:-unknown}" ;;
      esac
    done < <(find "$CASES/$half/$lvl" -mindepth 1 -maxdepth 1 -type d -print0 | LC_ALL=C sort -z)
  done
done

if [ "$TOTAL" -eq 0 ]; then
  echo "run.sh: no case found at or below $LEVEL under $CASES" >&2
  exit 2
fi

echo "---"
rc=0
for n in 0 1 2 3; do
  [ "$n" -le "$MAXLEVEL" ] || continue
  lvl="L$n"
  o="${OK[$lvl]:-0}" b="${BAD[$lvl]:-0}" s="${SKIPPED[$lvl]:-0}"
  printf '%s: %d ok, %d FAIL, %d SKIP\n' "$lvl" "$o" "$b" "$s"
  if [ "$b" -gt 0 ] || [ "$s" -gt 0 ]; then rc=1; fi
done
if [ "$rc" = 0 ]; then
  echo "level $LEVEL: PASS ($TOTAL cases)"
else
  echo "level $LEVEL: NOT PASSED (a level passes only when every case at and below it passes; skips do not pass)"
fi
exit "$rc"
