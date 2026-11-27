#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# NEGATIVE CONTROL — deliberately wrong. Never use as a kernel.
#
# no-atomicity-kernel.sh: checks the plan itself carefully (JCS, version,
# shape, pins, holes, path safety with an ASCII-only stand-in for NFC, order,
# collisions, blobs), but checks the TARGET one operation at a time and writes
# as it goes. A plan whose later operation fails its precondition therefore
# leaves the earlier writes behind. Its only intended defect is the missing
# check-everything-then-write phase; the suite must FAIL it (selftest.sh).
set -euo pipefail
export LC_ALL=C

refuse() {
  jq -cS -n --arg c "$1" --arg d "$2" '{detail: $d, refused: $c}'
  exit 10
}

cmd="${1:-}"
shift || true
declare -A A=()
while [ "$#" -ge 2 ]; do A["${1#--}"]="$2"; shift 2; done
PLAN="${A[plan]:-}"
BLOBS="${A[blobs]:-}"

sha() { sha256sum <"$1" | cut -c1-64; }

is_j1() { # file
  local canon body
  canon="$(jq -cS . <"$1" 2>/dev/null)" || return 1
  body="$(cat -- "$1"; echo x)"
  [ "$body" = "$canon"$'\n'x ]
}

check_plan() { # $1 = closed|open
  jq -e . "$PLAN" >/dev/null 2>&1 || refuse MALFORMED "not JSON"
  is_j1 "$PLAN" || refuse MALFORMED "not JCS"
  local v
  v="$(jq -r 'if type == "object" and has("plan_version") then .plan_version else "<missing>" end' "$PLAN")"
  [ "$v" != "<missing>" ] || refuse MALFORMED "no plan_version"
  [ "$v" = "0.1" ] || refuse PLAN_VERSION "plan_version $v"
  jq -e '
    def hex($n): type == "string" and test("^[0-9a-f]{\($n)}$");
    def src: type == "object" and keys == ["input", "path"] and (.input | type) == "string" and (.path | type) == "string";
    keys == ["holes", "inputs", "ops", "plan_version"]
    and (.holes | type == "array" and all(type == "string" and test("^[A-Z][A-Z0-9_]*$")))
    and (.inputs | type == "array" and all(type == "object" and keys == ["name", "repo", "rev"]))
    and (.ops | type == "array" and all(
      type == "object" and (.path | type) == "string" and (.source | src)
      and ((.replaces // {input: "", path: ""}) | src)
      and ((.op == "write" and (keys - ["replaces"]) == ["blob", "expect", "mode", "op", "path", "source"]
            and (.blob | type == "string" and test("^sha256:[0-9a-f]{64}$"))
            and (.mode | IN("644", "755"))
            and (.expect == "absent" or (.expect | type == "object" and keys == ["sha256"] and (.sha256 | hex(64))))))
        or (.op == "delete" and (keys - ["replaces"]) == ["expect", "op", "path", "source"]
            and (.expect | type == "object" and keys == ["sha256"] and (.sha256 | hex(64))))))
  ' "$PLAN" >/dev/null || refuse MALFORMED "plan shape"
  jq -e '.inputs | all(.rev | test("^[0-9a-f]{40}$"))' "$PLAN" >/dev/null || refuse SOURCE_UNPINNED "short rev"
  jq -e '[.inputs[].name] as $n | .ops | all((.source.input | IN($n[])) and ((.replaces.input // $n[0]) | IN($n[])))' \
    "$PLAN" >/dev/null || refuse SOURCE_UNPINNED "undeclared input"
  if [ "$1" = closed ] && jq -e '.holes | length > 0' "$PLAN" >/dev/null; then refuse OPEN_PLAN "holes"; fi

  local p prev="" seg folded
  local -A seen=()
  local -a all=()
  while IFS= read -r -d '' p; do
    [ -n "$p" ] || refuse PATH_UNSAFE "empty"
    case "$p" in /* | *\\*) refuse PATH_UNSAFE "absolute or backslash" ;; esac
    [[ ! "$p" =~ [[:cntrl:]] ]] || refuse PATH_UNSAFE "control character"
    [ "$(printf '%s' "$p" | tr -d '\000-\177' | wc -c)" = 0 ] || refuse PATH_UNSAFE "non-ASCII (NFC stand-in)"
    IFS=/ read -r -a segs <<<"$p"
    case "$p" in */) refuse PATH_UNSAFE "empty segment" ;; esac
    for seg in "${segs[@]}"; do
      case "$seg" in "" | . | ..) refuse PATH_UNSAFE "bad segment" ;; esac
      [ "$(printf '%s' "$seg" | tr '[:upper:]' '[:lower:]')" != ".git" ] || refuse PATH_UNSAFE ".git"
    done
    if [ -n "$prev" ]; then
      [ "$prev" != "$p" ] || refuse PATH_DUPLICATE "duplicate"
      [[ "$prev" < "$p" ]] || refuse PATH_DUPLICATE "unsorted"
    fi
    prev="$p"
    folded="$(printf '%s' "$p" | tr '[:upper:]' '[:lower:]')"
    [ -z "${seen[$folded]+x}" ] || refuse PATH_COLLISION "case fold"
    seen[$folded]=1
    all+=("$folded")
  done < <(jq --raw-output0 '.ops[].path' "$PLAN")
  for p in "${all[@]}"; do
    while [[ "$p" == */* ]]; do
      p="${p%/*}"
      [ -z "${seen[$p]+x}" ] || refuse PATH_COLLISION "prefix"
    done
  done

  local h
  while IFS= read -r h; do
    [ -f "$BLOBS/sha256/$h" ] || refuse BLOB_MISSING "$h"
    [ "$(sha "$BLOBS/sha256/$h")" = "$h" ] || refuse BLOB_MISMATCH "$h"
  done < <(jq -r '.ops[] | select(.op == "write") | .blob | ltrimstr("sha256:")' "$PLAN")
}

case "$cmd" in
  check) check_plan open ;;
  instantiate)
    check_plan open
    jq -e 'type == "object" and all(.[]; type == "string")' "${A[answers]}" >/dev/null 2>&1 || refuse MALFORMED "answers"
    jq -e --slurpfile a "${A[answers]}" '.holes | all(. as $h | $a[0] | has($h))' "$PLAN" >/dev/null || refuse HOLE_UNFILLED "unfilled"
    jq -e --slurpfile p "$PLAN" 'keys | all(. as $k | $p[0].holes | index($k))' "${A[answers]}" >/dev/null || refuse HOLE_UNKNOWN "unknown"
    out_plan="$(jq -c '.holes = []' "$PLAN")"
    mkdir -p "${A[out-blobs]}/sha256"
    while IFS= read -r h; do
      content="$(cat -- "$BLOBS/sha256/$h"; echo x)"
      content="${content%x}"
      while IFS=$'\t' read -r k v; do
        content="${content//"{{$k}}"/"$v"}"
      done < <(jq -r 'to_entries[] | "\(.key)\t\(.value)"' "${A[answers]}")
      if printf '%s' "$content" | grep -qE '\{\{[A-Z][A-Z0-9_]*\}\}'; then refuse RESIDUE "token left"; fi
      new="$(printf '%s' "$content" | sha256sum | cut -c1-64)"
      printf '%s' "$content" >"${A[out-blobs]}/sha256/$new"
      out_plan="$(jq -c --arg o "sha256:$h" --arg n "sha256:$new" '(.ops[] | select(.blob == $o) | .blob) |= $n' <<<"$out_plan")"
    done < <(jq -r '.ops[] | select(.op == "write") | .blob | ltrimstr("sha256:")' "$PLAN" | sort -u)
    jq -cS . <<<"$out_plan" >"${A[out]}"
    ;;
  apply)
    check_plan closed
    t="${A[target]}"
    [ ! -L "$t" ] || refuse TARGET_SYMLINK "target is a link"
    [ -d "$t" ] || refuse TARGET_INVALID "target is not a directory"
    results="[]"
    # THE DEFECT: decide and write one operation at a time.
    while IFS=$'\t' read -r op path blob mode expect; do
      a=""
      IFS=/ read -r -a segs <<<"$path"
      for seg in "${segs[@]:0:${#segs[@]}-1}"; do
        a="${a:+$a/}$seg"
        [ ! -L "$t/$a" ] || refuse TARGET_SYMLINK "ancestor $a"
        [ ! -e "$t/$a" ] || [ -d "$t/$a" ] || refuse PRECONDITION "ancestor $a is not a directory"
      done
      leaf="$t/$path"
      [ ! -L "$leaf" ] || refuse TARGET_SYMLINK "leaf"
      if [ "$op" = write ]; then
        want="${blob#sha256:}"
        if [ -f "$leaf" ] && [ "$(sha "$leaf")" = "$want" ] &&
          { [ -x "$leaf" ] && [ "$mode" = 755 ] || { [ ! -x "$leaf" ] && [ "$mode" = 644 ]; }; }; then
          r=unchanged
        elif [ ! -e "$leaf" ]; then
          [ "$expect" = absent ] || refuse PRECONDITION "expected content, absent"
          r=written
        elif [ -f "$leaf" ] && [ "$expect" = "$(sha "$leaf")" ]; then
          r=written
        else
          refuse PRECONDITION "expect does not hold for $path"
        fi
        if [ "$r" = written ]; then
          mkdir -p "$(dirname "$leaf")"
          cp -- "$BLOBS/sha256/$want" "$leaf"
          chmod "$mode" "$leaf"
        fi
      else
        if [ ! -e "$leaf" ]; then
          r=already-absent
        elif [ -f "$leaf" ] && [ "$expect" = "$(sha "$leaf")" ]; then
          rm -f -- "$leaf"
          r=deleted
        else
          refuse PRECONDITION "delete expect does not hold for $path"
        fi
      fi
      results="$(jq -c --arg p "$path" --arg r "$r" '. + [{path: $p, result: $r}]' <<<"$results")"
    done < <(jq -r '.ops[] | [.op, .path, (.blob // "-"), (.mode // "-"),
                    (if .expect == "absent" then "absent" else .expect.sha256 end)] | @tsv' "$PLAN")
    jq -cS -n --argjson o "$results" --arg d "sha256:$(sha "$PLAN")" '{ops: $o, plan: $d}' >"${A[receipt]}"
    ;;
  *) echo "usage: no-atomicity-kernel.sh check|instantiate|apply ..." >&2; exit 2 ;;
esac
