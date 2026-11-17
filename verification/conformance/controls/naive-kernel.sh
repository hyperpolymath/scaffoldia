#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# NEGATIVE CONTROL — deliberately wrong. Never use as a kernel.
#
# naive-kernel.sh: writes every operation it is given, with no check at all:
# no JCS or version check, no pin check, no path safety, no blob hashing, no
# preconditions, no symlink check, no hole or residue check. `check` always
# succeeds. The conformance suite must FAIL this kernel (selftest.sh).
set -euo pipefail

cmd="${1:-}"
shift || true
declare -A A=()
while [ "$#" -ge 2 ]; do A["${1#--}"]="$2"; shift 2; done

case "$cmd" in
  check) exit 0 ;;
  instantiate)
    plan="$(jq -c . "${A[plan]}")"
    mkdir -p "${A[out-blobs]}/sha256"
    while IFS= read -r h; do
      content="$(cat -- "${A[blobs]}/sha256/$h"; echo x)"
      content="${content%x}"
      while IFS=$'\t' read -r k v; do
        content="${content//"{{$k}}"/"$v"}"
      done < <(jq -r 'to_entries[] | "\(.key)\t\(.value)"' "${A[answers]}")
      new="$(printf '%s' "$content" | sha256sum | cut -c1-64)"
      printf '%s' "$content" >"${A[out-blobs]}/sha256/$new"
      plan="$(jq -c --arg o "sha256:$h" --arg n "sha256:$new" '(.ops[] | select(.blob == $o) | .blob) |= $n' <<<"$plan")"
    done < <(jq -r '.ops[] | select(.op == "write") | .blob | ltrimstr("sha256:")' <<<"$plan" | sort -u)
    jq -cS '.holes = []' <<<"$plan" >"${A[out]}"
    ;;
  apply)
    t="${A[target]}"
    results="[]"
    while IFS=$'\t' read -r op path blob mode; do
      if [ "$op" = write ]; then
        mkdir -p "$(dirname "$t/$path")"
        cp -- "${A[blobs]}/sha256/${blob#sha256:}" "$t/$path"
        chmod "$mode" "$t/$path"
        r=written
      else
        rm -f -- "$t/$path"
        r=deleted
      fi
      results="$(jq -c --arg p "$path" --arg r "$r" '. + [{path: $p, result: $r}]' <<<"$results")"
    done < <(jq -r '.ops[] | [.op, .path, (.blob // "-"), (.mode // "-")] | @tsv' "${A[plan]}")
    jq -cS -n --argjson o "$results" --arg d "sha256:$(sha256sum <"${A[plan]}" | cut -c1-64)" \
      '{ops: $o, plan: $d}' >"${A[receipt]}"
    ;;
  *) echo "usage: naive-kernel.sh check|instantiate|apply ..." >&2; exit 2 ;;
esac
