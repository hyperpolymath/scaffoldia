#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Verify that every non-local GitHub Action is pinned directly to a 40-hex
# commit or is resolved to a full commit in the generated gh actions-lock file.
# Do not modify actions.lock by hand.
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
if [ "$#" -gt 0 ]; then root="$(cd "$1" && pwd)"; fi
lock="$root/.github/workflows/actions.lock"
if [ ! -s "$lock" ]; then
  echo "FAIL: missing generated actions.lock: $lock" >&2
  exit 1
fi

fail=0
checked=0
shopt -s nullglob
for file in "$root"/.github/workflows/*.yml "$root"/.github/workflows/*.yaml; do
  rel=".github/workflows/$(basename "$file")"
  while IFS= read -r line; do
    # Comments (including workflow examples) do not match the anchored selector.
    spec="$(printf '%s\n' "$line" | sed -nE 's/^[[:space:]]*(-[[:space:]]*)?uses:[[:space:]]*([^[:space:]#]+).*$/\2/p')"
    [ -n "$spec" ] || continue
    checked=$((checked + 1))
    case "$spec" in
      ./*) continue ;;
      docker://*) echo "FAIL: $rel uses a Docker action without a reviewed digest: $spec" >&2; fail=1; continue ;;
    esac

    source="$(printf '%s\n' "$spec" | cut -d@ -f1)"
    version="$(printf '%s\n' "$spec" | awk -F@ '{print $NF}')"
    if [ "$source" = "$spec" ] || ! [[ "$source" == */* ]]; then
      echo "FAIL: $rel uses an action without an immutable ref: $spec" >&2
      fail=1
      continue
    fi
    # Native full-SHA uses (including reusable workflows) are immutable.
    if [[ "$version" =~ ^[0-9a-f]{40}$ ]]; then continue; fi
    # A tag is allowed only when the generated lock includes a matching
    # workflow reference AND an exact ref->sha1-40 dependency record.
    owner="$(printf '%s\n' "$source" | cut -d/ -f1)"
    name="$(printf '%s\n' "$source" | cut -d/ -f2)"
    key="$owner/$name@$version"

    if ! awk -v wf="    '$rel':" -v key="        - '$key'" '
      $0 == "dependencies:" {exit}
      $0 == wf {inside=1; next}
      inside && /^    [^ ]/ {inside=0}
      inside && $0 == key {found=1}
      END {exit(found ? 0 : 1)}
    ' "$lock"; then
      echo "FAIL: $rel: $key is missing from actions.lock workflow entries" >&2
      fail=1
      continue
    fi

    record="$(awk -v key="    '$key':" '
      $0 == "dependencies:" {deps=1; next}
      deps && $0 == key {inside=1; next}
      inside && /^    [^ ]/ {exit}
      inside {print}
    ' "$lock")"
    if ! printf '%s\n' "$record" | grep -Fxq "        ref: '$version'" ||
       ! printf '%s\n' "$record" | grep -Eq "^[[:space:]]+commit: 'sha1-[0-9a-f]{40}'$"; then
      echo "FAIL: $rel: lock entry $key lacks matching ref and 40-hex SHA" >&2
      fail=1
    fi
  done < "$file"
done
[ "$checked" -gt 0 ] || { echo "FAIL: no active action uses found" >&2; exit 1; }
[ "$fail" -eq 0 ] || exit 1
echo "PASS: $checked uses resolve to direct 40-hex pins or generated actions.lock SHA records"
