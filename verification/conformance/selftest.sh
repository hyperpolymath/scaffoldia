#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# selftest.sh — prove the conformance suite has teeth.
#
#   selftest.sh [--level L0|L1|L2|L3] [--cases DIR]
#
# Runs run.sh against every deliberately wrong kernel in controls/
# (*-kernel.sh) and requires the suite to FAIL each of them (run.sh exit 1).
# For each control it lists the cases that caught it. Exits 1 if any control
# passes (the suite is broken) or the runner could not run on it (exit 2), and
# 2 if there are no controls.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LEVEL="L0"
CASES="$HERE/cases"
while [ "$#" -gt 0 ]; do
  case "$1" in
    --level) LEVEL="${2:-}"; shift 2 ;;
    --cases) CASES="${2:-}"; shift 2 ;;
    *) echo "usage: selftest.sh [--level L0|L1|L2|L3] [--cases DIR]" >&2; exit 2 ;;
  esac
done

mapfile -t CONTROLS < <(find "$HERE/controls" -maxdepth 1 -type f -name '*-kernel.sh' | LC_ALL=C sort)
if [ "${#CONTROLS[@]}" -eq 0 ]; then
  echo "selftest: no controls found in $HERE/controls" >&2
  exit 2
fi

status=0
for control in "${CONTROLS[@]}"; do
  name="$(basename "$control")"
  rc=0
  out="$("$HERE/run.sh" --kernel "$control" --level "$LEVEL" --cases "$CASES")" || rc=$?
  caught="$(grep -c '^FAIL ' <<<"$out" || true)"
  total="$(grep -cE '^(ok|FAIL|SKIP) ' <<<"$out" || true)"
  case "$rc" in
    1)
      echo "control $name: FAILED by the suite, as required ($caught of $total cases caught it)"
      grep '^FAIL ' <<<"$out" | sed -E 's/^FAIL ([^:]*): (.*)$/  caught by \1 -- \2/'
      ;;
    0)
      echo "control $name: PASSED $LEVEL -- the suite is broken (a deliberately wrong kernel conforms)"
      status=1
      ;;
    *)
      echo "control $name: runner could not run (exit $rc); not evidence either way"
      tail -3 <<<"$out"
      status=1
      ;;
  esac
done
if [ "$status" = 0 ]; then
  echo "selftest: PASS (${#CONTROLS[@]} controls, each failed by the suite at $LEVEL)"
else
  echo "selftest: FAIL"
fi
exit "$status"
