#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Enforce canonical estate document-format split:
#   docs/<topic>/ -> AsciiDoc (.adoc)
#   docs/wiki/ and docs/wikis/ -> Markdown (.md) wiki content
# The owner of this rule is proven-cicd-and-workflows/actions/formatting-check.
set -euo pipefail

root="$(cd "${1:-.}" 2>/dev/null && pwd)" || {
  echo 'NOT_RUN: repository root does not exist' >&2
  exit 2
}
command -v git >/dev/null || { echo 'NOT_RUN: git unavailable' >&2; exit 2; }
git -C "$root" rev-parse --show-toplevel >/dev/null 2>&1 || {
  echo 'NOT_RUN: not a Git checkout' >&2
  exit 2
}
if [ ! -d "$root/docs" ]; then
  echo 'NOT_APPLICABLE: repository has no docs/ tree'
  exit 0
fi

checked=0
bad=0
while IFS= read -r -d '' path; do
  case "$path" in
    docs/*.md)
      checked=$((checked+1))
      case "$path" in
        docs/wiki/*.md|docs/wikis/*.md)
          echo "OK: Markdown wiki source: $path"
          ;;
        *)
          echo "FAIL: non-wiki documentation must be AsciiDoc: $path" >&2
          bad=$((bad+1))
          ;;
      esac
      ;;
  esac
done < <(git -C "$root" ls-files -z --cached --others --exclude-standard -- docs)

if [ "$bad" -gt 0 ]; then
  echo "FAIL: $bad Markdown document(s) outside wiki roots; $checked checked" >&2
  exit 1
fi
echo "PASS: $checked Markdown path(s) conform to canonical wiki-only exception"
