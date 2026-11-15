#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Policy controls: permitted wiki source, forbidden other Markdown, absent root.
set -euo pipefail
checker="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../scripts" && pwd)/check-no-md-in-docs.sh"
tmp="$(mktemp -d)"
trap 'rm -rf -- "$tmp"' EXIT
git -C "$tmp" init -q
mkdir -p "$tmp/docs/wikis" "$tmp/docs/architecture" "$tmp/docs/wiki/sub"
printf '# Wiki\n' > "$tmp/docs/wikis/README.md"
printf '# Wiki page\n' > "$tmp/docs/wiki/sub/page.md"
printf '= Architecture\n' > "$tmp/docs/architecture/README.adoc"
git -C "$tmp" add docs
expect() {
  local name="$1" want="$2" got=0
  bash "$checker" "$tmp" > "$tmp/check.log" 2>&1 || got=$?
  if [ "$got" -ne "$want" ]; then
    echo "FAIL: $name: expected $want, got $got" >&2
    cat "$tmp/check.log" >&2
    exit 1
  fi
  echo "PASS: $name (exit $got)"
}
expect 'silence: wiki Markdown and architecture AsciiDoc' 0
printf '# Wrong format\n' > "$tmp/docs/architecture/NOTES.md"
git -C "$tmp" add docs/architecture/NOTES.md
expect 'firing: non-wiki Markdown' 1
git -C "$tmp" rm -q -f docs/architecture/NOTES.md
expect 'recovery: removing invalid format clears gate' 0
if bash "$checker" "$tmp/does-not-exist" >/dev/null 2>&1; then
  echo 'FAIL: absent repository counted as conformant' >&2
  exit 1
fi
echo 'PASS: wiki format silence/firing/unavailable controls'
