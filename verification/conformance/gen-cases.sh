#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# gen-cases.sh — generate the L0 kernel conformance cases.
#
#   gen-cases.sh            regenerate cases/kernel/L0/ in place
#   gen-cases.sh --check    regenerate into a temporary directory and fail if
#                           the committed cases differ (generate-and-diff)
#
# The cases are committed; this script exists so that blob hashes, plan
# serialisation (JCS + one line feed, via `jq -cS`) and case metadata are
# derived, never typed by hand. Bash + jq + coreutils only. It writes only
# under the output directory.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODE="write"
if [ "${1:-}" = "--check" ]; then MODE="check"; fi

if [ "$MODE" = check ]; then
  TMP="$(mktemp -d)"
  trap 'rm -rf "$TMP"' EXIT
  ROOT="$TMP/cases"
else
  ROOT="$HERE/cases"
fi
L0="$ROOT/kernel/L0"
rm -rf "$L0"
mkdir -p "$L0"

# Fixed pins. The spine name is the publisher whose own name must not appear
# in a child (case child-identity).
REV="4d3c2b1a09f8e7d6c5b4a3928170f6e5d4c3b2a1"
SPINE_REPO="hyperpolymath/rsr-template-repo"

D=""          # current case directory
HOLES="[]"    # holes of the current plan (JSON array)
INPUTS=""     # inputs override (JSON array) or empty for the default

# new_case ID KIND OUTCOME CODE TITLE PRINCIPLE SOURCE [EXTRA-JSON]
new_case() {
  local id="$1" kind="$2" outcome="$3" code="$4" title="$5" principle="$6" source="$7" extra="{}"
  if [ "$#" -ge 8 ]; then extra="$8"; fi
  D="$L0/$id"
  HOLES="[]"
  INPUTS=""
  mkdir -p "$D"
  jq -cS -n --arg id "$id" --arg kind "$kind" --arg outcome "$outcome" --arg code "$code" \
    --arg title "$title" --arg principle "$principle" --arg source "$source" --argjson extra "$extra" '
    {id: $id, kind: $kind, level: "L0", title: $title, principle: $principle, source: $source,
     expect: ({outcome: $outcome} + (if $code == "" then {} else {code: $code} end) + ($extra.expect // {}))}
    + ($extra | del(.expect))' >"$D/case.json"
}

# blob CONTENT -> prints the 64-hex digest; stores blobs/sha256/<hex>.
blob() {
  local tmp h
  mkdir -p "$D/blobs/sha256"
  tmp="$D/blobs/.new"
  printf '%s' "$1" >"$tmp"
  h="$(sha256sum <"$tmp" | cut -c1-64)"
  mv -- "$tmp" "$D/blobs/sha256/$h"
  echo "$h"
}

# sha CONTENT -> 64-hex digest, nothing stored.
sha() { printf '%s' "$1" | sha256sum | cut -c1-64; }

# put ROOT PATH CONTENT [MODE]: put a file in before/ or expected/.
put() {
  local root="$1" p="$2" content="$3" mode="${4:-644}"
  mkdir -p "$(dirname "$D/$root/$p")"
  printf '%s' "$content" >"$D/$root/$p"
  chmod "$mode" "$D/$root/$p"
}

# w PATH DIGEST MODE EXPECT -> a write op (EXPECT: absent | <64 hex>).
w() {
  jq -cn --arg p "$1" --arg b "sha256:$2" --arg m "$3" --arg e "$4" '
    {op: "write", path: $p, blob: $b, mode: $m,
     expect: (if $e == "absent" then "absent" else {sha256: $e} end),
     source: {input: "spine", path: $p}}'
}

# del PATH SHA -> a delete op.
del() {
  jq -cn --arg p "$1" --arg e "$2" '{op: "delete", path: $p, expect: {sha256: $e}, source: {input: "spine", path: $p}}'
}

# plan OP... -> plan.json (JCS + LF), ops in the order given.
plan() {
  local ops
  ops="$(printf '%s\n' "$@" | jq -cs .)"
  local inputs="$INPUTS"
  if [ -z "$inputs" ]; then
    inputs="$(jq -cn --arg r "$SPINE_REPO" --arg v "$REV" '[{name: "spine", repo: $r, rev: $v}]')"
  fi
  jq -cS -n --argjson ops "$ops" --argjson holes "$HOLES" --argjson inputs "$inputs" \
    '{plan_version: "0.1", holes: $holes, inputs: $inputs, ops: $ops}' >"$D/plan.json"
}

# answers JSON-OBJECT -> answers.json (JCS + LF).
answers() { jq -cS -n --argjson a "$1" '$a' >"$D/answers.json"; }

# A user file present in most starting trees, so any stray write shows.
KEEP=$'user content, not planned\n'
keep_before() { put before keep.txt "$KEEP"; }
keep_expected() { put expected keep.txt "$KEEP"; }

README=$'= Example\n\nA minimal repository.\n'
INDEX=$'= Documentation\n'
SCRIPT=$'#!/bin/sh\necho ok\n'

SRC_README="conformance README, Plan format v0.1"
SRC_SAFE="conformance README, Safe paths"
SRC_APPLY="conformance README, Apply"
SRC_INST="conformance README, Instantiate"
SRC_ADR2="ADR-0002, Safety invariants and Acceptance evidence"
SRC_ADR3="ADR-0003, Acceptance evidence"

# ── accepted plans ──────────────────────────────────────────────────────────

new_case mint-minimal apply ok "" "Minimal valid mint into an empty target" \
  "A closed, safe plan applied to an empty target yields exactly the planned tree" "$SRC_ADR3" \
  '{"expect":{"results":{"README.adoc":"written","docs/index.adoc":"written"}}}'
h1="$(blob "$README")"; h2="$(blob "$INDEX")"
plan "$(w README.adoc "$h1" 644 absent)" "$(w docs/index.adoc "$h2" 644 absent)"
put expected README.adoc "$README"; put expected docs/index.adoc "$INDEX"

new_case apply-twice-idempotent apply-twice ok "" "Applying a plan twice equals applying it once" \
  "Idempotence: the second application leaves the tree identical and reports only unchanged/already-absent" "$SRC_ADR2" \
  '{"expect":{"results":{"README.adoc":"written","docs/index.adoc":"written","old.txt":"deleted","scripts/run.sh":"written"}}}'
OLD=$'obsolete\n'
h1="$(blob "$README")"; h2="$(blob "$INDEX")"; h3="$(blob "$SCRIPT")"
plan "$(w README.adoc "$h1" 644 absent)" "$(w docs/index.adoc "$h2" 644 absent)" \
  "$(del old.txt "$(sha "$OLD")")" "$(w scripts/run.sh "$h3" 755 absent)"
keep_before; put before old.txt "$OLD"
keep_expected; put expected README.adoc "$README"; put expected docs/index.adoc "$INDEX"
put expected scripts/run.sh "$SCRIPT" 755

new_case already-realised apply ok "" "A path that already has the desired content is unchanged" \
  "A write whose desired state already holds is a no-op even though expect says absent" "$SRC_APPLY" \
  '{"expect":{"results":{"README.adoc":"unchanged","docs/index.adoc":"written"}}}'
h1="$(blob "$README")"; h2="$(blob "$INDEX")"
plan "$(w README.adoc "$h1" 644 absent)" "$(w docs/index.adoc "$h2" 644 absent)"
put before README.adoc "$README"
put expected README.adoc "$README"; put expected docs/index.adoc "$INDEX"

new_case update-expected-content apply ok "" "A write whose expected base content matches replaces it" \
  "expect {sha256} authorises replacing exactly the content the planner saw" "$SRC_APPLY" \
  '{"expect":{"results":{"README.adoc":"written"}}}'
OLDREADME=$'= Old\n'
h1="$(blob "$README")"
plan "$(w README.adoc "$h1" 644 "$(sha "$OLDREADME")")"
put before README.adoc "$OLDREADME"; keep_before
put expected README.adoc "$README"; keep_expected

new_case delete-already-absent apply ok "" "Deleting a path that is not there is a no-op" \
  "A delete whose desired state (absent) already holds reports already-absent" "$SRC_APPLY" \
  '{"expect":{"results":{"gone.txt":"already-absent"}}}'
plan "$(del gone.txt "$(sha $'was here\n')")"
keep_before; keep_expected

new_case mode-755 apply ok "" "Executable mode 755 is honoured" \
  "The mode of each write is part of the desired state" "$SRC_README" \
  '{"expect":{"results":{"README.adoc":"written","bin/tool.sh":"written"}}}'
h1="$(blob "$README")"; h2="$(blob "$SCRIPT")"
plan "$(w README.adoc "$h1" 644 absent)" "$(w bin/tool.sh "$h2" 755 absent)"
put expected README.adoc "$README"; put expected bin/tool.sh "$SCRIPT" 755

new_case check-open-plan check ok "" "check accepts a well-formed open plan" \
  "An open plan is a valid plan; only apply requires it closed" "$SRC_README"
HOLES='["OWNER"]'
h1="$(blob $'= Repository of {{OWNER}}\n')"
plan "$(w README.adoc "$h1" 644 absent)"

new_case instantiate-two-holes instantiate ok "" "Instantiation fills two holes across two blobs" \
  "Every declared token is replaced; the result is a closed plan with recomputed hashes" "$SRC_INST" \
  '{"expect":{"results":{"NOTICE.adoc":"written","README.adoc":"written"}}}'
HOLES='["OWNER","REPO"]'
h1="$(blob $'= {{REPO}}\n')"; h2="$(blob $'Maintained by {{OWNER}}.\n')"
plan "$(w NOTICE.adoc "$h2" 644 absent)" "$(w README.adoc "$h1" 644 absent)"
answers '{"OWNER":"alice","REPO":"widget"}'
put expected README.adoc $'= widget\n'; put expected NOTICE.adoc $'Maintained by alice.\n'

new_case child-identity instantiate ok "" "A child minted from a published plan carries its own identity" \
  "Child identity is distinct from the publisher's: the publisher's own name appears nowhere in the child" \
  "ADR-0002, Safety invariants (child identity); ADR-0003, Tier 0" \
  '{"expect":{"tree_excludes":["rsr-template-repo","hyperpolymath"]}}'
HOLES='["OWNER","REPO"]'
h1="$(blob $'= {{REPO}}\n\nA repository of {{OWNER}}, minted from a published plan.\n')"
h2="$(blob $'forge: github.com/{{OWNER}}/{{REPO}}\nname: {{REPO}}\n')"
h3="$(blob $'CI note: at run time the identity comes from ${{ github.repository }}\n')"
plan "$(w README.adoc "$h1" 644 absent)" "$(w docs/ci-note.txt "$h3" 644 absent)" "$(w identity.txt "$h2" 644 absent)"
answers '{"OWNER":"example-org","REPO":"example-child"}'
put expected README.adoc $'= example-child\n\nA repository of example-org, minted from a published plan.\n'
put expected identity.txt $'forge: github.com/example-org/example-child\nname: example-child\n'
put expected docs/ci-note.txt $'CI note: at run time the identity comes from ${{ github.repository }}\n'

# ── refusals: plan shape ────────────────────────────────────────────────────

new_case malformed-not-json apply refused MALFORMED "A plan that is not JSON" \
  "Only a v0.1 plan is accepted" "$SRC_README"
printf 'write README.adoc please\n' >"$D/plan.json"
keep_before

new_case malformed-not-jcs apply refused MALFORMED "Valid JSON that is not JCS-serialised" \
  "A plan is I-JSON serialised by JCS; one plan has one byte form, so one digest" "$SRC_README"
h1="$(blob "$README")"
plan "$(w README.adoc "$h1" 644 absent)"
jq . "$D/plan.json" >"$D/plan.pretty" && mv -- "$D/plan.pretty" "$D/plan.json"
keep_before

new_case malformed-missing-field apply refused MALFORMED "A write without a mode" \
  "Every write carries path, blob, mode, expect and source" "$SRC_README"
h1="$(blob "$README")"
plan "$(w README.adoc "$h1" 644 absent | jq -c 'del(.mode)')"
keep_before

new_case malformed-unknown-field apply refused MALFORMED "An operation with an unknown member" \
  "A kernel does not ignore what it does not understand (there is no overwrite-anyway)" "$SRC_README"
h1="$(blob "$README")"
plan "$(w README.adoc "$h1" 644 absent | jq -c '. + {overwrite: true}')"
put before README.adoc "$OLD"

new_case plan-version apply refused PLAN_VERSION "A plan version this kernel does not implement" \
  "An unknown plan version is refused, not guessed" "$SRC_README"
h1="$(blob "$README")"
plan "$(w README.adoc "$h1" 644 absent)"
jq -cS '.plan_version = "0.2"' "$D/plan.json" >"$D/plan.v2" && mv -- "$D/plan.v2" "$D/plan.json"
keep_before

new_case unpinned-short-rev apply refused SOURCE_UNPINNED "An input pinned by an abbreviated commit id" \
  "Sources are pinned by full commit id, never inferred" "$SRC_ADR2"
INPUTS="[{\"name\":\"spine\",\"repo\":\"$SPINE_REPO\",\"rev\":\"4d3c2b1\"}]"
h1="$(blob "$README")"
plan "$(w README.adoc "$h1" 644 absent)"
keep_before

new_case unpinned-undeclared-input apply refused SOURCE_UNPINNED "An operation naming an undeclared input" \
  "Every write has provenance in a pinned input" "$SRC_ADR2"
h1="$(blob "$README")"
plan "$(w README.adoc "$h1" 644 absent | jq -c '.source.input = "elsewhere"')"
keep_before

# ── refusals: paths ─────────────────────────────────────────────────────────

unsafe_case() { # ID TITLE PATH
  new_case "$1" apply refused PATH_UNSAFE "$2" \
    "No write outside the target or into repository metadata" "$SRC_SAFE"
  local h1
  h1="$(blob $'planted\n')"
  local hr
  hr="$(blob "$README")"
  plan "$(w "$3" "$h1" 644 absent)" "$(w README.adoc "$hr" 644 absent)"
  keep_before
}
unsafe_case path-unsafe-dotdot "A path with a '..' segment" "../escape.txt"
unsafe_case path-unsafe-absolute "An absolute path" "/dev/null/escape.txt"
unsafe_case path-unsafe-backslash "A path with a backslash" 'docs\escape.txt'
unsafe_case path-unsafe-empty-segment "A path with an empty segment" "docs//escape.txt"
unsafe_case path-unsafe-dot-git "A path into .git" ".git/hooks/post-checkout"
unsafe_case path-unsafe-dot-git-case "A path into .GIT (case variant of .git)" ".GIT/hooks/post-checkout"
unsafe_case path-unsafe-control-char "A path with a control character" $'bad\001name.txt'
unsafe_case path-unsafe-non-nfc "A path not in Unicode NFC" $'cafe\xcc\x81.txt'

new_case path-duplicate apply refused PATH_DUPLICATE "Two operations on one path" \
  "A plan maps each path to at most one operation" "$SRC_README"
h1="$(blob "$README")"; h2="$(blob "$INDEX")"
plan "$(w README.adoc "$h1" 644 absent)" "$(w README.adoc "$h2" 644 absent)"
keep_before

new_case path-unsorted apply refused PATH_DUPLICATE "Operations not sorted by path" \
  "Operations are sorted bytewise by path, so a plan has one canonical order" "$SRC_README"
h1="$(blob "$README")"; h2="$(blob "$INDEX")"
plan "$(w b.adoc "$h1" 644 absent)" "$(w a.adoc "$h2" 644 absent)"
keep_before

new_case path-collision-case-fold apply refused PATH_COLLISION "Two paths equal after case folding" \
  "No path alias or case collision" "$SRC_ADR2"
h1="$(blob "$README")"; h2="$(blob "$INDEX")"
plan "$(w README.adoc "$h1" 644 absent)" "$(w readme.adoc "$h2" 644 absent)"
keep_before

new_case path-collision-prefix apply refused PATH_COLLISION "A file path that is a directory prefix of another" \
  "No path is a proper directory prefix of another" "$SRC_SAFE"
h1="$(blob "$README")"; h2="$(blob "$INDEX")"
plan "$(w docs "$h1" 644 absent)" "$(w docs/index.adoc "$h2" 644 absent)"
keep_before

# ── refusals: blobs, holes ──────────────────────────────────────────────────

new_case blob-missing apply refused BLOB_MISSING "A write whose blob is absent" \
  "Content is never inline and never fetched: every blob is present" "$SRC_README"
plan "$(w README.adoc "$(sha "$README")" 644 absent)"
keep_before

new_case blob-mismatch apply refused BLOB_MISMATCH "A blob whose content does not match its name" \
  "Content is addressed by SHA-256 and checked before use" "$SRC_README"
h1="$(sha "$README")"
mkdir -p "$D/blobs/sha256"; printf 'tampered\n' >"$D/blobs/sha256/$h1"
plan "$(w README.adoc "$h1" 644 absent)"
keep_before

new_case open-plan apply refused OPEN_PLAN "Apply of a plan with holes" \
  "Only a closed plan can be applied" "$SRC_README"
HOLES='["OWNER"]'
h1="$(blob $'= Repository of {{OWNER}}\n')"
plan "$(w README.adoc "$h1" 644 absent)"
keep_before

new_case hole-unfilled instantiate refused HOLE_UNFILLED "A hole without an answer" \
  "Every hole must have an answer" "$SRC_INST"
HOLES='["OWNER","REPO"]'
h1="$(blob $'= {{REPO}} of {{OWNER}}\n')"
plan "$(w README.adoc "$h1" 644 absent)"
answers '{"OWNER":"alice"}'

new_case hole-unknown instantiate refused HOLE_UNKNOWN "An answer that names no hole" \
  "Every answer must name a hole" "$SRC_INST"
HOLES='["OWNER"]'
h1="$(blob $'= Repository of {{OWNER}}\n')"
plan "$(w README.adoc "$h1" 644 absent)"
answers '{"OWNER":"alice","REPO":"widget"}'

new_case residue instantiate refused RESIDUE "An undeclared token survives instantiation" \
  "A child must not carry unfilled template text" "$SRC_INST"
HOLES='["OWNER"]'
h1="$(blob $'= Repository of {{OWNER}}\n')"; h2="$(blob $'Licence holder: {{LICENSOR}}\n')"
plan "$(w NOTICE.adoc "$h2" 644 absent)" "$(w README.adoc "$h1" 644 absent)"
answers '{"OWNER":"alice"}'

new_case residue-from-answer instantiate refused RESIDUE "An answer that itself contains a token" \
  "Substitution is one pass and residue is checked on the result, so an answer cannot smuggle in template text" "$SRC_INST"
HOLES='["OWNER","REPO"]'
h1="$(blob $'= {{REPO}} of {{OWNER}}\n')"
plan "$(w README.adoc "$h1" 644 absent)"
answers '{"OWNER":"{{REPO}}","REPO":"widget"}'

# From the engine-neutral mint-defect list (work order T6, row SC-T-002).
new_case instantiate-literal-answer instantiate ok "" "An answer with shell and sed metacharacters renders literally" \
  "Answers are data: substituted byte for byte, never interpreted" \
  "rsr-template-repo tests/e2e/use_template_mint_test.sh (existing control: answer with '&' and '|'), as listed in T6 SC-T-002" \
  '{"expect":{"results":{"README.adoc":"written"}}}'
HOLES='["OWNER"]'
h1="$(blob $'= Repository of {{OWNER}}\n')"
plan "$(w README.adoc "$h1" 644 absent)"
# shellcheck disable=SC2016 # the dollar and backticks are literal answer text
answers '{"OWNER":"a&b|c\\1 $HOME `x` {{ y }} /z"}'
put expected README.adoc $'= Repository of a&b|c\\1 $HOME `x` {{ y }} /z\n'

# ── refusals: target state ──────────────────────────────────────────────────

new_case precondition-expect-absent apply refused PRECONDITION "expect absent, but a file with other content exists" \
  "No silent overwrite: there is no overwrite-whatever-is-there" "$SRC_ADR2"
h1="$(blob "$README")"
plan "$(w README.adoc "$h1" 644 absent)"
put before README.adoc $'= My own README\n'

new_case precondition-expect-sha apply refused PRECONDITION "expect sha256, but the content differs" \
  "A replacement needs exactly the base content the planner saw" "$SRC_APPLY"
h1="$(blob "$README")"
plan "$(w README.adoc "$h1" 644 "$(sha $'= Old\n')")"
put before README.adoc $'= Old, then edited by the user\n'

new_case precondition-expect-sha-absent apply refused PRECONDITION "expect sha256, but the path is absent" \
  "A replacement of something that is not there is not a creation" "$SRC_APPLY"
h1="$(blob "$README")"
plan "$(w README.adoc "$h1" 644 "$(sha $'= Old\n')")"
keep_before

new_case precondition-delete-wrong-sha apply refused PRECONDITION "Delete of a file whose content differs" \
  "A delete removes only the content the planner saw" "$SRC_APPLY"
plan "$(del old.txt "$(sha $'obsolete\n')")"
put before old.txt $'obsolete, but edited by the user\n'

new_case precondition-ancestor-file apply refused PRECONDITION "A planned path under an existing file" \
  "A planned path needs directories, not files, above it" "$SRC_APPLY"
h1="$(blob "$INDEX")"
plan "$(w docs/index.adoc "$h1" 644 absent)"
put before docs $'a file, not a directory\n'

new_case all-or-nothing apply refused PRECONDITION "The last operation fails its precondition" \
  "All or nothing: a refused plan leaves the target byte-identical, including the first operations" "$SRC_APPLY"
h1="$(blob $'first\n')"; h2="$(blob $'second\n')"; h3="$(blob $'third\n')"
plan "$(w a.txt "$h1" 644 absent)" "$(w b/b.txt "$h2" 755 absent)" "$(w c.txt "$h3" 644 absent)"
put before c.txt $'the user wrote this\n'

new_case target-symlink apply refused TARGET_SYMLINK "An ancestor directory is a link pointing outside" \
  "No symlink escape: links are never followed when writing" "$SRC_ADR2"
h1="$(blob "$README")"; h2="$(blob "$INDEX")"
plan "$(w README.adoc "$h1" 644 absent)" "$(w docs/index.adoc "$h2" 644 absent)"
mkdir -p "$D/before"; ln -s ../outside "$D/before/docs"

new_case target-symlink-leaf apply refused TARGET_SYMLINK "The planned path itself is a link pointing outside" \
  "No symlink escape: a link at a planned path is not read through or written through" "$SRC_ADR2"
h1="$(blob "$README")"
plan "$(w README.adoc "$h1" 644 "$(sha $'= Old\n')")"
mkdir -p "$D/before"; ln -s ../outside/README.adoc "$D/before/README.adoc"

new_case target-invalid-absent apply refused TARGET_INVALID "The target does not exist" \
  "Apply needs an existing target directory; it does not create one" "$SRC_APPLY" '{"target":"absent"}'
h1="$(blob "$README")"
plan "$(w README.adoc "$h1" 644 absent)"

new_case target-invalid-file apply refused TARGET_INVALID "The target is a regular file" \
  "Apply needs an existing target directory" "$SRC_APPLY" '{"target":"file"}'
h1="$(blob "$README")"
plan "$(w README.adoc "$h1" 644 absent)"

new_case check-path-unsafe check refused PATH_UNSAFE "check refuses an unsafe path" \
  "check applies the same safety rules as apply" "$SRC_SAFE"
h1="$(blob "$README")"
plan "$(w ../README.adoc "$h1" 644 absent)"

if [ "$MODE" = check ]; then
  if diff -r --no-dereference "$HERE/cases/kernel/L0" "$L0" >"$TMP/diff" 2>&1; then
    echo "gen-cases: committed cases match the generator ($(find "$L0" -name case.json | wc -l) cases)"
  else
    echo "gen-cases: committed cases differ from the generator:" >&2
    head -40 "$TMP/diff" >&2
    exit 1
  fi
else
  echo "gen-cases: wrote $(find "$L0" -name case.json | wc -l) cases under $L0"
fi
