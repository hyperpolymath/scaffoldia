#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# Validate the required STATE.a2ml metadata section.
# Exit contract: 0 valid; 1 invalid project metadata; 2 unable to inspect input.
set -uo pipefail

if [ "$#" -gt 1 ]; then
    echo "validate-state: expected at most one STATE.a2ml path" >&2
    exit 2
fi

state="${1:-.machine_readable/descriptiles/STATE.a2ml}"
if [ ! -e "$state" ] || [ ! -f "$state" ]; then
    echo "STATE.a2ml: INVALID (required file missing or not a regular file: $state)" >&2
    exit 1
fi
if [ ! -r "$state" ]; then
    echo "STATE.a2ml: INERT/SETUP (file is not readable: $state)" >&2
    exit 2
fi

metadata_seen=0
project_seen=0
in_metadata=0

while IFS= read -r line || [ -n "$line" ]; do
    line="${line#"${line%%[![:space:]]*}"}"

    if [[ "$line" =~ ^\[([A-Za-z0-9_-]+)\][[:space:]]*(#.*)?$ ]]; then
        section="${BASH_REMATCH[1]}"
        if [ "$section" = "metadata" ]; then
            metadata_seen=1
            in_metadata=1
        else
            in_metadata=0
        fi
        continue
    fi

    if [ "$in_metadata" -eq 1 ] && [[ "$line" =~ ^project[[:space:]]*=[[:space:]]*(.*)$ ]]; then
        value="${BASH_REMATCH[1]}"
        value="${value#"${value%%[![:space:]]*}"}"
        value="${value%"${value##*[![:space:]]}"}"
        if [ -n "$value" ] && [ "$value" != '""' ] && [ "$value" != "''" ] && [[ ! "$value" =~ ^# ]]; then
            project_seen=1
        fi
    fi
done < "$state"

if [ "$metadata_seen" -ne 1 ]; then
    echo "STATE.a2ml: INVALID (missing [metadata] section)" >&2
    exit 1
fi
if [ "$project_seen" -ne 1 ]; then
    echo "STATE.a2ml: INVALID (missing non-empty project declaration inside [metadata])" >&2
    exit 1
fi

echo "STATE.a2ml: valid"
