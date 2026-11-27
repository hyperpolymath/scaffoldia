#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# NEGATIVE CONTROL — deliberately wrong. Never use as a kernel.
#
# refuse-all-kernel.sh: refuses everything. It never writes, so it can never
# be unsafe; the suite must still FAIL it, or "safe" would be satisfiable by
# doing nothing (selftest.sh).
set -euo pipefail
printf '%s\n' '{"detail":"this control refuses every plan","refused":"MALFORMED"}'
exit 10
