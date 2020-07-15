#!/usr/bin/env bash

set -euo pipefail

# For example, `replicate 3 s` behaves as `{ echo s; echo s; echo s; }`
#
# Note: Any newlines in `s` will be converted to spaces.
function replicate {
    # The `true` prevents a pipefail from ending the script.
    { yes "$2" || true; } | head "-n$1" | tr '\n' ' '
}

# Where to accumulate log files
logdir="slow-tests-logs-${BUILDKITE_JOB_ID:-local}"
mkdir -p "$logdir"
find "$logdir" -name '*.log' -delete

# Always do these final steps
function finish {
    cd "$logdir"

    # Dump to stdout the last line of each log file
    tail -n1 $(find . -name '*.log' | sort)

    if [ -n "${BUILDKITE-}" ]; then
        # Upload the log files as BuildKite artifacts
        #
        # TODO how to specify the charset: UTF-8? The browser renders the
        # artifact poorly when it's clicked in the UI. BuildKite v3 might allow
        # it via `--content-type`, but it seems we're still running v2, which
        # doesn't.
	buildkite-agent artifact upload "./**/*.log"
    fi
}
trap finish EXIT

# The directory containing the repository's default.nix
nixdir="$(dirname $0)/.."

# Ensure the invocations below of Nix-built exe see and use the desired locale
#
# See https://nixos.org/nixpkgs/manual/#locales
nix build -f "$nixdir" nightly-checks.glibcLocales -o glibcLocales
export LOCALE_ARCHIVE=$(readlink -m glibcLocales)/lib/locale/locale-archive
export LC_ALL=en_US.utf8

# We use GNU parallel to manage multiple processes
#
# This job runs on the benchmarking BuildKite queue, which means it'll be
# running alone on the machine. We want to keep the CPU saturated, eg.
nix build -f "$nixdir" nightly-checks.gnuparallel -o gnuparallel-exe

# Build/fetch the exe that runs the RealTPraos ThreadNet tests
nix build -f "$nixdir" nightly-checks.RealTPraos -o RealTPraos-exe

# GNU parallel will run multiple instances of this job
function eachJob {
    unique_job_id="$(printf %03d $PARALLEL_SEQ)"
    logdir="$1"
    n="$2"

    # Run the RealTPraos tests with the nightly flag set
    ./RealTPraos-exe/bin/test \
        --pattern RealTPraos \
        --quickcheck-tests="$n" \
        --iohk-enable-nightly-tests \
      1>"${logdir}/RealTPraos_${unique_job_id}.log" 2>&1
}
export -f eachJob   # now visible toprocesses spawned by GNU parallel

# The number of *physical* cores on this machine
ncores="$(./gnuparallel-exe/bin/parallel --number-of-cores)"

# How many tests to run per job, and how many jobs to run
#
# For example `7 10 5` would run three jobs, each with the given number of
# tests.
#
# At time of writing, 100 RealTPraos tests takes approximately 700s on the
# BuildKite instance reserved for benchmarking.
#
# See https://buildkite.com/input-output-hk/ouroboros-network-nightly/builds/137#63e36ccb-1eb5-4aee-8034-2e73f67f1251
#
# So 1000 should take about two hours.
#
# We only test in multiples of 100 because otherwise it risks skewing the
# QuickCheck generator distribution (note that QuickCheck sets `maxSize stdArgs
# = 100`).
#
# We transition from a few jobs with several tests to several jobs with few
# tests; this improves the batch's worst-case CPU utilization.
qcSizes="$(replicate $(expr 10 '*' $ncores) 100)"

# Run the jobs, but never more than the number of physical cores at once
./gnuparallel-exe/bin/parallel "-j$ncores" eachJob "$logdir" ::: $qcSizes
