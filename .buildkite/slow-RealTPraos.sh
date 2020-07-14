#!/usr/bin/env bash

set -euo pipefail

# The test output includes non-ASCII characters, so this setting may prevent an
# error `<stdout>: commitBuffer: invalid argument (invalid character)`.
#
# tl;dr If running this script locally, it seems best to run it inside of the
# nix-shell. This provides the necessary locales (eg `en_US.utf8`) and usually
# also selects one by default.
#
# This setting seems unnecessary on the BuildKite instance; everything there
# wisely defaults to UTF-8. It also appears to be redundant when running this
# script from inside of a nix-shell. We set it only to make the script more
# robust.
export LANG=en_US.utf8

# What we want this job's artifact to be named in the BuildKite UI
logfile=artifact-slow-RealTPraos.log

# Always upload the log file
function finish {
    # TODO how to specify the charset: UTF-8? The browser renders the artifact
    # poorly when it's clicked in the UI. BuildKite v3 might allow it via
    # `--content-type`, but it seems we're still running v2, which doesn't.
    buildkite-agent artifact upload "$logfile"
}
trap finish EXIT

# The directory containing the repository's default.nix
nixdir="$(dirname $0)/.."

# Build/fetch the exe that runs the RealTPraos tests
nix build -f "$nixdir" \
    haskellPackages.ouroboros-consensus-shelley-test.components.tests.test \
    -o the-test

# Run the RealTPraos tests with the nightly flag set
./the-test/bin/test \
    --pattern RealTPraos \
    --iohk-enable-nightly-tests \
  1>"$logfile" 2>&1
