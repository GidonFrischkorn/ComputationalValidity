#!/bin/zsh
# Re-runs all four simulations behind the package datasets with the corrected
# split-half reliability (R/get_reliability.R), the ORIGINAL parameter ranges
# (DMC amplitude .015-.400; decision D14, 2026-10-20), fixed seeds, and
# trial-level data stored for the recovery runs. Results go to the
# "relfix"-tagged directories in output/; the original output is untouched.
# Runs under R 4.5.2 (the only local R with dRiftDM); worker processes need
# local sockets, so run outside a sandbox that blocks them.
# Log: output/relfix_run.log, ending with "EXIT <code>".
# Usage: scripts/run_relfix_all.sh [ncores]
set -u
cd "$(dirname "$0")/.." || exit 1
export SIM_NCORES="${1:-8}"
export RUN_TAG=relfix
export A_MIN=0.015
export A_MAX=0.400
RH=/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources
LOG="output/${RUN_TAG}_run.log"
mkdir -p output
rc_all=0
{
  echo "START $(date)  tag=$RUN_TAG DMC A=[$A_MIN,$A_MAX] ncores=$SIM_NCORES"
  for s in Simulation_DMC_Recovery_narrowA Simulation_DMC_Correlation_narrowA \
           Simulation_SSP_Recovery_relfix Simulation_SSP_Correlation_relfix; do
    echo "=== $s $(date)"
    R_HOME=$RH "$RH/bin/exec/R" --vanilla -q -f "scripts/$s.R"
    rc=$?
    echo "${s}_EXIT $rc $(date)"
    [ "$rc" -ne 0 ] && rc_all=1
  done
  echo "EXIT $rc_all $(date)"
} > "$LOG" 2>&1
