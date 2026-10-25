#!/bin/zsh
# Runs a DMC re-run (recovery, then correlation) with a chosen amplitude range
# under R 4.5.2, the only local R with dRiftDM. Output is logged to
# output/<RUN_TAG>_run.log, which ends with an "EXIT <code>" line.
# Usage: RUN_TAG=A05-35 A_MIN=0.05 A_MAX=0.35 scripts/run_DMC_narrowA.sh [ncores]
# Defaults (no env vars): RUN_TAG=narrowA, A in [.10, .32].
# Worker processes need local sockets; run outside a sandbox that blocks them.
set -u
cd "$(dirname "$0")/.." || exit 1
export SIM_NCORES="${1:-8}"
export RUN_TAG="${RUN_TAG:-narrowA}"
export A_MIN="${A_MIN:-0.10}"
export A_MAX="${A_MAX:-0.32}"
RH=/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources
LOG="output/${RUN_TAG}_run.log"
mkdir -p output
{
  echo "START $(date)  tag=$RUN_TAG A=[$A_MIN,$A_MAX] ncores=$SIM_NCORES"
  R_HOME=$RH "$RH/bin/exec/R" --vanilla -q -f scripts/Simulation_DMC_Recovery_narrowA.R
  rc1=$?
  echo "RECOVERY_EXIT $rc1 $(date)"
  R_HOME=$RH "$RH/bin/exec/R" --vanilla -q -f scripts/Simulation_DMC_Correlation_narrowA.R
  rc2=$?
  echo "CORRELATION_EXIT $rc2 $(date)"
  if [ "$rc1" -eq 0 ] && [ "$rc2" -eq 0 ]; then echo "EXIT 0 $(date)"; else echo "EXIT 1 $(date)"; fi
} > "$LOG" 2>&1
