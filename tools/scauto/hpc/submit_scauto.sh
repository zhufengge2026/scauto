#!/usr/bin/env bash
# Submit PBMC Auto-Pilot on your HPC OpenPBS.
set -euo pipefail
PROJECT_DIR="${PROJECT_DIR:-$HOME/scauto}"
cd "${PROJECT_DIR}"
# shellcheck disable=SC1091
source "${PROJECT_DIR}/hpc/config.env"
mkdir -p hpc/logs

PBS_BIN="${PBS_BIN:-/opt/pbs/bin/qsub}"
QUEUE="${PBS_QUEUE:-medium}"
PPN="${PBS_PPN:-8}"
WALL="${PBS_WALLTIME:-48:00:00}"
RES="nodes=1:ppn=${PPN},walltime=${WALL}"
if [[ -n "${PBS_MEM:-}" ]]; then
  RES="${RES},mem=${PBS_MEM}"
fi
command -v "${PBS_BIN}" >/dev/null 2>&1 || PBS_BIN="$(command -v qsub)"

OUT="$("${PBS_BIN}" -q "${QUEUE}" -l "${RES}" tools/scauto/hpc/run_scauto.pbs 2>&1)" || {
  echo "qsub FAILED: ${OUT}" >&2
  exit 1
}
echo "Submitted (${QUEUE}, ${RES}): ${OUT}"
echo "Monitor: qstat -u ${USER}"
