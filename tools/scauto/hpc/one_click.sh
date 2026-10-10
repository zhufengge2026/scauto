#!/usr/bin/env bash
# Run on the colleague's computer (not on a PBS node).
# Uploads the pipeline + ticked 10x folders, submits OpenPBS, waits, pulls results.
#
#   bash tools/scauto/hpc/one_click.sh /path/to/job.env
#   bash tools/scauto/hpc/one_click.sh --resume /path/to/job.env
#
# --resume: do not upload or qsub. Wait for the PBS id in logs/hpc_job.id
# and copy results back (after the laptop was shut down mid-job).
#
# job.env is written by the helper. Needs ssh (BatchMode / key) and rsync or scp.
set -euo pipefail

RESUME=0
ENV_FILE=""
for arg in "$@"; do
  case "${arg}" in
    --resume) RESUME=1 ;;
    -*) echo "Unknown option: ${arg}" >&2; exit 2 ;;
    *) ENV_FILE="${arg}" ;;
  esac
done
: "${ENV_FILE:?Need job.env path}"
if [[ ! -f "${ENV_FILE}" ]]; then
  echo "Missing job.env: ${ENV_FILE}" >&2
  exit 2
fi
# shellcheck disable=SC1090
source "${ENV_FILE}"

: "${HPC_SSH:?}"
: "${REMOTE_PROJECT:?}"
: "${LOCAL_PROJECT:?}"
: "${LOCAL_SCAUTO:?}"
: "${LOCAL_VIZ:?}"
: "${LOCAL_OUT:?}"
: "${LOCAL_META:?}"
: "${SAMPLE_MAP:?}"
: "${REMOTE_INPUT:?}"
: "${REMOTE_OUT:?}"
: "${SAMPLES:?}"

NCPUS="${NCPUS:-8}"
MEM_GB="${MEM_GB:-64}"
WALLTIME="${WALLTIME:-48:00:00}"
POLL_SEC="${POLL_SEC:-75}"
POLL_RUN_SEC="${POLL_RUN_SEC:-20}"
R_MODULE="${R_MODULE:-R}"
PBS_QUEUE="${PBS_QUEUE:-medium}"
PBS_BIN_REMOTE="${PBS_BIN_REMOTE:-/opt/pbs/bin}"

if [[ "${POLL_SEC}" -lt 45 ]]; then POLL_SEC=45; fi
if [[ "${POLL_SEC}" -gt 120 ]]; then POLL_SEC=120; fi
if [[ "${POLL_RUN_SEC}" -lt 15 ]]; then POLL_RUN_SEC=15; fi
if [[ "${POLL_RUN_SEC}" -gt 45 ]]; then POLL_RUN_SEC=45; fi

LOG="${LOCAL_LOG:-${LOCAL_OUT}/logs/pipeline.log}"
mkdir -p "$(dirname "${LOG}")" "${LOCAL_OUT}/logs"
JOB_ID_FILE="${LOCAL_OUT}/logs/hpc_job.id"
WATCHER_PID_FILE="${LOCAL_OUT}/logs/hpc_watcher.pid"
echo "$$" > "${WATCHER_PID_FILE}"

log() {
  local msg
  msg="$(date '+%Y-%m-%d %H:%M:%S') $*"
  echo "${msg}"
  # nohup already appends stdout to LOG; only tee when running in a terminal.
  if [[ -t 1 ]]; then
    echo "${msg}" >> "${LOG}"
  fi
}

fail() {
  log "SCAUTO_HPC_STAGE=failed"
  log "========== ANALYSIS DID NOT FINISH =========="
  log "$*"
  exit 1
}

# -n only on plain ssh (so a `while read` sample list is not eaten).
# Never put -n on RSYNC_RSH: rsync talks to the remote over that ssh pipe.
# Reuse one TCP session (ControlMaster). your HPC drops new handshakes if we
# open a fresh ssh/rsync for every sample (MaxStartups / connection reset).
CTL_DIR="$(mktemp -d "${TMPDIR:-/tmp}/scauto-ssh.XXXXXX")"
CTL="${CTL_DIR}/cm"
SSH_BASE=(
  -o BatchMode=yes
  -o ConnectTimeout=25
  -o ServerAliveInterval=30
  -o ServerAliveCountMax=8
  -o LogLevel=ERROR
  -o ControlMaster=auto
  -o ControlPath="${CTL}"
  -o ControlPersist=600
)
if ssh -o BatchMode=yes -o ConnectTimeout=1 -o StrictHostKeyChecking=accept-new -G example.com >/dev/null 2>&1; then
  SSH_BASE+=(-o StrictHostKeyChecking=accept-new)
fi
SSH_CMD=(-n "${SSH_BASE[@]}")
export RSYNC_RSH="ssh ${SSH_BASE[*]}"

close_ssh_mux() {
  ssh -n -O exit -o ControlPath="${CTL}" "${HPC_SSH}" >/dev/null 2>&1 || true
  rm -rf "${CTL_DIR}"
}
trap close_ssh_mux EXIT

ssh_hpc() {
  ssh "${SSH_CMD[@]}" "${HPC_SSH}" "$@"
}

has_rsync=0
if command -v rsync >/dev/null 2>&1; then
  has_rsync=1
fi

run_rsync() {
  local attempt=1
  local max=6
  local wait
  while true; do
    if rsync -a --partial --exclude '.DS_Store' --exclude '.git' "$@"; then
      return 0
    fi
    if [[ "${attempt}" -ge "${max}" ]]; then
      return 1
    fi
    wait=$((attempt * 4))
    log "rsync connection dropped (attempt ${attempt}/${max}); waiting ${wait}s"
    sleep "${wait}"
    attempt=$((attempt + 1))
  done
}

push_dir() {
  local src="$1"
  local dest="$2"
  if [[ ! -d "${src}" ]]; then
    fail "Local folder missing: ${src}"
  fi
  ssh_hpc "mkdir -p '${dest}'"
  if [[ "${has_rsync}" -eq 1 ]]; then
    run_rsync "${src}/" "${HPC_SSH}:${dest}/" || fail "rsync failed: ${src} → ${dest}"
  else
    scp -q -r -o BatchMode=yes "${src}/." "${HPC_SSH}:${dest}/" \
      || fail "scp failed: ${src} → ${dest}"
  fi
}

push_file() {
  local src="$1"
  local dest="$2"
  ssh_hpc "mkdir -p '$(dirname "${dest}")'"
  if [[ "${has_rsync}" -eq 1 ]]; then
    run_rsync "${src}" "${HPC_SSH}:${dest}" || fail "rsync failed: ${src} → ${dest}"
  else
    scp -q -o BatchMode=yes "${src}" "${HPC_SSH}:${dest}" \
      || fail "scp failed: ${src} → ${dest}"
  fi
}

pull_dir() {
  local src="$1"
  local dest="$2"
  mkdir -p "${dest}"
  if [[ "${has_rsync}" -eq 1 ]]; then
    run_rsync "${HPC_SSH}:${src}/" "${dest}/"
  else
    scp -q -r -o BatchMode=yes "${HPC_SSH}:${src}/." "${dest}/"
  fi
}

# One attempt: copy the compute-node pipeline.log so the helper progress bar can move.
# Must not fail the watcher if the file is not there yet (queued / early R).
pull_compute_log() {
  local dest="${LOCAL_OUT}/logs/compute.log"
  mkdir -p "${LOCAL_OUT}/logs"
  if [[ "${has_rsync}" -eq 1 ]]; then
    rsync -a --partial "${HPC_SSH}:${REMOTE_OUT}/logs/pipeline.log" "${dest}" >/dev/null 2>&1 || true
  else
    scp -q -o BatchMode=yes "${HPC_SSH}:${REMOTE_OUT}/logs/pipeline.log" "${dest}" >/dev/null 2>&1 || true
  fi
}

wall_to_sec() {
  local w="$1"
  local h m s
  h="${w%%:*}"
  local rest="${w#*:}"
  m="${rest%%:*}"
  s="${rest##*:}"
  echo $((10#${h} * 3600 + 10#${m} * 60 + 10#${s}))
}

# Live qstat -f is empty once the job leaves the queue. Fall back to qstat -x
# so we do not wait four poll cycles, and so SSH blips are not "job vanished".
pbs_job_state() {
  local live hist
  live="$(ssh_hpc "export PATH='${PBS_BIN_REMOTE}':\"\$PATH\"
qstat -f '${JOBID}' 2>/dev/null | awk -F= '/job_state/ { gsub(/[[:space:]]/,\"\",\$2); print \$2; exit }'
" 2>/dev/null || true)"
  live="$(echo "${live}" | tr -d '[:space:]')"
  if [[ -n "${live}" ]]; then
    echo "${live}"
    return 0
  fi
  hist="$(ssh_hpc "export PATH='${PBS_BIN_REMOTE}':\"\$PATH\"
qstat -xf '${JOBID}' 2>/dev/null | awk -F= '/job_state/ { gsub(/[[:space:]]/,\"\",\$2); print \$2; exit }'
" 2>/dev/null || true)"
  hist="$(echo "${hist}" | tr -d '[:space:]')"
  if [[ -n "${hist}" ]]; then
    echo "${hist}"
    return 0
  fi
  echo ""
  return 1
}

log "SCAUTO_HPC_STAGE=ssh"
if [[ "${RESUME}" -eq 1 ]]; then
  log "Resume: wait for the existing PBS job and copy results. Will not upload or qsub."
else
  log "HPC one-click: host=${HPC_SSH} remote=${REMOTE_PROJECT}"
fi
ssh -n "${SSH_BASE[@]}" -o ControlMaster=yes -fN "${HPC_SSH}" \
  || fail "SSH failed. In Terminal run once: ssh ${HPC_SSH}   then accept the host key. Use a key (ssh-copy-id), not a password prompt."
ssh_hpc "echo SCAUTO_SSH_OK user=\$USER home=\$HOME" >/dev/null \
  || fail "SSH session is up but a test command failed."

if [[ "${RESUME}" -eq 1 ]]; then
  if [[ ! -f "${JOB_ID_FILE}" ]]; then
    fail "Resume needs logs/hpc_job.id from the original Submit. Missing: ${JOB_ID_FILE}"
  fi
  JOBID="$(tr -d '[:space:]' < "${JOB_ID_FILE}")"
  if [[ -z "${JOBID}" ]]; then
    fail "logs/hpc_job.id is empty. Cannot resume."
  fi
  log "Resume PBS job ${JOBID} to ${LOCAL_OUT}"
  ST0="$(pbs_job_state || true)"
  ST0="$(echo "${ST0}" | tr -d '[:space:]')"
  case "${ST0}" in
    Q|H) log "SCAUTO_HPC_STAGE=queued" ;;
    R|E) log "SCAUTO_HPC_STAGE=running" ;;
    F|C) log "Job already finished on the cluster; copying results." ;;
    "") log "Could not read qstat yet; will retry while waiting." ;;
    *) log "qstat state=${ST0}" ;;
  esac
else

log "SCAUTO_HPC_STAGE=upload"
log "Creating folders on the cluster and copying the analysis scripts (not the whole project)."
ssh_hpc "mkdir -p '${REMOTE_PROJECT}/hpc/logs' '${REMOTE_PROJECT}/results' '${REMOTE_PROJECT}/Rlib' '${REMOTE_INPUT}' '${REMOTE_OUT}'"

if [[ ! -f "${LOCAL_VIZ}/palette.R" || ! -f "${LOCAL_VIZ}/pbmc_lineage_markers.json" ]]; then
  fail "Local viz/ is incomplete (need palette.R and pbmc_lineage_markers.json): ${LOCAL_VIZ}"
fi
push_dir "${LOCAL_SCAUTO}" "${REMOTE_PROJECT}/tools/scauto"
log "Uploading viz/ (palette + marker JSON) to ${REMOTE_PROJECT}/viz"
push_dir "${LOCAL_VIZ}" "${REMOTE_PROJECT}/viz"
if [[ -n "${LOCAL_CONFIG_ENV:-}" && -f "${LOCAL_CONFIG_ENV}" ]]; then
  # Do not overwrite the lab hpc/config.env (it still has Mac LOCAL_PROJECT_DIR).
  push_file "${LOCAL_CONFIG_ENV}" "${REMOTE_PROJECT}/hpc/scauto_oneclick.env"
fi
if [[ -n "${LOCAL_PBS:-}" && -f "${LOCAL_PBS}" ]]; then
  push_file "${LOCAL_PBS}" "${REMOTE_PROJECT}/hpc/run_scauto.pbs"
fi
if [[ -n "${LOCAL_ADVANCED_YAML:-}" && -f "${LOCAL_ADVANCED_YAML}" ]]; then
  push_file "${LOCAL_ADVANCED_YAML}" "${REMOTE_PROJECT}/hpc/scauto_advanced.yaml"
  log "Uploaded Advanced Settings overlay (hpc/scauto_advanced.yaml)"
fi
push_file "${LOCAL_META}" "${REMOTE_INPUT}/sample_meta.csv"
ssh_hpc "test -f '${REMOTE_PROJECT}/viz/palette.R' && test -f '${REMOTE_PROJECT}/viz/pbmc_lineage_markers.json'" \
  || fail "viz/ did not arrive on the cluster (palette.R / pbmc_lineage_markers.json). Re-run Submit to HPC."

log "Copying ticked 10x sample folders (matrix / barcodes / features only for those samples)."
if [[ ! -f "${SAMPLE_MAP}" ]]; then
  fail "Sample map missing: ${SAMPLE_MAP}"
fi
sample_mkdir=""
while IFS=$'\t' read -r samp path || [[ -n "${samp:-}" ]]; do
  [[ -z "${samp:-}" || "${samp}" == \#* ]] && continue
  sample_mkdir="${sample_mkdir} '${REMOTE_INPUT}/${samp}'"
done < "${SAMPLE_MAP}"
if [[ -n "${sample_mkdir}" ]]; then
  ssh_hpc "mkdir -p ${sample_mkdir}"
fi
n_samp=0
while IFS=$'\t' read -r samp path || [[ -n "${samp:-}" ]]; do
  [[ -z "${samp:-}" ]] && continue
  [[ "${samp}" == \#* ]] && continue
  n_samp=$((n_samp + 1))
  log "  upload ${samp}"
  if [[ ! -d "${path}" ]]; then
    fail "Sample folder missing: ${path}"
  fi
  push_dir "${path}" "${REMOTE_INPUT}/${samp}"
done < "${SAMPLE_MAP}"
if [[ "${n_samp}" -lt 1 ]]; then
  fail "No samples listed in ${SAMPLE_MAP}"
fi
log "Uploaded ${n_samp} sample folder(s). Checking they are all on the cluster…"
missing_remote=""
while IFS=$'\t' read -r samp path || [[ -n "${samp:-}" ]]; do
  [[ -z "${samp:-}" || "${samp}" == \#* ]] && continue
  if ! ssh_hpc "test -f '${REMOTE_INPUT}/${samp}/matrix.mtx' || test -f '${REMOTE_INPUT}/${samp}/matrix.mtx.gz'"; then
    missing_remote="${missing_remote} ${samp}"
  fi
done < "${SAMPLE_MAP}"
if [[ -n "${missing_remote}" ]]; then
  fail "These sample folders did not arrive on the cluster:${missing_remote}. Re-run Submit to HPC."
fi
log "All ${n_samp} sample folder(s) are on the cluster."

log "SCAUTO_HPC_STAGE=install_pkgs"
log "Checking R packages on the login node (compute nodes have no internet). First time can take 20–40 minutes."
ssh_hpc "export PATH='${PBS_BIN_REMOTE}':\"\$PATH\"
set -euo pipefail
export PROJECT_DIR='${REMOTE_PROJECT}'
export LOCAL_PROJECT_DIR=\"\$PROJECT_DIR\"
cd \"\$PROJECT_DIR\"
if [[ -f /usr/share/lmod/lmod/init/bash ]]; then
  # shellcheck disable=SC1091
  set +u
  source /usr/share/lmod/lmod/init/bash
  set -u
fi
if [[ -f hpc/config.env ]]; then
  # shellcheck disable=SC1091
  source hpc/config.env
fi
if [[ -f hpc/scauto_oneclick.env ]]; then
  # shellcheck disable=SC1091
  source hpc/scauto_oneclick.env
fi
# Restore cluster paths after sourcing (lab config.env points LOCAL_PROJECT_DIR at a Mac).
export PROJECT_DIR='${REMOTE_PROJECT}'
export LOCAL_PROJECT_DIR=\"\$PROJECT_DIR\"
module purge 2>/dev/null || true
module load '${R_MODULE}'
export R_LIBS_USER=\"\$PROJECT_DIR/Rlib\"
mkdir -p \"\$R_LIBS_USER\"
if [[ ! -f viz/palette.R || ! -f viz/pbmc_lineage_markers.json ]]; then
  echo \"SCAUTO_VIZ_MISSING under \$PROJECT_DIR\" >&2
  ls -la viz 2>/dev/null || true
  exit 2
fi
if [[ -f \"\$R_LIBS_USER/.scauto_ok\" ]]; then
  echo SCAUTO_PKGS_CACHED
  exit 0
fi
Rscript tools/scauto/check_install.R --install --project \"\$PROJECT_DIR\"
date > \"\$R_LIBS_USER/.scauto_ok\"
echo SCAUTO_PKGS_INSTALLED
" || fail "Login-node setup failed (R packages or project files). The log above is the real error — often viz/ was not visible because LOCAL_PROJECT_DIR still pointed at a Mac path. Re-run Submit to HPC."

log "SCAUTO_HPC_STAGE=submit"
# Do not pass qsub -v: OpenPBS on your HPC errors with
# "cannot send environment with the job" (login env is large; -v also
# breaks if SCAUTO_SAMPLES contains commas). Paths are already in the PBS file.
QSUB_OUT="$(ssh_hpc "export PATH='${PBS_BIN_REMOTE}':\"\$PATH\"
cd '${REMOTE_PROJECT}'
mkdir -p hpc/logs
QSUB=\$(command -v qsub || echo '${PBS_BIN_REMOTE}/qsub')
\"\$QSUB\" -q '${PBS_QUEUE}' -N scauto \\
  -l select=1:ncpus=${NCPUS}:mem=${MEM_GB}gb -l walltime=${WALLTIME} \\
  hpc/run_scauto.pbs 2>&1
")" || fail "qsub failed: ${QSUB_OUT:-}"

JOBID="$(echo "${QSUB_OUT}" | tr -d '[:space:]')"
if [[ -z "${JOBID}" ]]; then
  fail "qsub returned no job id: ${QSUB_OUT}"
fi
echo "${JOBID}" > "${JOB_ID_FILE}"
log "Submitted OpenPBS job ${JOBID} (queue ${PBS_QUEUE}, ${NCPUS} CPUs, ${MEM_GB} GB, walltime ${WALLTIME})"
log "SCAUTO_HPC_STAGE=queued"
log "SCAUTO_HPC_JOB=${JOBID}"

fi

MAX_WAIT=$(( $(wall_to_sec "${WALLTIME}") + 7200 ))
ELAPSED=0
net_fail=0
last_st=""
compute_log_noted=0
while true; do
  ST="$(pbs_job_state || true)"
  ST="$(echo "${ST}" | tr -d '[:space:]')"
  if [[ -z "${ST}" ]]; then
    net_fail=$((net_fail + 1))
    if [[ "${net_fail}" -ge 6 ]]; then
      log "Could not read qstat after ${net_fail} tries; will still try to copy any results."
      break
    fi
    log "Could not read qstat try ${net_fail}; will retry."
    sleep "${POLL_SEC}"
    ELAPSED=$((ELAPSED + POLL_SEC))
    continue
  fi
  net_fail=0
  if [[ "${ST}" != "${last_st}" ]]; then
    case "${ST}" in
      Q)
        log "qstat ${JOBID} state=Q waiting in queue"
        log "SCAUTO_HPC_STAGE=queued"
        ;;
      H)
        log "qstat ${JOBID} state=H held"
        log "SCAUTO_HPC_STAGE=queued"
        ;;
      R)
        log "qstat ${JOBID} state=R running on a compute node"
        log "SCAUTO_HPC_STAGE=running"
        ;;
      E)
        log "qstat ${JOBID} state=E exiting"
        log "SCAUTO_HPC_STAGE=running"
        ;;
      F|C)
        log "qstat ${JOBID} state=${ST} finished"
        break
        ;;
      *)
        log "qstat ${JOBID} state=${ST} leaving wait loop"
        break
        ;;
    esac
    last_st="${ST}"
  fi
  wait_sec="${POLL_SEC}"
  case "${ST}" in
    R|E)
      pull_compute_log
      if [[ "${compute_log_noted}" -eq 0 && -f "${LOCAL_OUT}/logs/compute.log" ]]; then
        log "Live analysis log: logs/compute.log"
        compute_log_noted=1
      fi
      wait_sec="${POLL_RUN_SEC}"
      ;;
  esac
  sleep "${wait_sec}"
  ELAPSED=$((ELAPSED + wait_sec))
  if [[ "${ELAPSED}" -gt "${MAX_WAIT}" ]]; then
    fail "Timed out waiting for job ${JOBID}. Check: ssh ${HPC_SSH} qstat -x ${JOBID}"
  fi
done
pull_compute_log

EXIT_ST="$(ssh_hpc "export PATH='${PBS_BIN_REMOTE}':\"\$PATH\"
qstat -xf '${JOBID}' 2>/dev/null | awk -F= '/Exit_status/ { gsub(/[[:space:]]/,\"\",\$2); print \$2; exit }'
" 2>/dev/null || true)"
EXIT_ST="$(echo "${EXIT_ST}" | tr -d '[:space:]')"
if [[ -n "${EXIT_ST}" ]]; then
  log "qstat Exit_status=${EXIT_ST}"
fi

log "SCAUTO_HPC_STAGE=pull"
log "Copying results back to this computer: ${LOCAL_OUT}"
pull_dir "${REMOTE_OUT}" "${LOCAL_OUT}" || fail "rsync/scp of results failed. Job may have finished; try copying ${REMOTE_OUT} by hand."

# PBS stdout (R errors) is not always in pipeline.log — copy the job .OU next to local logs.
PBS_OU="$(ssh_hpc "ls -t '${REMOTE_PROJECT}/hpc/logs'/*.OU 2>/dev/null | head -1" || true)"
PBS_OU="$(echo "${PBS_OU}" | tr -d '[:space:]')"
if [[ -n "${PBS_OU}" ]]; then
  pull_one="${LOCAL_OUT}/logs/pbs_job.out"
  mkdir -p "${LOCAL_OUT}/logs"
  if [[ "${has_rsync}" -eq 1 ]]; then
    rsync -a --partial "${HPC_SSH}:${PBS_OU}" "${pull_one}" || true
  else
    scp -q -o BatchMode=yes "${HPC_SSH}:${PBS_OU}" "${pull_one}" || true
  fi
  if [[ -f "${pull_one}" ]]; then
    log "PBS job log saved to logs/pbs_job.out"
    if [[ -n "${EXIT_ST}" && "${EXIT_ST}" != "0" ]]; then
      log "----- last lines of PBS job log -----"
      tail -n 40 "${pull_one}" || true
    fi
  fi
fi

if [[ -n "${EXIT_ST}" && "${EXIT_ST}" != "0" ]]; then
  fail "HPC job Exit_status=${EXIT_ST}. Results that exist were copied. See logs/pipeline.log and logs/pbs_job.out."
fi
if [[ ! -f "${LOCAL_OUT}/REPORT.md" && ! -f "${LOCAL_OUT}/run_manifest.json" ]]; then
  log "Warning: REPORT.md / run_manifest.json not found yet — job may have failed before writing outputs."
fi

log "SCAUTO_HPC_STAGE=done"
log "SCAUTO_HPC_DONE"
log "========== ANALYSIS FINISHED =========="
log "Results are in ${LOCAL_OUT}"
exit 0
