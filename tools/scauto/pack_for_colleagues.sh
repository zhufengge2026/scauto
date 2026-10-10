#!/bin/bash
# Build a colleague-ready copy of scAuto (not the 200+ GB lab project).
#
#   bash tools/scauto/pack_for_colleagues.sh
#   bash tools/scauto/pack_for_colleagues.sh /path/to/scauto_helper
#   INCLUDE_TRIAL=0 bash tools/scauto/pack_for_colleagues.sh   # scripts only
#
# Zip the output folder and send the zip. Never include hpc/config.env.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DEST="${1:-${HOME}/Desktop/scauto_for_colleagues}"
INCLUDE_TRIAL="${INCLUDE_TRIAL:-1}"
TRIAL_SAMPLES=(sampleA sampleB sampleC sampleD)

echo "===== scAuto pack for colleagues ====="
echo "From: ${ROOT}"
echo "To:   ${DEST}"
echo

if [[ ! -f "${ROOT}/viz/palette.R" ]]; then
  echo "Cannot find viz/palette.R. Run this from the lab project that contains tools/scauto." >&2
  exit 1
fi
if [[ ! -f "${ROOT}/tools/scauto/ui/Launch.command" ]]; then
  echo "Cannot find tools/scauto/ui/Launch.command." >&2
  exit 1
fi

mkdir -p "${DEST}/tools" "${DEST}/viz" "${DEST}/resources" "${DEST}/docs" "${DEST}/data"

copy_tree() {
  local src="$1" dst="$2"
  mkdir -p "${dst}"
  if command -v rsync >/dev/null 2>&1; then
    rsync -a --delete --exclude '.DS_Store' --exclude '._*' --exclude '__pycache__' --exclude '.Rhistory' "${src}/" "${dst}/"
  else
    rm -rf "${dst}"
    mkdir -p "$(dirname "${dst}")"
    cp -R "${src}" "${dst}"
  fi
}

copy_tree "${ROOT}/viz" "${DEST}/viz"
if [[ -d "${ROOT}/resources" ]]; then
  copy_tree "${ROOT}/resources" "${DEST}/resources"
fi

if command -v rsync >/dev/null 2>&1; then
  rsync -a --delete \
    --exclude '.DS_Store' \
    --exclude '._*' \
    --exclude '__pycache__' \
    --exclude '.Rhistory' \
    --exclude 'logs/' \
    --exclude 'pack_root/' \
    "${ROOT}/tools/scauto/" "${DEST}/tools/scauto/"
else
  rm -rf "${DEST}/tools/scauto"
  cp -R "${ROOT}/tools/scauto" "${DEST}/tools/scauto"
  rm -rf "${DEST}/tools/scauto/logs" "${DEST}/tools/scauto/pack_root"
fi
mkdir -p "${DEST}/tools/scauto/logs"

for f in USER_MANUAL.md FOR_COLLEAGUES.md FOR_COLLEAGUES.txt INSTALL.md README.md HOW_TO_USE_THE_ANALYSER.txt HOW_TO_PREPARE_DATA.txt HOW_TO_USE_HPC.txt; do
  if [[ -f "${DEST}/tools/scauto/${f}" ]]; then
    cp "${DEST}/tools/scauto/${f}" "${DEST}/docs/${f}"
  fi
done
rm -f "${DEST}/00_HOW_TO_USE.txt" "${DEST}/HOW_TO_USE.txt" "${DEST}/HOW_TO_USE_THE_ANALYSER.txt" "${DEST}/USER_MANUAL.md" \
  "${DEST}/INSTALL.md" "${DEST}/FOR_COLLEAGUES.md" "${DEST}/FOR_COLLEAGUES.txt" \
  "${DEST}/README.md" "${DEST}/00_START_HERE.txt"

if [[ -d "${ROOT}/tools/scauto/pack_root" ]]; then
  cp "${ROOT}/tools/scauto/pack_root/"* "${DEST}/"
fi
chmod +x "${DEST}/1_Install_R.command" "${DEST}/2_Start.command" \
  "${DEST}/tools/scauto/ui/Launch.command" \
  "${DEST}/tools/scauto/install_prereqs.sh" \
  "${DEST}/tools/scauto/pack_for_colleagues.sh" 2>/dev/null || true

if [[ "${INCLUDE_TRIAL}" == "1" ]]; then
  echo "Copying four trial 10x folders (this can take a few minutes)..."
  for s in "${TRIAL_SAMPLES[@]}"; do
    src="${ROOT}/mtx_files_per_sample/${s}"
    if [[ ! -d "${src}" ]]; then
      echo "Missing trial folder: ${src}" >&2
      echo "Set INCLUDE_TRIAL=0 to pack scripts only." >&2
      exit 1
    fi
    copy_tree "${src}" "${DEST}/data/trial_4samples/${s}"
  done
else
  echo "Skipping trial matrices (INCLUDE_TRIAL=0)."
fi

# Do not ship lab HPC accounts even if someone pointed DEST inside the project.
rm -f "${DEST}/hpc/config.env" "${DEST}/tools/scauto/hpc/config.env" 2>/dev/null || true

echo
echo "Done. Send this folder (or a zip of it):"
echo "  ${DEST}"
echo
du -sh "${DEST}" 2>/dev/null || true
echo
echo "Colleague: open 0_START_HERE.txt, then double-click 1_Install_R and 2_Start."
echo "Do not copy the full lab project."
