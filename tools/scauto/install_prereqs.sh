#!/bin/bash
# Install R if it is missing. Analysis packages install when you click
# Start analysis in the helper. macOS / Linux. Needs internet. Safe to re-run.
#
#   bash tools/scauto/install_prereqs.sh
#   or double-click Launch.command — it will offer this if R is missing.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "${ROOT}"

echo "===== PBMC Auto-Pilot: install R ====="
echo "Project: ${ROOT}"
echo "Analysis packages download later when you click Start analysis."
echo

find_rscript() {
  if command -v Rscript >/dev/null 2>&1; then
    command -v Rscript
    return 0
  fi
  for p in \
    /usr/local/bin/Rscript \
    /opt/homebrew/bin/Rscript \
    /Library/Frameworks/R.framework/Resources/bin/Rscript \
    /usr/bin/Rscript
  do
    if [[ -x "${p}" ]]; then
      echo "${p}"
      return 0
    fi
  done
  return 1
}

open_cran() {
  local url="https://cran.r-project.org"
  case "$(uname -s)" in
    Darwin) url="https://cran.r-project.org/bin/macosx/"; open "${url}" ;;
    Linux)
      url="https://cran.r-project.org/bin/linux/"
      if command -v xdg-open >/dev/null 2>&1; then xdg-open "${url}"; fi
      ;;
  esac
  echo "Opened: ${url}"
}

RSCRIPT="$(find_rscript || true)"
if [[ -z "${RSCRIPT}" ]]; then
  echo "R is not installed on this computer."
  SYS="$(uname -s)"
  if [[ "${SYS}" == "Darwin" ]] && command -v brew >/dev/null 2>&1; then
    echo "Homebrew found. Installing R with: brew install r"
    brew install r
  elif [[ "${SYS}" == "Linux" ]] && command -v apt-get >/dev/null 2>&1; then
    echo "Installing R with apt (may ask for your password)..."
    sudo apt-get update
    sudo apt-get install -y r-base r-base-dev
  else
    echo "Opening the official R download page in your browser."
    echo "Install R 4.3 or newer, then come back here and press Enter."
    open_cran || true
    echo
    read -r -p "Press Enter after R is installed..." _
  fi
  RSCRIPT="$(find_rscript || true)"
fi

if [[ -z "${RSCRIPT}" ]]; then
  echo
  echo "Still cannot find Rscript."
  echo "Install R from https://cran.r-project.org then run this script again."
  echo "Details: tools/scauto/INSTALL.md"
  exit 1
fi

echo "Using: ${RSCRIPT}"
"${RSCRIPT}" --version | head -1
echo
echo "R is ready. Open the helper; click Start analysis to install Seurat if needed."
echo "  Mac:     double-click 2_Start.command  (or tools/scauto/ui/Launch.command)"
echo "  Windows: double-click 2_Start.bat      (or tools/scauto/ui/Launch.bat)"
echo "  Any OS:  ${RSCRIPT} ${ROOT}/tools/scauto/ui/launch.R"
