#!/bin/bash
# Double-click to open PBMC Auto-Pilot (macOS).
cd "$(dirname "$0")/../../.." || exit 1
ROOT="$(pwd)"
HERE="$(cd "$(dirname "$0")" && pwd)"

find_rscript() {
  if command -v Rscript >/dev/null 2>&1; then
    command -v Rscript
    return 0
  fi
  for p in \
    /usr/local/bin/Rscript \
    /opt/homebrew/bin/Rscript \
    /Library/Frameworks/R.framework/Resources/bin/Rscript
  do
    if [[ -x "${p}" ]]; then
      echo "${p}"
      return 0
    fi
  done
  return 1
}

RSCRIPT="$(find_rscript || true)"
if [[ -z "${RSCRIPT}" ]]; then
  BTN="$(osascript -e 'display dialog "R is not installed on this Mac. Install R now? (Needs internet. Analysis packages download later when you click Start analysis.)" buttons {"Cancel","Install R"} default button "Install R"' -e 'button returned of result' 2>/dev/null || true)"
  if [[ "${BTN}" == "Install R" ]]; then
    bash "${ROOT}/tools/scauto/install_prereqs.sh" || {
      osascript -e 'display dialog "Install did not finish. See tools/scauto/INSTALL.md" buttons {"OK"} default button 1'
      exit 1
    }
    RSCRIPT="$(find_rscript || true)"
  fi
fi
if [[ -z "${RSCRIPT}" ]]; then
  osascript -e 'display dialog "Rscript was not found. Double-click tools/scauto/install_prereqs.sh or see INSTALL.md" buttons {"OK"} default button 1'
  exit 1
fi
# Finder PATH is short; put this Rscript first so the GUI can start analysis.
export PATH="$(dirname "${RSCRIPT}"):${PATH}"
# Force macOS LaunchServices (Safari/Chrome), not XQuartz / R browseURL.
# XQuartz DISPLAY looks remote to R; leave it unset so the page can open.
unset DISPLAY
export R_BROWSER="/usr/bin/open"
export BROWSER="/usr/bin/open"

"${RSCRIPT}" "${HERE}/launch.R"
status=$?
echo
if [[ "${status}" -ne 0 && "${status}" -ne 130 ]]; then
  echo "The helper did not start (exit ${status}). Copy any error above."
  echo "If the window printed an address, paste it into Safari or Chrome."
  read -r -p "Press Enter to close."
  exit "${status}"
fi
