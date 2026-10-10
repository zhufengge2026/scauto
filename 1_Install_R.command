#!/bin/bash
# Double-click: install R if missing. Stay in the pack folder.
cd "$(dirname "$0")" || exit 1
chmod +x tools/scauto/install_prereqs.sh 2>/dev/null || true
exec bash tools/scauto/install_prereqs.sh
