#!/bin/bash
# Double-click: open Single-cell analysis autopilot.
cd "$(dirname "$0")" || exit 1
chmod +x tools/scauto/ui/Launch.command 2>/dev/null || true
exec bash tools/scauto/ui/Launch.command
