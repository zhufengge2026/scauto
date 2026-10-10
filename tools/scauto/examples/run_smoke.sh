#!/usr/bin/env bash
# One-sample smoke test (sampleA, ~4k barcodes) from the project root.
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
cd "${ROOT}"
OUT="${ROOT}/results/scauto_smoke_sampleA"
mkdir -p "${OUT}"
Rscript "${ROOT}/tools/scauto/run_scauto.R" \
  --project "${ROOT}" \
  --input "${ROOT}/mtx_files_per_sample/sampleA" \
  --meta "${ROOT}/tools/scauto/examples/sample_meta.smoke.csv" \
  --outdir "${OUT}"
echo "Smoke output: ${OUT}"
