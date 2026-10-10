# Deprecated conflated modules (do not use alone for annotation)

- `CORE_TPEX` (TCF7+SELL+IL7R+PDCD1+…) — stem genes dominate → false peak in naive/CM
  → use `CORE_NAIVE_STEM` + `CORE_TPEX_CHECKPOINT` (+ AND gate TCF7-high & PDCD1-high)
- `CORE_KLRC2_MEMORY` (KLRC2+TCF7+SELL+…) — same issue
  → use `CORE_KLRC2_NKG2C` + `CORE_NAIVE_STEM` / `CORE_MEMORY_HELPER`
  → phenotype KLRC2+GZMB− requires KLRC2 high AND GZMB low

Updated: 2026-09-07 17:32:29
