# Literature module species / tissue audit (CD8Tu lineage)

Policy for this project PBMC analyses: curated functional modules should derive from
**human peripheral blood** (or human blood±tissue scRNA-seq), not mouse-only experiments.

## Actions taken (2026-09-07)

| Item | Issue | Action |
|------|-------|--------|
| Mogilenko et al. Immunity 2021 | Primary aging immune atlas is **mouse**; human PBMC used only as secondary validation | **Replaced** by Terekhova et al. Immunity 2023 (human PBMC aging atlas, ~2M cells) |
| Sarkar et al. 2008 (GSE10239) MSigDB C7 | **Mouse** LCMV CD8 effector/memory microarray | **Removed** from `cd8_mait_gdt` and `cd8_mait` catalogs |
| Andreatta et al. 2021 ProjecTILs | Reference atlas is **mouse-heavy** TIL | **Reattributed** CORE_Tpex citation → Kotliar et al. 2025 (human consensus programs) |

## Replacement module

- **Terekhova et al. 2023** — *Immunity* 56:2836–2854.e9. DOI `10.1016/j.immuni.2023.12.014`, PMID 37963456.
- Human blood scRNA/TCR/BCR + feature barcoding; reports age-associated **GZMK+ CD8** accumulation.
- Gene panel (circulating GZMK+ CD8 memory): GZMK, EOMES, CMC1, CCL5, CST7, CD74, DTHD1, CXCR3, CD27
  (aligned with human PBMC markers; dropped mouse-tilted TOX/PDCD1/ITGAE/CXCR6 from Mogilenko panel).

## Kept with caveats (human, but not pure PBMC)

| Citation | Species | Tissue | Verdict |
|----------|---------|--------|---------|
| Hao et al. Cell 2021 (Azimuth) | Human | **PBMC** | Prefer |
| Terekhova et al. Immunity 2023 | Human | **PBMC** | Prefer |
| Garner et al. Nat Immunol 2023 | Human | Blood ± liver MAIT | Acceptable |
| Gray et al. Sci Immunol 2024 | Human | Multi-tissue γδT (incl. blood) | Acceptable; residency modules are tissue-biased |
| Zheng et al. Science 2021 | Human | Tumor TIL | Acceptable for Tex/exhaustion; not PBMC |
| Kotliar et al. Nat Methods 2025 | Human | Consensus T programs | Prefer for activation/exhaustion |
| Allen Immune Health Atlas 2024 | Human | Blood atlas resource | Prefer for TEMRA taxonomy |
| Liberzon / GO / KEGG | Cross-species databases | N/A | Acceptable as pathway sets |
| Abbas et al. 2005 (GSE22886) | Human | Sorted immune microarray | Acceptable (not scRNA) |

## Other lineages (not changed in this pass)

- **Crinier et al. 2018** (NK catalog): human+mouse comparative NK atlas — human modules used; note dual-species paper.


## 2023–2026 expansion (2026-09-07)

Added curated modules from human blood / CD8 atlases after systematic query blocks in `SEARCH_STRATEGY.md`:

| Paper | Year | Role |
|-------|------|------|
| Terekhova et al. Immunity | 2023 | GZMK+ CD8; KLRC2+GZMB−; type-2 CCR4+ CD8 |
| Garner et al. Nat Immunol | 2023 | MAIT blood/liver programs (already present) |
| Gray et al. Sci Immunol | 2024 | γδT lifespan (already present) |
| Allen Immune Health Atlas | 2024 | TEMRA taxonomy (already present) |
| Wang et al. Nat Immunol | 2025 | Lifespan PBMC; GNLY+/CMC1+ CD8 TEM |
| Xue et al. Nat Methods | 2025 | Human CD8 atlas; CMC1 vs GNLY Temra; innate-like CD8 |
| Kotliar et al. Nat Methods | 2025 | Consensus T programs (already present) |

Excluded as primary sources: Mogilenko 2021 (mouse-primary), Sarkar 2008 (mouse), Andreatta 2021 (mouse TIL), Chandra 2023 (mouse-dominant MAIT periphery).
