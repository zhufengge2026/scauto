# Functional modules: where they come from

Auto-Pilot does **not** query PubMed or GO/KEGG while a run is in progress.
Gene lists are fixed files in this project.

## What the subtype heatmap uses

Compact panels (typically 4–16 genes) **curated from human blood / PBMC scRNA-seq papers**
(2016–2026), for example Hao/Azimuth 2021, Kotliar TCAT 2025, Crinier 2018 NK atlas,
Villani 2017 DC atlas, Rebuffet 2024 NK, Terekhova 2023, Garner 2023 MAIT, Gray 2024 γδT.
These rows have `source_type` containing `curated` and `used_in_heatmap = TRUE`.
Citations, DOI and PMID are in `literature_gene_sets_catalog.csv`.

Files on disk:

- `resources/literature_modules/<lineage>/catalog.csv` + `genes.csv`
- extra panels: `tools/scauto/config/literature_extra_gene_sets.json`
- references: `resources/literature_modules/references.csv`

## What is stored but **not** plotted on that heatmap

GO Biological Process, MSigDB Hallmark, KEGG, and some MSigDB C7 immunologic signatures.
`source_type = msigdb`. These are **snapshots** copied into the same folders (not a live
database pull). They have tens–hundreds of genes, so the compact heatmap skips them.
They are still exported here so the full set is inspectable.

## Fallback

If `resources/literature_modules` is missing (a copy of the helper without this project),
heatmaps use `tools/scauto/config/functional_modules.json` (`bundled_fallback_panels.csv`).

## Columns

- `used_in_heatmap`: TRUE = compact curated panel drawn on the subtype heatmap
- `source_type`: `curated_panel` / `curated_panel_2016_2026` = literature; `msigdb` = GO/Hallmark/KEGG/C7
- `gene_list`: semicolon-separated symbols

