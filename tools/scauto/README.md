# PBMC Auto-Pilot
#
# Default-parameter human PBMC pipeline from 10x triplets
# (matrix.mtx + barcodes + features/genes).
#
Colleague-facing guide: **`0_START_HERE.txt`** (pack) or **`docs/HOW_TO_USE_THE_ANALYSER.txt`**. Short pitch: **`docs/FOR_COLLEAGUES.md`**. Pack a copy: `bash tools/scauto/pack_for_colleagues.sh`.

Colleagues receive the output folder (REPORT.md, CSV, PNG/PDF). That folder
does not need the internet or R. Computation runs here, after R is installed.
Start analysis installs missing packages the first time (needs internet once).
Datasets with more than 50,000 cells after QC should go to your HPC (see hpc/).
The graphical helper estimates RAM and disk after you tick samples. If this
computer is too small, it recommends HPC. With an HPC account and SSH key you
can **Submit to HPC** in one click (upload ticked 10x folders, qsub, pull
results back). A zip of scripts remains available as a fallback.

## Graphical interface (recommended)

Opens a local web wizard: choose a folder, preview 10x files, enter groups,
then click **Start analysis**. That button checks R packages and installs any
that are missing, then runs. Preview figures and tables in the page, and
download a single file or the whole results folder as a zip.

Colleagues on another Mac / Windows / Linux PC: open **`0_START_HERE.txt`**, then double-click **`1_Install_R`** and **`2_Start`** at the top of the packed folder. Detail: **`docs/INSTALL.md`**.

If you are inside the lab project (not a packed copy):

```bash
Rscript tools/scauto/ui/launch.R
```

- macOS packed copy: double-click `2_Start.command`
- Windows packed copy: double-click `2_Start.bat`
- Lab project: `tools/scauto/ui/Launch.command` / `Launch.bat`
- Browser: `http://127.0.0.1:8765` (Chrome, Edge, Firefox, or Safari — not IE)

Closing the terminal (or the Windows black window) stops the app.
In **Choose folder**, click **Use trial samples (2 GroupA + 2 GroupB)**:
sampleA and sampleB (GroupB) plus sampleC and sampleD (GroupA). All four are
**Baseline**, about 24,000 cells. This set has two samples per group, so
Wilcoxon and Fisher OR both run.

## Command line

The command line also checks and installs missing packages before the analysis.

```bash
Rscript tools/scauto/run_scauto.R \
  --input /path/to/10x_root \
  --meta /path/to/sample_meta.csv \
  --outdir results/scauto_my_run
```

`--input` may be:

- one sample folder that already contains `matrix.mtx`, `barcodes.tsv`, `features.tsv` (or `genes.tsv`; `.gz` is OK)
- a parent folder of several such sample folders
- Cell Ranger `filtered_feature_bc_matrix` (detected automatically)

`--samples A,B` limits a parent folder to those sample names.

`sample_meta.csv` must have a `sample` column matching folder names. Optional
`group` column drives clinical proportion tests. Extra columns (Timepoint,
GroupA, donor, …) are copied onto cells.

Smoke test on this project (one library, ~4k cells):

```bash
bash tools/scauto/examples/run_smoke.sh
```

## What the run writes

| Path | Content |
|------|---------|
| `REPORT.md` | Short summary |
| `run_manifest.json` | Parameters, cell counts, laptop vs HPC mode |
| `tables/literature_gene_sets_catalog.csv` | All gene sets: citation, DOI, PMID, genes, heatmap flag |
| `tables/literature_gene_sets_genes.csv` | Long table (one gene per row) |
| `tables/literature_gene_sets_README.md` | Literature panels vs GO/MSigDB/KEGG snapshots vs bundled fallback |
| `tables/literature_gene_sets_references.csv` | Paper list (DOI / PMID) |
| `lineage/<name>/functional_modules_*.csv` | Per-lineage catalog, heatmap subset, genes, and genes actually scored |
| `tables/sample_lineage_cell_counts.csv` | Cells per sample × superlineage (long + `_wide.csv`) |
| `tables/sample_subtype_cell_counts.csv` | Cells per sample × lineage subtype (long + `_wide.csv`) |
| `lineage/<CD4_T, CD8_T_unconv, NK, Mono_DC, B_plasma>/` | Secondary clustering, subtype names, DotPlot, module heatmap, optional `trajectory/` |
| `clinical/` | Sample proportions, Wilcoxon, Fisher OR, `deg_wilcoxon/` + `deg_MAST/`, `pathway/`, and optional `clinical_numeric/` Spearman heatmaps |
| `figures/` | QC violins and atlas UMAPs (Editorial Ink) |
| `objects/atlas_seurat.rds` | Use this RDS for further analysis in R/Seurat (skipped if the cell count is too large) |
| `tools/scauto/logs/usage_log.csv` | Local usage log on that computer |
| `tools/scauto/config/usage_report.json` | Web-app URL so every copy POSTs usage to your Google Sheet (`USAGE_REPORT.md`) |

Azimuth and CellTypist are **off** on the laptop path (the Azimuth reference
alone is too large for 16 GB). Marker-module lineage + subtype panels still run.

## Current limitations (this version)

- Clinical tests use one grouping column at a time (for example GroupA vs GroupB). Extra columns such as Timepoint are each tested separately. Combined / interaction designs such as **GroupA × Timepoint** are not set up yet.
- Cell–cell communication (CellChat, NicheNet, and similar) and SCENIC (gene regulatory networks) are not included. Continue from `objects/atlas_seurat.rds` in R if you need those analyses.

## your HPC (shared university cluster)

From the helper: tick the samples, then **Use university HPC**. Enter host
(for example `hpc.example.edu` or your SSH alias `hpc`) and username.
**Test SSH connection** once. **Submit to HPC** copies only the ticked 10x
folders and the analysis scripts, submits OpenPBS queue `medium`, waits, and
copies `results/` back to this computer.

One-time SSH setup (password prompts cannot be filled by the helper):

```bash
ssh USER@hpc.example.edu          # accept the host fingerprint
ssh-copy-id USER@hpc.example.edu  # so later logins use a key
```

Host, username, and cluster folder are remembered in `~/.scauto_hpc.json`.

**Download scripts only (.zip)** is the manual fallback (PBS + rsync example).

You can also submit without the helper. Pre-download Azimuth / CellTypist on
the **login** node into the project `.cache` (compute nodes usually have no
internet). Install R packages into `Rlib` on the login node too; the job sets
`SCAUTO_NO_PKG_INSTALL=1`. Then:

```bash
bash tools/scauto/hpc/submit_scauto.sh
```

Edit `hpc/run_scauto.pbs` for `--input`, `--meta`, and `--outdir`. Default
queue settings match this project (`medium`, OpenPBS `select=`). Queue
Edit `hpc/run_scauto.pbs` for the queue name and resource limits at your site.

## Defaults (locked)

QC: MT ≤ 10% for PBMC / blood, or ≤ 50% if you choose Liver; nCount 1,000–40,000, nFeature ≤ 8,000.  
HVG 2,000 (MT/RP excluded from HVG, kept in the object). PCA 50, Leiden on
the first 30 Harmony dimensions (or PCs if a single library), resolution 0.8,
seed 42. Harmony by `sample` when there is more than one library (`theta=4`,
`max_iter=20`, `early_stop=false`; required for Harmony 2). Secondary clustering
uses Leiden. CD8+unconv starts at resolution 0.8 (at least 6 clusters), Mono+DC
at 0.7 (at least 5), NK 0.2. Lineage subtype FindAllMarkers and DotPlots exclude
MT / MTRNR / RPL / RPS / MRPL / MRPS genes. Each subtype writes a complete marker table and the top 100 positive markers under `lineage/<name>/markers/`. DotPlots use black point outlines and
larger labels. Functional heatmaps use square tiles, larger labels, and compact
literature scRNA-seq gene sets (2016–2026). Full citations and gene lists: `tables/literature_gene_sets_catalog.csv` and `lineage/<name>/functional_modules_*.csv`. How they were chosen: `tables/literature_gene_sets_README.md`.

Do not subset the object to HVGs. Do not regress clinical covariates. Do not
integrate separately by clinical group.
