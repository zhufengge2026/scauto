# PBMC Auto-Pilot — for colleagues who do not write code

The helper turns 10x sample folders into figures, spreadsheet-ready tables, and (when the dataset is not extremely large) an integrated Seurat RDS. You do not need to write R or tune parameters.

The window title is **Single-cell analysis autopilot**.

## Why use it

**1. One-click, built for first-time users**

Choose a 10x folder → tick samples → enter clinical groups if you need a comparison → **Start analysis** (this computer) or **Submit to HPC** (university cluster).

The first run installs any missing R packages. A progress bar and log stay on the page. When the run finishes, preview files here or download the whole results folder. You do not need the command line or Seurat.

**2. Outputs follow a PBMC / clinical-comparison analysis, not a generic cluster-and-stop template**

The pipeline is set up for human PBMC and clinical contrasts (for example disease-activity groups). It reports QC, lineages (T, NK, monocyte/DC, B/plasma) and within-lineage subtypes, literature gene-set / module scores with citations, group tests (cell proportions and DEGs, including Wilcoxon and MAST), and pathway enrichment (GO, KEGG, GSEA, GSVA, Reactome — each database in its own folder). CytoTRACE2 and Monocle3 trajectory are optional under Advanced Settings for lineages you tick. That is aimed at “which cell types, genes, and pathways differ between groups,” not at building a pipeline from scratch.

**3. About 1–4 typical PBMC libraries can finish on a laptop; larger jobs use university HPC**

After you tick samples, the helper estimates RAM and disk. One to four ordinary PBMC libraries often fit on a workstation or laptop; more samples or very large libraries are sent to the cluster.

For HPC you need **your own** account. Once, in Terminal, log in and install an SSH key (that step uses your university password). After that, **Submit to HPC** uploads only the ticked folders, submits the job, and copies results back. The helper cannot type a password. Do not share the account or the key.

Shutting this computer down does not cancel a job that is already on the cluster. Reopen the helper and use **Continue last HPC job** to wait and pull results. That does not upload again or submit a new job.

**4. Defaults are already chosen from real runs; you should not need a parameter sweep**

QC, HVGs, Harmony, clustering, lineage scores, and second-round clustering use locked defaults from the developer’s analyses. For most human PBMC clinical comparisons, leave them as they are. Tick samples and fill groups in the helper; you do not edit code to do that.

**5. Tables are written so you can replot in your own style**

Results include CSV files (Excel / GraphPad): per-cell sample, group, lineage and subtype; cluster markers and group DEGs; sample-level proportions; literature gene-set catalogues with citations. PNG/PDF figures are included for a first look. For a journal layout, replot from the tables; you do not need to re-run the matrix.

**6. An integrated RDS is saved if you want more analysis yourself**

If you want to continue in R, open `objects/atlas_seurat.rds` in Seurat. You can subset, replot, or add extra tests without repeating QC and integration. If the object would be too large, the RDS is skipped; tables and figures are still written.

## Current limitations (this version)

- Clinical tests use one grouping column at a time (for example GroupA vs GroupB). Extra columns such as Timepoint are each tested separately. Combined designs such as **GroupA × Timepoint** are not set up yet.
- Cell–cell communication (CellChat, NicheNet, and similar) and SCENIC (gene regulatory networks) are not included yet. Use the RDS above if you need those analyses.

## What you need

| Run | Requirements |
|-----|----------------|
| Small local run | R on this computer (`INSTALL.md`) and 10x sample folders |
| Large run / not enough RAM | Plus **your** HPC account and a one-time SSH key |

This helper expects folders from the **10x Genomics platform** (Cell Ranger): `matrix.mtx`, `barcodes.tsv`, and `features.tsv` or `genes.tsv` (`.gz` is fine). If the data is from another company, or you only have RDS / h5ad / CSV / FASTQ, convert first — in the helper open **How to Prepare Data** (see also `USER_MANUAL.md` §6).

## What you get

Results land in a folder on this computer (HPC jobs are copied here when they finish).

| Path | Use |
|------|-----|
| `figures/` | QC and UMAPs for slides |
| `tables/`, `lineage/`, `clinical/` (CSV) | Custom plots, stats, supplements. Pathway tables: `clinical/<lineage>/pathway/<GO,KEGG,GSEA,GSVA,Reactome>/`. Trajectory (if you turned it on): `lineage/<name>/trajectory/` |
| `REPORT.md` | Short run summary |
| `objects/atlas_seurat.rds` | If you want more analysis yourself, open this RDS in R/Seurat |
| `tools/scauto/logs/usage_log.csv` | Usage log (login name, how many runs / samples / cells). Excel. |

**Download all results (.zip)** and **Open results folder** read that local folder. They do not fetch from the cluster again.

## How to start

Step-by-step (including what folder to copy): **`HOW_TO_USE_THE_ANALYSER.txt`**.

1. Install and open the helper (`1_Install_R` then `2_Start` at the top of this folder).
2. To try the tool: **Use trial samples (2 GroupA + 2 GroupB)**.
3. For a real run: choose your 10x folder → tick samples → fill groups → follow the memory prompt → **Start analysis** or **Use university HPC**.

Install, SSH, and command-line detail: `INSTALL.md` and `README.md` in this folder.
