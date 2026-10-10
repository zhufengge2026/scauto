# scAuto

One-stop human PBMC single-cell RNA-seq analyser.

scAuto turns 10x Genomics Cell Ranger folders into QC figures, lineage and subtype tables, literature gene-set scores, group tests, and pathway reports. A local web wizard is the usual entry point; an R command-line interface is included for laptops and university HPC.

Defaults are locked from real PBMC clinical analyses (Harmony, Leiden, lineage recluster, Wilcoxon / MAST, GO / KEGG / GSEA / GSVA / Reactome). Optional CytoTRACE2 and Monocle3 can be switched on under Advanced Settings. This is not a generic cluster-and-stop template.

## Quick start

Install [R](https://cran.r-project.org/) (4.3+ recommended), then either:

**Graphical helper (recommended)**

```bash
Rscript tools/scauto/ui/launch.R
```

Or double-click `1_Install_R` then `2_Start` in this folder. Browser: <http://127.0.0.1:8765>

**Command line**

```bash
Rscript tools/scauto/run_scauto.R \
  --input /path/to/10x_root \
  --meta /path/to/sample_meta.csv \
  --outdir results/scauto_my_run
```

`--input` may be one sample folder (`matrix.mtx` + `barcodes.tsv` + `features.tsv` or `genes.tsv`; `.gz` is OK) or a parent of several such folders. `sample_meta.csv` needs a `sample` column matching folder names. Optional `group` drives clinical proportion tests.

More detail: [`docs/FOR_COLLEAGUES.md`](docs/FOR_COLLEAGUES.md), [`docs/USER_MANUAL.md`](docs/USER_MANUAL.md), [`docs/HOW_TO_PREPARE_DATA.txt`](docs/HOW_TO_PREPARE_DATA.txt).

## Repository layout

| Path | Role |
|------|------|
| `tools/scauto/` | Pipeline (ingest → QC → cluster → annotate → DEG / pathway / trajectory) plus Shiny UI |
| `viz/` | Editorial Ink palettes and plotting helpers |
| `resources/` | Bundled literature gene-set tables |
| `docs/` | Install, HPC, and colleague-facing manuals |
| `data/` | Your 10x folders go here (not shipped on GitHub) |

## Data not included

10x count matrices and real clinical tables are omitted: they exceed GitHub’s file-size limit and contain patient-derived measurements. Put your own Cell Ranger folders next to a metadata CSV. Templates: `tools/scauto/examples/sample_meta.smoke.csv` and `tools/scauto/examples/sample_clinical_numeric_template.csv`.

## Usage reporting

Anonymous usage POSTs are **off** in this public copy (`tools/scauto/config/usage_report.json`). See `tools/scauto/USAGE_REPORT.md` if you want to enable your own Google Sheet endpoint.

## License

MIT. See [LICENSE](LICENSE).
