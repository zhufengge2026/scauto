# scAuto User Manual

**Single-cell analysis autopilot**

For colleagues who have not analysed single-cell data before, and whose computer may not have R or analysis packages yet. You do not write code.

The helper runs **on this computer**. The browser is only the control panel. Nothing is uploaded to a public website. **Submit to HPC** (optional) copies only the samples you ticked to **your** university cluster account.

---

## 1. What to copy to a colleague

**Do not copy the whole lab folder** (`your_project_folder` or similar). That folder is hundreds of gigabytes of old analyses. The helper will not start from `app.R` alone.

Keep this **layout and these names**:

```text
scauto_helper/                 ← any folder name is fine
  0_START_HERE.txt             ← start here
  1_Install_R.command / .bat   ← install R
  2_Start.command / .bat       ← open the helper
  docs/                        ← longer manuals
  data/trial_4samples/         ← four practice libraries
  viz/
  resources/
  tools/scauto/
```

On a Mac that already has the lab project, the easiest way is:

```text
bash tools/scauto/pack_for_colleagues.sh
```

That writes a ready folder on the Desktop (about **1 GB** with the four trial samples). Zip that folder and send the zip. Do **not** include `hpc/config.env`, `results/`, or any RDS objects from the lab project.

| Include | Leave out |
|---------|-----------|
| `tools/scauto/` | The rest of the lab project (`results/`, `intermediate_RDS/`, `pipelines/`, …) |
| `viz/` | Lab `hpc/config.env` (accounts and paths) |
| `resources/` | Other people’s HPC keys or passwords |
| Trial samples `sampleA`, `sampleB`, `sampleC`, `sampleD` under `data/trial_4samples/` | Lab `results/` and RDS objects |

Colleagues use **their own** 10x folders for a real analysis. The four trial samples are only so they can click through once.

---

## 2. What this does

You point it at folders from the **10x Genomics platform** (Chromium + Cell Ranger). It produces:

- QC and UMAP figures (PNG / PDF)
- Spreadsheet tables (CSV — open in Excel)
- A short `REPORT.md`
- Optional integrated Seurat object (`objects/atlas_seurat.rds`) if the cell count is not huge. Use this RDS if you want to do more analysis yourself in R (subset, replot, extra tests) without repeating QC and integration.

It is set up for **human PBMC** and a **two-group clinical comparison** (for example GroupA vs GroupB). Defaults are already chosen. Leave them unless someone in the lab asks you to change Advanced Settings.

---

## 3. What you need

| Item | Notes |
|------|--------|
| A computer | Mac, Windows, or Linux. Chrome, Edge, Firefox, or Safari (**not** Internet Explorer). |
| Internet | **Once**, the first time: to install R (if missing) and analysis packages. Later runs can be offline if packages are already there. |
| RAM | 4 trial samples (~24,000 cells) need about **13 GB**. About 30,000 cells ≈ a 16 GB laptop. Many samples → university HPC, not a laptop. |
| Disk | Several GB free for a small run. |
| Your data | From the **10x Genomics platform** (Chromium + Cell Ranger): each sample is a **folder** with `matrix.mtx`, `barcodes.tsv`, and `features.tsv` or `genes.tsv` (`.gz` is fine). `filtered_feature_bc_matrix` also works. Other platforms: convert first (see §6 / **How to Prepare Data**). |

You do **not** need to install Seurat, Harmony, or Python yourself. The helper installs analysis packages when you click **Start analysis**.

You do **not** need the command line for a normal run.

---

## 4. First time on a new computer

### Step A — Install R if it is missing

R is required. You cannot skip it.

| Computer | What to do |
|----------|------------|
| Mac / Linux | Double-click `1_Install_R.command` at the top of this folder (Mac: if it will not open, open Terminal, drag the file in, press Enter). Or double-click **2_Start** below — it will offer to install R. |
| Windows | Double-click `1_Install_R.bat` |

Download **R 4.3 or newer** from [https://cran.r-project.org](https://cran.r-project.org) if the script asks you to. Windows: tick **Add R to PATH** if you see that option. Windows users who later see compile errors for `harmony` should also install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

Stay online. This step only installs R, not Seurat.

### Step B — Open the helper

| Computer | What to do |
|----------|------------|
| Mac | Double-click `2_Start.command`. If macOS blocks it: right-click → **Open**. If it still will not run: `chmod +x 2_Start.command` once in Terminal. |
| Windows | Double-click `2_Start.bat` |
| Any | In Terminal: `Rscript tools/scauto/ui/launch.R` |

A **black Terminal / command window** stays open. **Leave it open.** Closing it stops the helper.

Your browser should open `http://127.0.0.1:8765`. If it does not, paste that address into Chrome or Safari.

The first Launch may download a few small packages (`shiny`, …) so the page can open. Stay online.

---

## 5. Trial run (recommended the first time)

On the **Data** page:

1. Click **Use trial samples (2 GroupA + 2 GroupB)**.  
   This uses sampleA + sampleB (GroupB) and sampleC + sampleD (GroupA), all Baseline, if those folders were included in the copy.
2. Check that four samples are ticked.
3. Click through **Groups** (names are already filled) and **Settings**.
4. Click **Start analysis**.

The first **Start analysis** on a new computer downloads Seurat and related packages. That can take **10–40 minutes**. The page lists each check: waiting → checking → OK (or install). Stay online until every line is OK.

The trial itself often takes about **one hour** on a workstation, sometimes longer on a laptop. Close Excel with huge files, extra browser tabs, and other R sessions so RAM is free.

When it finishes, stay on the **Run** page. Use **Download all results (.zip)** or **Open results folder**.

---

## 6. Your own samples

In the helper, **How to Prepare Data** (top of the page) is the full beginner guide. Short version below.

### What the helper can open

Each sample is a **folder** named with your library ID. Inside it:

- `matrix.mtx` (or `.gz`)
- `barcodes.tsv` (or `.gz`)
- `features.tsv` or `genes.tsv` (or `.gz`)

Cell Ranger `filtered_feature_bc_matrix` (including `outs/filtered_feature_bc_matrix`) is detected automatically. Prefer **filtered** matrices, not raw.

Several samples: put those folders next to each other and choose the **parent** folder.

To try the software without your own data, use **Use trial samples**.

### If the data is not from the 10x Genomics platform

This helper is for **10x Genomics, Inc.** Chromium data processed with Cell Ranger. It does **not** read FASTQ, BAM, Excel, a Seurat `.rds`, or an `.h5ad` until you convert each sample to the three-file folder above. Use **human gene symbols** (this pipeline is for human PBMC).

| You have | What to do |
|----------|------------|
| 10x Genomics Cell Ranger folder or mtx triplet | Choose that folder (or its parent). |
| Only `filtered_feature_bc_matrix.h5` | Export with DropletUtils `read10xCounts` → `write10xCounts`. |
| Seurat RDS | Split by sample if needed, then `write10xCounts` from the counts assay. |
| scanpy `.h5ad` | Write `matrix.mtx` (transpose: genes × cells), `barcodes.tsv`, `features.tsv`. |
| Gene × cell CSV / Excel | Save as CSV, then `write10xCounts` in R. Excel alone is not enough. |
| FASTQ from the 10x Genomics platform | Run **Cell Ranger** first. This helper does not align reads. |
| 10x Chromium library sequenced on **MGI / DNBSEQ** | If the core already gave Cell Ranger mtx folders, choose those. If you only have `*.fq.gz`, ask the core to convert FASTQ and run Cell Ranger. |
| MGI **DNBelab** C4 / C series | Use MGI’s analysis software first. If it wrote the three mtx files, choose that folder. Do not run 10x Cell Ranger on DNBelab FASTQ. |
| MGI Stereo-seq | Not this helper. |
| Another company (Parse / BD Rhapsody / SMART-seq) | Use that vendor’s pipeline until you have a gene-by-cell matrix, then write the Cell Ranger triplet. |

Snippets are in **How to Prepare Data**.

### Data page

1. **Choose folder** — pick the **parent** folder that contains one subfolder per sample (or a single sample folder that already has the three 10x files).
2. Tick the samples to include. Untick anything you do not want.
3. Read the RAM / disk note. If the helper says this computer is too small, use **Use university HPC** (section 8). Do not force a huge run on a laptop.

### Groups page

Type a group name for each sample (example: `GroupA` / `GroupB`).  
Use the **same spelling** for samples in the same group. Two groups are enough for a comparison. Leave group blank only if you do not need a clinical contrast (then DEG / pathway tests are skipped).

### Settings page

Leave defaults for **PBMC / blood** (mitochondrial genes ≤ 10%). If this is **liver** tissue, choose **Liver** on that page. That sets mitochondrial genes to ≤ **50%**, because hepatocytes normally have more mitochondrial RNA. Open **Advanced Settings** only if a colleague who knows the pipeline asked you to (other QC cutoffs, pathway databases, trajectory, extra clinical numbers).

Optional in Advanced Settings:

- Extra sample numbers (Age, LabValue1, …) from a CSV with a `sample` column. Spearman heatmaps need at least **4** samples with values.
- CytoTRACE2 / Monocle3 trajectory — **off** unless those R packages are already installed. They are **not** installed automatically.

### Start

Click **Start analysis**. Confirm if a plan window appears.

---

## 7. While it runs

- Keep the **black Terminal window** and the browser tab open.
- The **Log** box updates. Scroll **up** to the first line; it stays on the latest lines unless you scroll up.
- **Force Stop** asks for confirmation. Use it only if you mean to cancel.
- Do not put the laptop to sleep on a local run. Plugging in power is safer.

If the browser says **Page lost contact**, the analysis may still be running in the black window. Close that black window to stop everything, then double-click Launch again.

---

## 8. If this computer is too small (university HPC)

**HPC** means **High-Performance Computing**: a shared university cluster, not this laptop. In the helper, click **How to Use HPC** (top right, and in the HPC window). That page is the full beginner guide. Short version:

**Account.** Ask university IT department. They request a username from the university HPC office (ask your HPC office for the hostname). You cannot invent a login. Do not share accounts or `hpc/config.env`. Use the university VPN if SSH is off-campus.

**SSH (once, in Terminal — the helper cannot type a password).**

```text
ssh-keygen -t ed25519
ssh YOUR_USER@hpc.example.edu
ssh-copy-id YOUR_USER@hpc.example.edu
```

Accept the fingerprint (`yes`), then your HPC password. After that, `ssh` should log in without a password. Windows: OpenSSH or Git Bash; if `ssh-copy-id` is missing, see **How to Use HPC**.

**Recommended: one-click.** Tick samples → **Use university HPC** → host + username → **Test SSH connection** → **Submit to HPC**. Only ticked folders are uploaded. Keep this computer on for copy-back. If you closed the helper, use **Continue last HPC job** (that banner’s Close only hides the notice).

**Fallback: scripts this program generates.** In the HPC window, **Download scripts only (.zip)**. Unzip. Read `README.md` and `COMMANDS.txt`. Copy *your* project and the ticked 10x folders to *your* cluster path. Edit `PROJECT_DIR` in `hpc/run_scauto.pbs` and `hpc/submit.sh` if needed. On the **login** node once: `Rscript tools/scauto/check_install.R --install`. Then `bash hpc/submit.sh` and `qstat -u $USER`. Edit the queue name and resource limits for your site. Compute nodes have no internet.

---

## 9. Where results are

Each run creates a folder such as `results/scauto_YYYYMMDD_HHMMSS` inside the helper folder you copied.

| Open this | What it is |
|-----------|------------|
| `REPORT.md` | Short summary of the run |
| `figures/` | QC plots and UMAPs for slides |
| `tables/` | Cell counts, combined markers, literature gene-set lists |
| `lineage/` | Per lineage (CD4 T, CD8 T, NK, Mono/DC, B/plasma): UMAP, DotPlot, module heatmap, subtype markers |
| `clinical/` | Group tests: proportions, DEGs, pathway folders (GO, KEGG, GSEA, GSVA, Reactome) |
| `objects/atlas_seurat.rds` | If you want more analysis yourself in R/Seurat, open this file. It is skipped if too many cells |
| `logs/pipeline.log` | Full text log if something failed |

**Download all results (.zip)** packs that folder. Colleagues who only want to **read** results need Excel (or similar) and a PDF/PNG viewer — not R.

If you want to continue in R, load the RDS from the results folder (you do not need to re-run the helper):

```r
library(Seurat)
obj <- readRDS("objects/atlas_seurat.rds")
```

CSV files: if Excel garbles gene names (`SEPTIN` → dates), import as text or open in Google Sheets.

---

## 10. Trouble shooting

| What you see | What to do |
|--------------|------------|
| Launch does nothing / “R is not installed” | Section 4, Step A. Install R, then Launch again. Stay online. |
| Browser is empty | Paste `http://127.0.0.1:8765` into Chrome or Safari. Close an old black Launch window first. |
| “Cannot find the project root (need viz/palette.R)” | The copy is incomplete. You need `viz/` next to `tools/`, not `app.R` by itself. Use the pack script. |
| Trial button does nothing | The four folders sampleA, sampleB, sampleC, sampleD are missing under `data/trial_4samples/`. |
| First Start sits on “install” for a long time | Normal. Seurat is large. Stay online. Do not close the black window. |
| Helper says RAM / disk is too low | Untick samples, close other programs, or use university HPC. |
| Run failed | Keep the window. Copy the Log (scroll to the first line if needed) and send it to whoever maintains this helper. Also send `logs/pipeline.log`. |
| Mac: “2_Start.command cannot be opened” | Right-click → Open. System Settings → Privacy & Security → Open Anyway. |

---

## 11. Current limitations (this version)

This helper does a standard PBMC / clinical run. It does not yet do everything you might want for a paper.

- Clinical tests use **one grouping column at a time** (for example GroupA vs GroupB). Extra columns (for example Timepoint) are each tested on their own. Combined / interaction designs are **not** set up yet — for example **GroupA × Timepoint**, or other multi-factor combinations.
- Advanced methods are **not** included yet, including **cell–cell communication** (CellChat, NicheNet, and similar) and **SCENIC** (gene regulatory networks).

If you need those analyses, open `objects/atlas_seurat.rds` from the results folder and continue in R.

---

## 12. What you should not do

- Do not copy or email the lab’s `hpc/config.env` or anyone else’s HPC account.
- Do not edit the R scripts to “just run it”.
- Do not start a second Launch while the first black window is still open (if the page looks stuck, close the old window first).
- Do not use Internet Explorer.
- Do not put 50,000+ cells on a 16 GB laptop. Use HPC.

---

## 13. Extra reading (optional)

| File | Who it is for |
|------|----------------|
| `docs/HOW_TO_USE_THE_ANALYSER.txt` | Plain-English steps (start here) |
| `docs/FOR_COLLEAGUES.md` | Short “why this helper exists” |
| `docs/INSTALL.md` | Same install steps, slightly more technical |
| `docs/README.md` | Command line and default parameters |

You do not need those files to finish a trial run.
