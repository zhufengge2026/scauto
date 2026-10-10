# Install checklist (Mac / Windows / Linux)

The helper is a **local** program. The browser is only the control panel (`http://127.0.0.1:8765`). A local **Start** uses this computer’s RAM. **Submit to HPC** copies the ticked 10x folders to *your* cluster account (not to a public website).

Colleague-facing steps: **`0_START_HERE.txt`** or **`docs/HOW_TO_USE_THE_ANALYSER.txt`**. To pack a copy (not the 200+ GB lab folder): `bash tools/scauto/pack_for_colleagues.sh`.

Copy **`viz/` + `resources/` + `tools/scauto/`** (and, for the trial, four folders under `data/trial_4samples/`). Do not copy `app.R` alone. Do not copy lab `hpc/config.env`.

## 0. If this computer does not have R

You cannot skip R. The analysis runs in R (Seurat). The helper will not start until R is installed.

**Easiest:** double-click the installer for your system (needs internet, several minutes):

| Computer | Double-click |
|----------|----------------|
| macOS / Linux | `1_Install_R.command` (Mac: if it will not open, run `bash 1_Install_R.command` in Terminal) |
| Windows | `1_Install_R.bat` |

That script installs **R 4.3+** if it is missing (Homebrew / winget / or it opens the CRAN download page for you). Analysis packages are **not** a separate step: they download when you click **Start analysis**.

If the automatic R install is not available, download R yourself from <https://cran.r-project.org> (Windows: tick **Add R to PATH** if you see it), then run the same installer script again.

Windows users who later see compile errors for `harmony` should also install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

## 1. Start the helper

| Computer | Start |
|----------|--------|
| macOS | Double-click `2_Start.command` (if blocked: `chmod +x 2_Start.command`) |
| Windows | Double-click `2_Start.bat` |
| Any OS | `Rscript tools/scauto/ui/launch.R` |

If you double-click Launch and R is still missing, it will point you to the installer in step 0.

The first Launch may download a few helper packages (`shiny`, …) so the page can open. That needs internet once.

The default browser should open. If it does not, go to `http://127.0.0.1:8765`. Closing the terminal / black window **stops** the app.

## 2. Click Start analysis

Choose a 10x folder, set groups, then click **Start analysis**.

The **Run** page always checks every required item (R version, project files, RAM, and each package), even if they were already installed. You will see each line on the page go from waiting → checking → OK. If something is missing, it is installed from CRAN (`Seurat` can take several minutes the first time; `MAST` is used for clinical-group DEGs; `msigdbr` holds offline pathway gene sets), then every item is checked again.

Pathway databases also use Bioconductor packages (`clusterProfiler`, `org.Hs.eg.db`, `ReactomePA`, `GSVA`) when they can be installed. A missing package skips that database only; the rest of the run still finishes. CytoTRACE2 and Monocle3 are **not** installed automatically — install them yourself if you will tick those Advanced Settings options.

Stay online the first time. Later runs still list every check; they only download if something is missing.

You do **not** need to run `check_install.R` first. That script is only a diagnostic if something fails.

## 3. Software

| Item | Notes |
|------|--------|
| R 4.3 or newer | Installed in step 0 |
| Packages | Installed by Launch (helper window) and by **Start analysis** (Seurat and the rest) |
| Browser | Chrome, Edge, Firefox, or Safari. **Not Internet Explorer.** |
| zip (Windows) | Needed for **Download all results (.zip)**. Rtools or any `zip.exe` on PATH |

## 4. Memory (this computer)

The Data step estimates RAM from the samples you tick.

- 4 Baseline trial samples (~24,000 cells) ≈ **13 GB**
- ~30,000 cells ≈ **16 GB** laptop budget
- ~50,000 cells or more → university HPC, not a laptop

If RAM or free disk is too low, the helper blocks **Start** and offers **Use university HPC**. You need an HPC account. One-time: `ssh USER@HOST` then `ssh-copy-id USER@HOST` (the helper cannot type a password). **Test SSH connection**, then **Submit to HPC** uploads the ticked samples, submits the job, and copies results back. macOS/Linux already have `ssh` and `rsync`; Windows needs OpenSSH (and rsync or Git Bash).

**Download scripts only (.zip)** is still there if you prefer to copy files by hand.

Close Excel with large files, extra browser tabs, and other R sessions before a local Start.

HPC compute nodes usually have no internet. The one-click path installs packages on the **login** node the first time (`Rlib/.scauto_ok`), including CRAN `msigdbr` and (when Bioconductor is reachable) pathway packages. Jobs set `SCAUTO_NO_PKG_INSTALL=1`. A missing pathway database is skipped rather than stopping the job. Install CytoTRACE2 / monocle3 on the login node before ticking those Advanced options.

## 5. Quick trial

In **Choose folder**, click **Use trial samples (2 GroupA + 2 GroupB)**
(sampleA, sampleB, sampleC, sampleD; all Baseline) if those folders are on this copy of the project.

## Optional: print a checklist

```text
Rscript tools/scauto/check_install.R
```
