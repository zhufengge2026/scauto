# GUI helpers — no Seurat required.

ui_scauto_root <- function() {
  normalizePath(file.path(getOption("scauto.ui_dir"), ".."))
}

ui_project_root <- function() {
  getOption("scauto.project")
}

count_barcodes <- function(mat_dir) {
  files <- list.files(mat_dir, full.names = TRUE)
  hit <- files[grepl("^barcodes\\.tsv\\.gz$", basename(files))]
  if (!length(hit)) hit <- files[grepl("^barcodes\\.tsv$", basename(files))]
  if (!length(hit)) return(NA_integer_)
  path <- hit[[1]]
  con <- if (grepl("\\.gz$", path)) gzfile(path, "rt") else file(path, "rt")
  on.exit(close(con), add = TRUE)
  length(readLines(con, warn = FALSE))
}

folder_hint <- function(path) {
  files <- tryCatch(list.files(path), error = function(e) character())
  if (any(grepl("^matrix\\.mtx", files, ignore.case = TRUE))) return("10x sample")
  kids <- tryCatch(
    list.files(path, full.names = TRUE, include.dirs = TRUE, no.. = TRUE),
    error = function(e) character()
  )
  kids <- kids[dir.exists(kids)]
  if (length(kids) > 80L) return("")
  n10 <- sum(vapply(kids, function(k) {
    any(grepl("^matrix\\.mtx", list.files(k), ignore.case = TRUE))
  }, logical(1)))
  if (n10 > 0) return(sprintf("%s 10x sample(s)", n10))
  ""
}

list_browse_entries <- function(path, limit = 400L) {
  empty <- data.frame(
    name = character(), path = character(), hint = character(), is_10x = logical(),
    stringsAsFactors = FALSE
  )
  if (is.null(path) || !nzchar(path) || !dir.exists(path)) return(empty)
  kids <- tryCatch(
    list.files(path, full.names = TRUE, include.dirs = TRUE, no.. = TRUE),
    error = function(e) character()
  )
  kids <- kids[dir.exists(kids)]
  kids <- kids[!grepl("(^|/)\\.[^/]+$", kids)]
  kids <- sort(kids)
  truncated <- length(kids) > limit
  if (truncated) kids <- kids[seq_len(limit)]
  hints <- vapply(kids, folder_hint, character(1), USE.NAMES = FALSE)
  data.frame(
    name = basename(kids),
    path = kids,
    hint = hints,
    is_10x = hints == "10x sample",
    stringsAsFactors = FALSE
  )
}

current_10x_note <- function(path) {
  files <- tryCatch(list.files(path), error = function(e) character())
  hits <- files[grepl("^(matrix\\.mtx|barcodes\\.tsv|features\\.tsv|genes\\.tsv)",
                      files, ignore.case = TRUE)]
  if (!length(hits)) return(NULL)
  sprintf("This folder already has 10x files: %s", paste(hits, collapse = ", "))
}

native_choose_dir <- function(prompt = "Select a data folder") {
  sys <- Sys.info()[["sysname"]]
  if (identical(sys, "Darwin")) {
    out <- suppressWarnings(system2(
      "osascript",
      c(
        "-e", "tell application \"Finder\"",
        "-e", "activate",
        "-e", "try",
        "-e", sprintf("POSIX path of (choose folder with prompt \"%s\")", prompt),
        "-e", "on error",
        "-e", "return \"\"",
        "-e", "end try",
        "-e", "end tell"
      ),
      stdout = TRUE,
      stderr = FALSE
    ))
    out <- paste(out, collapse = "")
    out <- gsub("[\r\n]+", "", out)
    out <- sub("/$", "", out)
    if (!nzchar(out)) return(NULL)
    return(out)
  }
  if (identical(sys, "Windows")) {
    cmd <- paste0(
      "Add-Type -AssemblyName System.Windows.Forms; ",
      "$d = New-Object System.Windows.Forms.FolderBrowserDialog; ",
      "$d.Description = '", prompt, "'; ",
      "if ($d.ShowDialog() -eq 'OK') { $d.SelectedPath }"
    )
    out <- suppressWarnings(system2("powershell", c("-NoProfile", "-Command", cmd), stdout = TRUE))
    out <- paste(out, collapse = "")
    if (!nzchar(trimws(out))) return(NULL)
    return(trimws(out))
  }
  if (nzchar(Sys.which("zenity"))) {
    out <- suppressWarnings(system2("zenity", c("--file-selection", "--directory",
                                               "--title", prompt), stdout = TRUE, stderr = FALSE))
    if (!length(out) || !nzchar(out)) return(NULL)
    return(out[[1]])
  }
  NULL
}

open_folder <- function(path) {
  if (!dir.exists(path)) return(invisible(FALSE))
  sys <- Sys.info()[["sysname"]]
  if (identical(sys, "Darwin")) {
    system2("open", path, wait = FALSE)
  } else if (identical(sys, "Windows")) {
    system2("explorer", shQuote(path), wait = FALSE)
  } else {
    system2("xdg-open", path, wait = FALSE)
  }
  invisible(TRUE)
}

safe_discover <- function(input) {
  if (is.null(input) || !nzchar(input) || !dir.exists(input)) {
    return(list(ok = FALSE, error = "Please choose a folder that exists.", dirs = NULL))
  }
  dirs <- tryCatch(
    discover_sample_dirs(input, NULL),
    error = function(e) e
  )
  if (inherits(dirs, "error")) {
    return(list(ok = FALSE, error = conditionMessage(dirs), dirs = NULL))
  }
  n <- vapply(dirs, count_barcodes, integer(1))
  list(
    ok = TRUE,
    error = NULL,
    dirs = dirs,
    n_barcodes = n,
    n_total = sum(n, na.rm = TRUE)
  )
}

# Laptop budget matches tools/scauto/config/default.yaml (16 GB · 30k / 50k cells).
MEM_ASSUME_LAPTOP_GB <- 16
MEM_WARN_CELLS <- 30000
MEM_HPC_CELLS <- 50000
DISK_MIN_GB <- 8

host_n_cores <- function() {
  n <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_)
  n <- suppressWarnings(as.integer(n)[1])
  if (!is.finite(n) || n < 1L) return(NA_integer_)
  n
}

host_disk_free_gb <- function(path = ".") {
  path <- tryCatch(normalizePath(path, winslash = "/", mustWork = FALSE), error = function(e) path)
  if (!dir.exists(path)) path <- dirname(path)
  if (!dir.exists(path)) return(NA_real_)
  out <- suppressWarnings(system2("df", c("-Pk", path), stdout = TRUE, stderr = FALSE))
  if (!length(out) || length(out) < 2L) return(NA_real_)
  parts <- strsplit(trimws(out[[length(out)]]), "\\s+")[[1]]
  if (length(parts) < 4L) return(NA_real_)
  avail_kb <- suppressWarnings(as.numeric(parts[[4]]))
  if (!is.finite(avail_kb) || avail_kb < 0) return(NA_real_)
  round(avail_kb / (1024^2), 1)
}

suggest_pbs_mem_gb <- function(est_gb, n_cells) {
  need <- 64
  if (is.finite(est_gb) && est_gb > 0) need <- ceiling(est_gb * 1.7)
  if (is.finite(n_cells) && n_cells > 80000) need <- max(need, 128)
  if (is.finite(n_cells) && n_cells > 150000) need <- max(need, 192)
  need <- max(64, min(192, 16 * ceiling(need / 16)))
  as.integer(need)
}

host_ram_gb <- function() {
  sys <- Sys.info()[["sysname"]]
  if (identical(sys, "Darwin")) {
    b <- suppressWarnings(as.numeric(system("sysctl -n hw.memsize", intern = TRUE)))
    if (length(b) && is.finite(b) && b > 0) return(round(b / (1024^3), 1))
  }
  if (file.exists("/proc/meminfo")) {
    line <- grep("^MemTotal:", readLines("/proc/meminfo", n = 40L, warn = FALSE), value = TRUE)
    kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", line[[1]])))
    if (length(kb) && is.finite(kb) && kb > 0) return(round(kb / 1024^2, 1))
  }
  if (identical(sys, "Windows") && nzchar(Sys.which("powershell"))) {
    out <- suppressWarnings(system2(
      "powershell",
      c("-NoProfile", "-Command",
        "[math]::Round((Get-CimInstance Win32_ComputerSystem).TotalPhysicalMemory/1GB,1)"),
      stdout = TRUE, stderr = FALSE
    ))
    gb <- suppressWarnings(as.numeric(out[[1]]))
    if (length(gb) && is.finite(gb) && gb > 0) return(round(gb, 1))
  }
  NA_real_
}

estimate_pipeline_ram_gb <- function(n_samples, n_cells) {
  n_samples <- max(0, as.numeric(n_samples)[1])
  n_cells <- max(0, as.numeric(n_cells)[1])
  if (!is.finite(n_cells) || n_cells <= 0 || n_samples < 1) return(NA_real_)
  # Peak RSS for this Seurat path (ingest, Harmony, markers, five-lineage recluster).
  # Pre-QC barcodes slightly overestimate; calibrated to ~16 GB at 30k cells.
  round(2.8 + 0.42 * (n_cells / 1000) + 0.12 * n_samples, 1)
}

# Rough laptop wall-clock (minutes). Returns c(low, high). First-time CRAN
# installs (Seurat) are extra and are not included here.
estimate_pipeline_time_min <- function(n_samples, n_cells) {
  n_samples <- max(0, as.numeric(n_samples)[1])
  n_cells <- max(0, as.numeric(n_cells)[1])
  if (!is.finite(n_cells) || n_cells <= 0 || n_samples < 1) {
    return(c(low = NA_real_, high = NA_real_))
  }
  mid <- 10 + 1.8 * (n_cells / 1000) + 0.5 * n_samples
  c(
    low = max(8, round(mid * 0.6)),
    high = max(15, round(mid * 1.6))
  )
}

format_run_elapsed <- function(secs) {
  secs <- suppressWarnings(as.numeric(secs)[1])
  if (!is.finite(secs) || secs < 0) return("unknown")
  secs <- as.integer(round(secs))
  h <- secs %/% 3600L
  m <- (secs %% 3600L) %/% 60L
  s <- secs %% 60L
  if (h > 0L) {
    sprintf("%d h %d min %d s", h, m, s)
  } else if (m > 0L) {
    sprintf("%d min %d s", m, s)
  } else {
    sprintf("%d s", s)
  }
}

memory_check <- function(n_samples, n_cells, ram_gb = host_ram_gb(),
                         disk_gb = NA_real_, n_cores = host_n_cores()) {
  n_samples <- as.integer(n_samples)[1]
  n_cells <- as.numeric(n_cells)[1]
  if (!is.finite(n_samples) || n_samples < 1L || !is.finite(n_cells) || n_cells <= 0) {
    return(list(
      valid = FALSE, n_samples = max(0L, n_samples), n_cells = max(0, n_cells),
      est_gb = NA_real_, host_gb = ram_gb, host_known = is.finite(ram_gb),
      disk_gb = disk_gb, n_cores = n_cores, pbs_mem_gb = 64L,
      recommend_hpc = FALSE, offer_hpc_pack = FALSE, prompt_hpc = FALSE, level = "none", title = "Select samples",
      headline = "Tick at least one sample to estimate memory.",
      detail = ""
    ))
  }
  est <- estimate_pipeline_ram_gb(n_samples, n_cells)
  host_known <- is.finite(ram_gb) && ram_gb > 0
  host <- if (host_known) ram_gb else MEM_ASSUME_LAPTOP_GB
  disk_known <- is.finite(disk_gb) && disk_gb >= 0
  disk_need <- if (is.finite(est)) max(DISK_MIN_GB, round(2.2 * est, 1)) else DISK_MIN_GB
  level <- "ok"
  if (n_cells > MEM_HPC_CELLS || (is.finite(est) && est > 0.85 * host)) {
    level <- "too_large"
  } else if (n_cells > MEM_WARN_CELLS || (is.finite(est) && est > 0.60 * host)) {
    level <- "tight"
  }
  if (disk_known && disk_gb < disk_need) {
    if (disk_gb < 0.7 * disk_need || disk_gb < DISK_MIN_GB) {
      level <- "too_large"
    } else if (!identical(level, "too_large")) {
      level <- "tight"
    }
  }
  if (is.finite(n_cores) && n_cores <= 2L && n_cells > 20000 && !identical(level, "too_large")) {
    level <- "tight"
  }
  host_txt <- if (host_known) {
    sprintf("This computer reports about %s GB of RAM.", format(host, nsmall = 1))
  } else {
    sprintf("Could not read this computer's RAM. The helper is sized for a %s GB laptop.", MEM_ASSUME_LAPTOP_GB)
  }
  extra <- character()
  if (disk_known) {
    extra <- c(extra, sprintf("Free disk on this volume: about %s GB (need ~%s GB for objects and figures).",
                              format(disk_gb, nsmall = 1), format(disk_need, nsmall = 1)))
  }
  if (is.finite(n_cores)) extra <- c(extra, sprintf("CPU cores: %s.", n_cores))
  headline <- sprintf(
    "%s sample(s) · about %s cells (before QC). Estimated RAM: about %s GB.",
    format(n_samples, big.mark = ","),
    format(round(n_cells), big.mark = ","),
    format(est, nsmall = 1)
  )
  detail <- switch(
    level,
    ok = paste(
      host_txt, paste(extra, collapse = " "),
      "Please close other heavy programs (Excel with large files, browsers with many tabs, other R sessions) and confirm memory looks enough before you click Start. University HPC is available for this size if you prefer the cluster."
    ),
    tight = paste(
      host_txt, paste(extra, collapse = " "),
      "This size is tight for a laptop. Close other programs, or untick some samples. Prefer the university HPC if the run is slow. You can download an HPC pack (scripts, commands, and a short HOWTO) from this page."
    ),
    too_large = paste(
      host_txt, paste(extra, collapse = " "),
      "This selection is above what this computer can run safely (~30,000–50,000 cells, or more RAM/disk than is free). Untick samples, or download the HPC pack and submit on the university cluster. Do not click Start on this laptop."
    ),
    host_txt
  )
  title <- switch(
    level,
    ok = "This computer looks OK — please still check",
    tight = "Tight on this computer — HPC is safer",
    too_large = "Not enough local resources — use HPC",
    "Resource check"
  )
  list(
    valid = TRUE,
    n_samples = n_samples,
    n_cells = n_cells,
    est_gb = est,
    host_gb = host,
    host_known = host_known,
    disk_gb = disk_gb,
    disk_need_gb = disk_need,
    n_cores = n_cores,
    pbs_mem_gb = suggest_pbs_mem_gb(est, n_cells),
    recommend_hpc = identical(level, "too_large"),
    offer_hpc_pack = TRUE,
    prompt_hpc = level %in% c("tight", "too_large"),
    level = level,
    title = title,
    headline = headline,
    detail = detail
  )
}

# Zip of PBS + submit + sample_meta + copy-paste commands for your HPC.
write_hpc_bundle <- function(dest_zip, scauto_root, project, input_dir, samples, groups,
                             est, outdir_hint = "", meta = NULL) {
  samples <- as.character(samples)
  if (!length(samples)) stop("No samples selected")
  if (length(groups) != length(samples)) groups <- rep("", length(samples))
  groups <- as.character(groups)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  work <- tempfile(paste0("scauto_hpc_pack_", stamp, "_"))
  dir.create(file.path(work, "hpc"), recursive = TRUE)
  n_cells <- if (!is.null(est$n_cells)) est$n_cells else NA_real_
  est_gb <- if (!is.null(est$est_gb)) est$est_gb else NA_real_
  mem_gb <- if (!is.null(est$pbs_mem_gb) && is.finite(est$pbs_mem_gb)) {
    as.integer(est$pbs_mem_gb)
  } else {
    suggest_pbs_mem_gb(est_gb, n_cells)
  }
  ncpus <- 8L
  wall <- if (is.finite(n_cells) && n_cells > 120000) "72:00:00" else "48:00:00"
  out_name <- if (nzchar(outdir_hint)) basename(outdir_hint) else paste0("scauto_hpc_", stamp)
  local_in <- if (nzchar(input_dir)) normalizePath(input_dir, winslash = "/", mustWork = FALSE) else ""
  hpc_project <- "$HOME/scauto"
  hpc_in <- paste0(hpc_project, "/scauto_hpc_input/", stamp)
  hpc_out <- paste0(hpc_project, "/results/", out_name)
  hpc_meta <- paste0(hpc_in, "/sample_meta.csv")
  if (!is.null(meta) && is.data.frame(meta) && nrow(meta) && "sample" %in% names(meta)) {
    sample_csv <- meta[as.character(meta$sample) %in% samples, , drop = FALSE]
    drop <- intersect("n_barcodes", names(sample_csv))
    if (length(drop)) sample_csv <- sample_csv[, setdiff(names(sample_csv), drop), drop = FALSE]
  } else {
    sample_csv <- data.frame(sample = samples, group = groups, stringsAsFactors = FALSE)
  }
  utils::write.csv(sample_csv, file.path(work, "sample_meta.csv"), row.names = FALSE)
  writeLines(samples, file.path(work, "selected_samples.txt"))
  writeLines(
    c(
      paste("written:", format(Sys.time())),
      paste("n_samples:", length(samples)),
      paste("n_cells_before_qc:", if (is.finite(n_cells)) round(n_cells) else "NA"),
      paste("estimated_ram_gb:", if (is.finite(est_gb)) est_gb else "NA"),
      paste("this_computer_ram_gb:", if (!is.null(est$host_gb)) est$host_gb else "NA"),
      paste("free_disk_gb:", if (!is.null(est$disk_gb)) est$disk_gb else "NA"),
      paste("n_cores:", if (!is.null(est$n_cores)) est$n_cores else "NA"),
      paste("resource_level:", if (!is.null(est$level)) est$level else "NA"),
      paste("suggested_pbs_mem_gb:", mem_gb),
      paste("suggested_ncpus:", ncpus),
      paste("suggested_walltime:", wall),
      paste("local_input_dir:", local_in),
      paste("samples:", paste(samples, collapse = ","))
    ),
    file.path(work, "resource_estimate.txt")
  )
  pbs <- c(
    "#!/bin/bash",
    "# PBMC Auto-Pilot — generated by the local helper when this computer is too small.",
    "#PBS -N scauto",
    "#PBS -q medium",
    paste0("#PBS -l select=1:ncpus=", ncpus, ":mem=", mem_gb, "gb"),
    paste0("#PBS -l walltime=", wall),
    "#PBS -j oe",
    paste0("#PBS -o ", hpc_project, "/hpc/logs/"),
    "set -euo pipefail",
    paste0("PROJECT_DIR=\"${PROJECT_DIR:-", hpc_project, "}\""),
    "cd \"${PBS_O_WORKDIR:-${PROJECT_DIR}}\"",
    "if [[ -f /usr/share/lmod/lmod/init/bash ]]; then",
    "  # shellcheck disable=SC1091",
    "  set +u",
    "  source /usr/share/lmod/lmod/init/bash",
    "  set -u",
    "fi",
    "# shellcheck disable=SC1091",
    "source \"${PROJECT_DIR}/hpc/config.env\"",
    "mkdir -p \"${PROJECT_DIR}/hpc/logs\" \"${PROJECT_DIR}/results\"",
    "module purge 2>/dev/null || true",
    "module load \"${R_MODULE}\"",
    "export R_LIBS_USER=\"${PROJECT_DIR}/Rlib\"",
    paste0("export OMP_NUM_THREADS=", ncpus),
    paste0("export MKL_NUM_THREADS=", ncpus),
    paste0("export OPENBLAS_NUM_THREADS=", ncpus),
    "export SCAUTO_NO_PKG_INSTALL=1",
    "export CELLTYPIST_FOLDER=\"${PROJECT_DIR}/.cache/celltypist\"",
    "export XDG_CACHE_HOME=\"${PROJECT_DIR}/.cache\"",
    paste0("INPUT=\"${SCAUTO_INPUT:-", hpc_in, "}\""),
    paste0("META=\"${SCAUTO_META:-", hpc_meta, "}\""),
    paste0("OUT=\"${SCAUTO_OUT:-", hpc_out, "}\""),
    paste0("SAMPLES=\"${SCAUTO_SAMPLES:-", paste(samples, collapse = ","), "}\""),
    "cmd=(Rscript \"${PROJECT_DIR}/tools/scauto/run_scauto.R\"",
    "  --project \"${PROJECT_DIR}\"",
    "  --input \"${INPUT}\"",
    "  --meta \"${META}\"",
    "  --outdir \"${OUT}\"",
    "  --allow-large)",
    "if [[ -n \"${SAMPLES}\" ]]; then",
    "  cmd+=(--samples \"${SAMPLES}\")",
    "fi",
    "echo \"host=$(hostname) job=${PBS_JOBID:-local}\"; date",
    "\"${cmd[@]}\"",
    "echo SCAUTO_HPC_DONE; date"
  )
  writeLines(pbs, file.path(work, "hpc", "run_scauto.pbs"))
  sub <- c(
    "#!/usr/bin/env bash",
    "# Submit the generated PBS job on your HPC. Run this ON the cluster, after rsync.",
    "set -euo pipefail",
    paste0("PROJECT_DIR=\"${PROJECT_DIR:-", hpc_project, "}\""),
    "export PATH=\"/opt/pbs/bin:${PATH}\"",
    "cd \"${PROJECT_DIR}\"",
    "# shellcheck disable=SC1091",
    "source \"${PROJECT_DIR}/hpc/config.env\"",
    "mkdir -p hpc/logs",
    "HERE=\"$(cd \"$(dirname \"$0\")\" && pwd)\"",
    "QSUB=\"${PBS_BIN:-/opt/pbs/bin/qsub}\"",
    "command -v \"${QSUB}\" >/dev/null || QSUB=\"$(command -v qsub)\"",
    paste0("JOB=$(\"${QSUB}\" -q medium -N scauto -l select=1:ncpus=", ncpus, ":mem=", mem_gb, "gb -l walltime=", wall, " \\"),
    "  \"${HERE}/run_scauto.pbs\")",
    "echo \"Submitted: ${JOB}\"",
    "echo \"Monitor: qstat -u ${USER}\""
  )
  writeLines(sub, file.path(work, "hpc", "submit.sh"))
  rsync_ex <- c(
    "#!/usr/bin/env bash",
    "# Example: copy the selected 10x folders from this computer to your HPC.",
    "# Edit HPC_HOST if your SSH alias is not hpc.",
    "set -euo pipefail",
    "HPC_HOST=\"${HPC_HOST:-hpc}\"",
    paste0("HPC_IN=\"", hpc_in, "\""),
    paste0("LOCAL_IN=\"", local_in, "\""),
    "ssh -o BatchMode=yes \"${HPC_HOST}\" \"mkdir -p '${hpc_in}'\"",
    "rsync -avP \"${LOCAL_IN}/\" \"${HPC_HOST}:${hpc_in}/\"",
    paste0("rsync -avP \"$(cd \"$(dirname \"$0\")\" && pwd)/../sample_meta.csv\" \"${HPC_HOST}:${hpc_meta}\""),
    "echo \"Data on HPC: ${HPC_IN}\""
  )
  writeLines(rsync_ex, file.path(work, "hpc", "rsync_data.example.sh"))
  cmds <- c(
    "# Copy-paste on your HPC after the 10x folders and this pack are on the cluster.",
    paste0("# Suggested resources: ", ncpus, " CPUs, ", mem_gb, " GB RAM, walltime ", wall),
    "",
    paste0("export PROJECT_DIR=", hpc_project),
    "source \"$PROJECT_DIR/hpc/config.env\"",
    "module load \"$R_MODULE\"",
    "export R_LIBS_USER=\"$PROJECT_DIR/Rlib\"",
    "# First time on the login node only (compute nodes have no internet):",
    "Rscript \"$PROJECT_DIR/tools/scauto/check_install.R\" --install",
    "",
    paste0("chmod +x hpc/submit.sh   # inside the unzipped pack"),
    "bash hpc/submit.sh",
    "",
    "# Or one line:",
    paste0(
      "qsub -q medium -N scauto -l select=1:ncpus=", ncpus, ":mem=", mem_gb,
      "gb -l walltime=", wall, " \\"
    ),
    "  hpc/run_scauto.pbs",
    "",
    "qstat -u \"$USER\"",
    paste0("# Results: ", hpc_out)
  )
  writeLines(cmds, file.path(work, "COMMANDS.txt"))
  readme <- c(
    "# HPC pack from PBMC Auto-Pilot",
    "",
    "This computer does not have enough RAM/disk for the samples you ticked.",
    "Use the university cluster (your HPC / OpenPBS queue `medium`) instead of Start on the laptop.",
    "",
    "## What is in this zip",
    "",
    "- `sample_meta.csv` — the samples and groups from the helper",
    "- `selected_samples.txt` / `resource_estimate.txt` — cell count and RAM estimate",
    "- `COMMANDS.txt` — copy-paste commands",
    "- `hpc/run_scauto.pbs` — PBS job (filled in)",
    "- `hpc/submit.sh` — `qsub` wrapper",
    "- `hpc/rsync_data.example.sh` — example copy of the 10x folders",
    "",
    "## What you still need on the cluster",
    "",
    "1. The **whole project** (`tools/scauto`, `viz/`, `hpc/config.env`, `Rlib`).",
    "2. The **10x folders** you ticked (matrix.mtx + barcodes + features).",
    paste0("   Local path on this computer: `", local_in, "`"),
    paste0("   Suggested HPC path: `", hpc_in, "`"),
    "3. R packages in `$PROJECT/Rlib` (install on the **login** node; compute nodes have no internet).",
    "",
    "## Steps",
    "",
    "1. Unzip this pack.",
    "2. Copy the 10x sample folders to the cluster (see `hpc/rsync_data.example.sh`).",
    "   If the project is already on HPC, rsync only the new input folder and `sample_meta.csv`.",
    paste0("3. If the HPC project path is not `", hpc_project, "`, edit `PROJECT_DIR` in `hpc/run_scauto.pbs` and `hpc/submit.sh`."),
    "4. On the login node: `Rscript tools/scauto/check_install.R --install` (once).",
    "5. `chmod +x hpc/submit.sh && bash hpc/submit.sh`",
    "6. `qstat -u $USER` until the job finishes, then download `results/`.",
    "",
    paste0("PBS request: ", ncpus, " CPUs, ", mem_gb, " GB, walltime ", wall,
           " (your site's queue and resource limits)."),
    "",
    "Azimuth / CellTypist stay off unless you enable them in `tools/scauto/config/default.yaml`",
    "and pre-download references into `$PROJECT/.cache` on the login node.",
    "",
    if (!is.null(est$headline)) est$headline else "",
    if (!is.null(est$detail)) est$detail else ""
  )
  writeLines(readme, file.path(work, "README.md"))
  zip_folder(work, dest_zip)
  unlink(work, recursive = TRUE)
  normalizePath(dest_zip, winslash = "/", mustWork = TRUE)
}

write_run_meta <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(df, path, row.names = FALSE)
  normalizePath(path)
}

list_10x_files <- function(mat_dir) {
  files <- list.files(mat_dir, full.names = TRUE)
  keep <- files[grepl(
    "^(matrix\\.mtx|barcodes\\.tsv|features\\.tsv|genes\\.tsv|metadata)",
    basename(files),
    ignore.case = TRUE
  )]
  if (!length(keep)) {
    return(data.frame(file = character(), path = character(), size_mb = numeric(),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    file = basename(keep),
    path = keep,
    size_mb = round(file.info(keep)$size / 1024^2, 2),
    stringsAsFactors = FALSE
  )
}

read_head_lines <- function(path, n = 40L) {
  con <- if (grepl("\\.gz$", path, ignore.case = TRUE)) gzfile(path, "rt") else file(path, "rt")
  on.exit(close(con), add = TRUE)
  readLines(con, n = n, warn = FALSE)
}

read_tail_lines <- function(path, n = 80L, max_bytes = 131072L) {
  if (!file.exists(path)) return(character())
  sz <- file.info(path)$size
  if (is.na(sz) || sz < 1) return(character())
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  nread <- as.integer(min(sz, max_bytes))
  seek(con, where = sz - nread, origin = "start")
  raw <- readBin(con, what = "raw", n = nread)
  if (!length(raw)) return(character())
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  if (sz > nread) txt <- sub("^[^\n]*\n", "", txt)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  utils::tail(lines, n)
}

# Full log for the Run-page window so the first line is reachable. Oversized
# files keep the start plus the latest tail (middle omitted).
read_log_display_lines <- function(path, max_bytes = 1048576L) {
  if (!nzchar(path) || !file.exists(path)) return(character())
  sz <- file.info(path)$size
  if (is.na(sz) || sz < 1) return(character())
  split_txt <- function(txt) {
    if (!nzchar(txt)) return(character())
    strsplit(txt, "\n", fixed = TRUE)[[1]]
  }
  as_utf8 <- function(raw) {
    if (!length(raw)) return("")
    txt <- rawToChar(raw)
    Encoding(txt) <- "UTF-8"
    txt
  }
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  if (sz <= max_bytes) {
    return(split_txt(as_utf8(readBin(con, what = "raw", n = as.integer(sz)))))
  }
  n_head <- as.integer(floor(max_bytes * 0.55))
  n_tail <- as.integer(max_bytes - n_head)
  head_txt <- as_utf8(readBin(con, what = "raw", n = n_head))
  seek(con, where = sz - n_tail, origin = "start")
  tail_txt <- as_utf8(readBin(con, what = "raw", n = n_tail))
  head_txt <- sub("\n[^\n]*$", "\n", head_txt)
  tail_txt <- sub("^[^\n]*\n", "", tail_txt)
  omitted_kb <- round((sz - n_head - n_tail) / 1024)
  c(
    split_txt(head_txt),
    paste0(
      "----- [middle omitted (~", omitted_kb, " KB); full file: ",
      basename(path), "] -----"
    ),
    split_txt(tail_txt)
  )
}

empty_run_files <- function() {
  data.frame(rel = character(), path = character(), size_mb = numeric(),
             kind = character(), stringsAsFactors = FALSE)
}

list_run_files <- function(outdir, skip_deg_bulk = FALSE) {
  if (!nzchar(outdir) || !dir.exists(outdir)) return(empty_run_files())
  outdir <- normalizePath(outdir, winslash = "/", mustWork = TRUE)
  paths <- list.files(outdir, full.names = TRUE, recursive = TRUE)
  paths <- paths[!grepl("/\\.", paths)]
  if (!length(paths)) return(empty_run_files())
  paths <- normalizePath(paths, winslash = "/", mustWork = FALSE)
  rel <- substring(paths, nchar(outdir) + 2L)
  if (isTRUE(skip_deg_bulk)) {
    bulk <- grepl("/deg_(wilcoxon|MAST)/", rel) & !grepl("/00_", rel)
    paths <- paths[!bulk]
    rel <- rel[!bulk]
  }
  if (!length(paths)) return(empty_run_files())
  ext <- tolower(tools::file_ext(paths))
  kind <- ifelse(ext %in% c("png", "jpg", "jpeg", "pdf"), "figure",
                 ifelse(ext %in% c("csv", "tsv"), "table",
                        ifelse(ext %in% c("md", "txt", "log", "json"), "text",
                               ifelse(ext %in% c("rds"), "object", "file"))))
  data.frame(
    rel = rel,
    path = paths,
    size_mb = round(file.info(paths)$size / 1024^2, 3),
    kind = kind,
    stringsAsFactors = FALSE
  )
}

pipeline_finished_in_log <- function(outdir) {
  lf <- file.path(outdir, "logs", "pipeline.log")
  if (!nzchar(outdir) || !file.exists(lf)) return(FALSE)
  lines <- tryCatch(read_tail_lines(lf, n = 30L, max_bytes = 16384L),
                    error = function(e) character())
  if (!length(lines)) return(FALSE)
  tailn <- paste(lines, collapse = "\n")
  grepl(" ANALYSIS FINISHED ", tailn, fixed = TRUE) ||
    grepl(" STOPPED BY USER ", tailn, fixed = TRUE) ||
    grepl("SCAUTO_HPC_DONE", tailn, fixed = TRUE) ||
    grepl("(^|\n)[0-9: -]{19} DONE ", tailn)
}

hpc_settings_file <- function() {
  file.path(path.expand("~"), ".scauto_hpc.json")
}

default_hpc_settings <- function() {
  list(
    host = "",
    user = "",
    remote_project = "",
    last_outdir = ""
  )
}

read_hpc_settings <- function() {
  p <- hpc_settings_file()
  out <- default_hpc_settings()
  if (!file.exists(p)) return(out)
  raw <- tryCatch(jsonlite::fromJSON(p, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(raw) || !is.list(raw)) return(out)
  for (k in names(out)) {
    if (!is.null(raw[[k]]) && nzchar(as.character(raw[[k]])[1])) {
      out[[k]] <- as.character(raw[[k]])[1]
    }
  }
  out
}

write_hpc_settings <- function(user = NULL, host = NULL, remote_project = NULL,
                               last_outdir = NULL) {
  p <- hpc_settings_file()
  payload <- read_hpc_settings()
  if (!is.null(user)) payload$user <- as.character(user)[1]
  if (!is.null(host)) payload$host <- as.character(host)[1]
  if (!is.null(remote_project)) payload$remote_project <- as.character(remote_project)[1]
  if (!is.null(last_outdir)) payload$last_outdir <- as.character(last_outdir)[1]
  tryCatch(
    jsonlite::write_json(payload, p, auto_unbox = TRUE, pretty = TRUE),
    error = function(e) NULL
  )
  invisible(p)
}

hpc_safe_token <- function(x, what = "value") {
  x <- trimws(as.character(x)[1])
  if (!nzchar(x)) return(x)
  if (grepl("[;|&`$<>(){}\n\r\\\\]", x)) {
    stop(sprintf("Unsafe characters in %s. Use letters, numbers, / . _ - @ only.", what), call. = FALSE)
  }
  x
}

hpc_ssh_target <- function(user, host) {
  host <- hpc_safe_token(host, "HPC host")
  user <- trimws(as.character(user)[1])
  if (!nzchar(host)) stop("Enter the HPC host (for example hpc.example.edu or your SSH alias hpc).", call. = FALSE)
  if (grepl("@", host, fixed = TRUE)) return(host)
  if (nzchar(user)) {
    user <- hpc_safe_token(user, "HPC username")
    return(paste0(user, "@", host))
  }
  host
}

hpc_bash <- function() {
  b <- Sys.which("bash")
  if (nzchar(b)) return(unname(b))
  cands <- c(
    "C:/Program Files/Git/bin/bash.exe",
    "C:/Program Files (x86)/Git/bin/bash.exe"
  )
  hit <- cands[file.exists(cands)]
  if (length(hit)) return(hit[[1]])
  ""
}

hpc_has_ssh <- function() nzchar(Sys.which("ssh"))

hpc_ssh_opts <- function(timeout = 20) {
  c(
    "-n",
    "-o", "BatchMode=yes",
    "-o", paste0("ConnectTimeout=", as.integer(timeout)),
    "-o", "StrictHostKeyChecking=accept-new",
    "-o", "LogLevel=ERROR"
  )
}

hpc_run_ssh <- function(target, remote_cmd, timeout = 20) {
  ssh <- Sys.which("ssh")
  if (!nzchar(ssh)) {
    return(list(ok = FALSE, stdout = "", stderr = "ssh is not installed on this computer."))
  }
  args <- c(hpc_ssh_opts(timeout), target, remote_cmd)
  out <- tryCatch(
    processx::run(
      ssh, args,
      error_on_status = FALSE,
      timeout = timeout + 8,
      spinner = FALSE
    ),
    error = function(e) list(status = 1L, stdout = "", stderr = conditionMessage(e))
  )
  list(
    ok = identical(as.integer(out$status), 0L),
    stdout = paste(out$stdout, collapse = "\n"),
    stderr = paste(out$stderr, collapse = "\n"),
    status = out$status
  )
}

hpc_parse_probe <- function(txt) {
  kv <- list(home = "", user = "", unit_dir = "", lab_project = "", has_qsub = FALSE)
  if (!nzchar(txt)) return(kv)
  grab <- function(key) {
    m <- regexec(paste0("(^|\n)", key, "=([^\n]*)"), txt)
    h <- regmatches(txt, m)[[1]]
    if (length(h) >= 3L) trimws(h[[3]]) else ""
  }
  kv$home <- grab("HOME")
  kv$user <- grab("USER")
  kv$unit_dir <- grab("UNIT_DIR")
  kv$lab_project <- grab("LAB_PROJECT")
  kv$has_qsub <- identical(grab("HAS_QSUB"), "1")
  kv
}

hpc_suggest_project <- function(probe, user) {
  if (!is.null(probe$lab_project) && nzchar(probe$lab_project)) return(probe$lab_project)
  if (!is.null(probe$unit_dir) && nzchar(probe$unit_dir)) {
    return(file.path(probe$unit_dir, "scauto_work"))
  }
  if (!is.null(probe$home) && nzchar(probe$home)) {
    return(paste0(probe$home, "/scauto_work"))
  }
  if (nzchar(user)) return(paste0("/home/", user, "/scauto_work"))
  "~/scauto_work"
}

hpc_test_connection <- function(user, host) {
  if (!hpc_has_ssh()) {
    return(list(
      ok = FALSE, target = "",
      message = "This computer has no ssh command. On Windows install OpenSSH or Git for Windows. On a Mac it is already there.",
      suggested_project = "", probe = NULL
    ))
  }
  target <- tryCatch(hpc_ssh_target(user, host), error = function(e) {
    return(list(ok = FALSE, target = "", message = conditionMessage(e),
                suggested_project = "", probe = NULL))
  })
  if (is.list(target) && isFALSE(target$ok)) return(target)
  remote <- paste(
    "export PATH=/opt/pbs/bin:$PATH",
    "printf 'HOME=%s\\nUSER=%s\\n' \"$HOME\" \"$USER\"",
    "if command -v qsub >/dev/null 2>&1; then echo HAS_QSUB=1; else echo HAS_QSUB=0; fi",
    sep = "; "
  )
  res <- hpc_run_ssh(target, remote, timeout = 20)
  if (!isTRUE(res$ok)) {
    err <- paste(res$stderr, res$stdout)
    hint <- if (grepl("Permission denied|Host key verification|Host key|Could not resolve|Connection timed|Connection refused|Host is down", err, ignore.case = TRUE)) {
      paste(
        "SSH did not log in. One-time setup: in Terminal run  ssh", target,
        "  accept the host fingerprint, then  ssh-copy-id", target,
        "  so a password is not needed. The helper cannot type your password."
      )
    } else {
      paste("SSH failed.", if (nzchar(err)) err else "Check host, username, and VPN.")
    }
    return(list(ok = FALSE, target = target, message = hint, suggested_project = "", probe = NULL))
  }
  probe <- hpc_parse_probe(res$stdout)
  sug <- hpc_suggest_project(probe, if (nzchar(user)) user else probe$user)
  qtxt <- if (isTRUE(probe$has_qsub)) "qsub is available." else "qsub was not on PATH; the job script will use /opt/pbs/bin/qsub."
  list(
    ok = TRUE,
    target = target,
    message = paste0(
      "Logged in as ", if (nzchar(probe$user)) probe$user else target, ". ",
      qtxt, " Suggested folder: ", sug
    ),
    suggested_project = sug,
    probe = probe
  )
}

hpc_write_config_env <- function(path, user, host, hpc_project, r_module) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- c(
    paste0("export HPC_USER=\"", user, "\""),
    paste0("export HPC_HOST=\"", host, "\""),
    paste0("export HPC_PROJECT_DIR=\"", hpc_project, "\""),
    paste0("export PROJECT_DIR=\"", hpc_project, "\""),
    paste0("export LOCAL_PROJECT_DIR=\"", hpc_project, "\""),
    "export PBS_BIN=\"/opt/pbs/bin/qsub\"",
    "export PBS_QUEUE=\"medium\"",
    paste0("export R_MODULE=\"", r_module, "\"")
  )
  writeLines(lines, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

hpc_write_pbs_file <- function(path, hpc_project, hpc_in, hpc_out, hpc_meta,
                               samples, ncpus, mem_gb, wall, config_remote = "",
                               usage_user = "", usage_host = "") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  samples <- paste(as.character(samples), collapse = ",")
  config_remote <- trimws(as.character(config_remote)[1])
  if (!nzchar(config_remote)) config_remote <- ""
  pbs <- c(
    "#!/bin/bash",
    "# Generated by PBMC Auto-Pilot one-click HPC.",
    "#PBS -N scauto",
    "#PBS -q medium",
    paste0("#PBS -l select=1:ncpus=", ncpus, ":mem=", mem_gb, "gb"),
    paste0("#PBS -l walltime=", wall),
    "#PBS -j oe",
    paste0("#PBS -o ", hpc_project, "/hpc/logs/"),
    "set -euo pipefail",
    paste0("PROJECT_DIR=\"${PROJECT_DIR:-", hpc_project, "}\""),
    "cd \"${PBS_O_WORKDIR:-${PROJECT_DIR}}\"",
    "if [[ -f /usr/share/lmod/lmod/init/bash ]]; then",
    "  # shellcheck disable=SC1091",
    "  set +u",
    "  source /usr/share/lmod/lmod/init/bash",
    "  set -u",
    "fi",
    "if [[ -f \"${PROJECT_DIR}/hpc/config.env\" ]]; then",
    "  # shellcheck disable=SC1091",
    "  source \"${PROJECT_DIR}/hpc/config.env\"",
    "fi",
    "if [[ -f \"${PROJECT_DIR}/hpc/scauto_oneclick.env\" ]]; then",
    "  # shellcheck disable=SC1091",
    "  source \"${PROJECT_DIR}/hpc/scauto_oneclick.env\"",
    "fi",
    paste0("export PROJECT_DIR=\"${PROJECT_DIR:-", hpc_project, "}\""),
    "export LOCAL_PROJECT_DIR=\"${PROJECT_DIR}\"",
    "mkdir -p \"${PROJECT_DIR}/hpc/logs\" \"${PROJECT_DIR}/results\"",
    "module purge 2>/dev/null || true",
    "module load \"${R_MODULE}\"",
    "export R_LIBS_USER=\"${PROJECT_DIR}/Rlib\"",
    paste0("export OMP_NUM_THREADS=", ncpus),
    paste0("export MKL_NUM_THREADS=", ncpus),
    paste0("export OPENBLAS_NUM_THREADS=", ncpus),
    "export SCAUTO_NO_PKG_INSTALL=1",
    if (nzchar(as.character(usage_user)[1])) {
      paste0("export SCAUTO_USAGE_USER=\"", gsub('"', "", as.character(usage_user)[1]), "\"")
    } else {
      NULL
    },
    if (nzchar(as.character(usage_host)[1])) {
      paste0("export SCAUTO_USAGE_HOST=\"", gsub('"', "", as.character(usage_host)[1]), "\"")
    } else {
      NULL
    },
    "export CELLTYPIST_FOLDER=\"${PROJECT_DIR}/.cache/celltypist\"",
    "export XDG_CACHE_HOME=\"${PROJECT_DIR}/.cache\"",
    paste0("INPUT=\"${SCAUTO_INPUT:-", hpc_in, "}\""),
    paste0("META=\"${SCAUTO_META:-", hpc_meta, "}\""),
    paste0("OUT=\"${SCAUTO_OUT:-", hpc_out, "}\""),
    paste0("SAMPLES=\"${SCAUTO_SAMPLES:-", samples, "}\""),
    paste0("CONFIG=\"${SCAUTO_CONFIG:-", config_remote, "}\""),
    "cmd=(Rscript \"${PROJECT_DIR}/tools/scauto/run_scauto.R\"",
    "  --project \"${PROJECT_DIR}\"",
    "  --input \"${INPUT}\"",
    "  --meta \"${META}\"",
    "  --outdir \"${OUT}\"",
    "  --allow-large)",
    "if [[ -n \"${SAMPLES}\" ]]; then",
    "  cmd+=(--samples \"${SAMPLES}\")",
    "fi",
    "if [[ -n \"${CONFIG}\" ]]; then",
    "  cmd+=(--config \"${CONFIG}\")",
    "fi",
    "echo \"host=$(hostname) job=${PBS_JOBID:-local}\"; date",
    "\"${cmd[@]}\"",
    "echo SCAUTO_HPC_DONE; date"
  )
  writeLines(pbs, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

sh_export_line <- function(name, value) {
  sprintf("%s=%s", name, shQuote(as.character(value)[1], type = "sh"))
}

## Locked defaults for Start analysis. Advanced Settings writes an overlay YAML
## that `run_scauto.R --config` deep-merges onto config/default.yaml.
scauto_advanced_defaults <- function() {
  list(
    tissue = "pbmc",
    max_percent_mt = 10,
    min_ncount = 1000,
    max_ncount = 40000,
    min_nfeature = 0,
    max_nfeature = 8000,
    n_hvg = 2000,
    npcs = 50,
    cluster_dims = 30,
    cluster_resolution = 0.8,
    n_neighbors = 20,
    harmony_enable = "auto",
    harmony_theta = 4,
    recluster_resolution = 0.4,
    recluster_min_clusters = 3,
    deg_enable = TRUE,
    deg_padj = 0.05,
    deg_lfc = 0.25,
    save_rds = TRUE,
    cassia_enable = "auto",
    pathway_go = TRUE,
    pathway_kegg = TRUE,
    pathway_gsea = TRUE,
    pathway_gsva = TRUE,
    pathway_reactome = TRUE,
    cytotrace2 = FALSE,
    monocle3 = FALSE,
    traj_lineages = character(),
    clinical_numeric_enable = FALSE,
    clinical_numeric_cols = character()
  )
}

sanitize_advanced_params <- function(p) {
  d0 <- scauto_advanced_defaults()
  d <- d0
  if (is.list(p) && length(p)) {
    for (nm in names(d0)) {
      if (!is.null(p[[nm]]) && !identical(p[[nm]], "")) d[[nm]] <- p[[nm]]
    }
  }
  clamp_num <- function(x, default, lo, hi, as_int = FALSE) {
    v <- suppressWarnings(if (as_int) as.integer(x)[1] else as.numeric(x)[1])
    if (!is.finite(v)) v <- default
    max(lo, min(hi, v))
  }
  d$tissue <- if (exists("scauto_normalize_tissue", mode = "function")) {
    scauto_normalize_tissue(d$tissue)
  } else {
    t <- tolower(trimws(as.character(d$tissue)[1]))
    if (grepl("liver", t)) "liver" else "pbmc"
  }
  if (identical(d$tissue, "liver") &&
      (!is.finite(suppressWarnings(as.numeric(d$max_percent_mt)[1])) ||
       as.numeric(d$max_percent_mt)[1] <= 10)) {
    d$max_percent_mt <- 50
  }
  d$max_percent_mt <- clamp_num(d$max_percent_mt, d0$max_percent_mt, 0, 100)
  d$min_ncount <- clamp_num(d$min_ncount, d0$min_ncount, 0, 1e7, as_int = TRUE)
  d$max_ncount <- clamp_num(d$max_ncount, d0$max_ncount, d$min_ncount + 1, 1e8, as_int = TRUE)
  d$min_nfeature <- clamp_num(d$min_nfeature, d0$min_nfeature, 0, 50000, as_int = TRUE)
  d$max_nfeature <- clamp_num(d$max_nfeature, d0$max_nfeature, max(d$min_nfeature + 1L, 50L), 100000, as_int = TRUE)
  d$n_hvg <- clamp_num(d$n_hvg, d0$n_hvg, 200, 8000, as_int = TRUE)
  d$npcs <- clamp_num(d$npcs, d0$npcs, 10, 100, as_int = TRUE)
  d$cluster_dims <- clamp_num(d$cluster_dims, d0$cluster_dims, 5, as.integer(d$npcs), as_int = TRUE)
  d$cluster_resolution <- clamp_num(d$cluster_resolution, d0$cluster_resolution, 0.05, 2)
  d$n_neighbors <- clamp_num(d$n_neighbors, d0$n_neighbors, 5, 80, as_int = TRUE)
  he <- tolower(trimws(as.character(d$harmony_enable)[1]))
  d$harmony_enable <- if (he %in% c("yes", "true", "on")) "yes" else if (he %in% c("no", "false", "off")) "no" else "auto"
  d$harmony_theta <- clamp_num(d$harmony_theta, d0$harmony_theta, 0, 10)
  d$recluster_resolution <- clamp_num(d$recluster_resolution, d0$recluster_resolution, 0.05, 2)
  d$recluster_min_clusters <- clamp_num(d$recluster_min_clusters, d0$recluster_min_clusters, 2, 20, as_int = TRUE)
  deg_v <- as.logical(d$deg_enable)[1]
  d$deg_enable <- if (is.na(deg_v)) d0$deg_enable else isTRUE(deg_v)
  d$deg_padj <- clamp_num(d$deg_padj, d0$deg_padj, 1e-6, 0.2)
  d$deg_lfc <- clamp_num(d$deg_lfc, d0$deg_lfc, 0, 2)
  rds_v <- as.logical(d$save_rds)[1]
  d$save_rds <- if (is.na(rds_v)) d0$save_rds else isTRUE(rds_v)
  ce <- tolower(trimws(as.character(d$cassia_enable)[1]))
  d$cassia_enable <- if (ce %in% c("yes", "true", "on")) "yes" else if (ce %in% c("no", "false", "off")) "no" else "auto"
  as_bool <- function(x, default) {
    v <- as.logical(x)[1]
    if (is.na(v)) default else isTRUE(v)
  }
  d$pathway_go <- as_bool(d$pathway_go, d0$pathway_go)
  d$pathway_kegg <- as_bool(d$pathway_kegg, d0$pathway_kegg)
  d$pathway_gsea <- as_bool(d$pathway_gsea, d0$pathway_gsea)
  d$pathway_gsva <- as_bool(d$pathway_gsva, d0$pathway_gsva)
  d$pathway_reactome <- as_bool(d$pathway_reactome, d0$pathway_reactome)
  d$cytotrace2 <- as_bool(d$cytotrace2, d0$cytotrace2)
  d$monocle3 <- as_bool(d$monocle3, d0$monocle3)
  lin <- d$traj_lineages
  if (is.null(lin)) lin <- character()
  lin <- unique(as.character(lin))
  lin <- lin[nzchar(lin) & lin %in% unname(scauto_superlineage_choices())]
  d$traj_lineages <- lin
  cn <- as.logical(d$clinical_numeric_enable)[1]
  d$clinical_numeric_enable <- if (is.na(cn)) FALSE else isTRUE(cn)
  cols <- unlist(d$clinical_numeric_cols, use.names = FALSE)
  cols <- unique(as.character(cols))
  d$clinical_numeric_cols <- cols[nzchar(cols)]
  d
}

overlay_params_for_run <- function(adv = NULL, tissue = "pbmc") {
  p <- sanitize_advanced_params(adv)
  tissue <- if (exists("scauto_normalize_tissue", mode = "function")) {
    scauto_normalize_tissue(c(tissue, p$tissue))
  } else if (grepl("liver", tolower(paste(tissue, p$tissue)))) {
    "liver"
  } else {
    "pbmc"
  }
  p$tissue <- tissue
  if (identical(tissue, "liver") && as.numeric(p$max_percent_mt)[1] <= 10) {
    p$max_percent_mt <- 50
  }
  p
}

run_needs_config_overlay <- function(adv = NULL, tissue = "pbmc") {
  p <- overlay_params_for_run(adv, tissue)
  !is.null(adv) || identical(p$tissue, "liver")
}

scauto_tissue_choices <- function() {
  c(
    "PBMC / blood — mitochondrial genes ≤ 10%" = "pbmc",
    "Liver — mitochondrial genes ≤ 50%" = "liver"
  )
}

scauto_superlineage_choices <- function() {
  c(
    "CD4 T" = "CD4_T",
    "CD8 T + unconventional" = "CD8_T_unconv",
    "NK" = "NK",
    "Mono + DC" = "Mono_DC",
    "B + plasma" = "B_plasma"
  )
}

yaml_bool <- function(x) if (isTRUE(x)) "true" else "false"

yaml_quote <- function(x) {
  x <- as.character(x)[1]
  sprintf('"%s"', gsub("\\\\", "\\\\\\\\", gsub('"', '\\"', x, fixed = TRUE), fixed = TRUE))
}

harmony_enable_yaml <- function(x) {
  switch(as.character(x)[1], yes = "true", no = "false", "auto")
}

write_advanced_config <- function(path, params) {
  p <- sanitize_advanced_params(params)
  n_hvg_raw <- max(as.integer(p$n_hvg) + 500L, 3000L)
  cassia_tissue <- if (identical(p$tissue, "liver")) "liver" else "peripheral blood"
  qc_lines <- c(
    sprintf("tissue: %s", p$tissue),
    "qc:",
    sprintf("  tissue: %s", p$tissue),
    sprintf("  max_percent_mt: %s", p$max_percent_mt),
    sprintf("  min_ncount: %s", p$min_ncount),
    sprintf("  max_ncount: %s", p$max_ncount),
    sprintf("  max_nfeature: %s", p$max_nfeature)
  )
  if (isTRUE(p$min_nfeature > 0)) {
    qc_lines <- c(qc_lines, sprintf("  min_nfeature: %s", p$min_nfeature))
  }
  lines <- c(
    "# Overlay from Advanced Settings (merged onto config/default.yaml)",
    qc_lines,
    "normalize:",
    sprintf("  n_hvg: %s", p$n_hvg),
    sprintf("  n_hvg_raw: %s", n_hvg_raw),
    sprintf("  npcs: %s", p$npcs),
    sprintf("  cluster_dims: %s", p$cluster_dims),
    "cluster:",
    sprintf("  resolution: %s", p$cluster_resolution),
    sprintf("  n_neighbors: %s", p$n_neighbors),
    "harmony:",
    sprintf("  enable: %s", harmony_enable_yaml(p$harmony_enable)),
    sprintf("  theta: %s", p$harmony_theta),
    "recluster:",
    sprintf("  resolution: %s", p$recluster_resolution),
    sprintf("  min_clusters: %s", p$recluster_min_clusters),
    "cassia:",
    sprintf("  enable: %s", harmony_enable_yaml(p$cassia_enable)),
    sprintf("  tissue: %s", cassia_tissue),
    "deg:",
    sprintf("  enable: %s", yaml_bool(p$deg_enable)),
    sprintf("  padj_cutoff: %s", p$deg_padj),
    sprintf("  lfc_cutoff: %s", p$deg_lfc),
    "pathway:",
    sprintf("  enable: %s", yaml_bool(
      isTRUE(p$pathway_go) || isTRUE(p$pathway_kegg) || isTRUE(p$pathway_gsea) ||
        isTRUE(p$pathway_gsva) || isTRUE(p$pathway_reactome)
    )),
    sprintf("  go: %s", yaml_bool(p$pathway_go)),
    sprintf("  kegg: %s", yaml_bool(p$pathway_kegg)),
    sprintf("  gsea: %s", yaml_bool(p$pathway_gsea)),
    sprintf("  gsva: %s", yaml_bool(p$pathway_gsva)),
    sprintf("  reactome: %s", yaml_bool(p$pathway_reactome)),
    "trajectory:",
    sprintf("  enable: %s", yaml_bool(
      (isTRUE(p$cytotrace2) || isTRUE(p$monocle3)) && length(p$traj_lineages) > 0L
    )),
    sprintf("  cytotrace2: %s", yaml_bool(p$cytotrace2)),
    sprintf("  monocle3: %s", yaml_bool(p$monocle3))
  )
  if (length(p$traj_lineages)) {
    lines <- c(lines, "  lineages:", paste0("    - ", p$traj_lineages))
  } else {
    lines <- c(lines, "  lineages: []")
  }
  lines <- c(
    lines,
    "clinical_numeric:",
    sprintf("  enable: %s", yaml_bool(isTRUE(p$clinical_numeric_enable))),
    if (length(p$clinical_numeric_cols)) {
      c("  columns:", paste0("    - ", vapply(p$clinical_numeric_cols, yaml_quote, character(1))))
    } else {
      "  columns: []"
    },
    "memory:",
    sprintf("  save_rds: %s", yaml_bool(isTRUE(p$save_rds))),
    sprintf("  save_rds_max_cells: %s", if (isTRUE(p$save_rds)) 1000000000L else 0L)
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

advanced_summary_html <- function(params) {
  p <- sanitize_advanced_params(params)
  paste0(
    "<span style='color:#1F3A5F'><b>Advanced overlay:</b> ",
    if (identical(p$tissue, "liver")) "liver, " else "PBMC, ",
    "mt&le;", p$max_percent_mt,
    "%, nCount ", p$min_ncount, "-", p$max_ncount,
    ", nFeature ", if (p$min_nfeature > 0) paste0(p$min_nfeature, "-") else "&le;",
    p$max_nfeature,
    ", HVG ", p$n_hvg, ", PCs ", p$npcs,
    ", Leiden res ", p$cluster_resolution,
    ", Harmony ", p$harmony_enable,
    ", CASSIA ", p$cassia_enable,
    ", DEG ", if (isTRUE(p$deg_enable)) "on" else "off",
    ", pathway ", {
      pw <- c("GO", "KEGG", "GSEA", "GSVA", "Reactome")[
        c(isTRUE(p$pathway_go), isTRUE(p$pathway_kegg), isTRUE(p$pathway_gsea),
          isTRUE(p$pathway_gsva), isTRUE(p$pathway_reactome))
      ]
      if (length(pw)) paste(pw, collapse = "+") else "off"
    },
    ", trajectory ", if (isTRUE(p$cytotrace2) || isTRUE(p$monocle3)) {
      paste0(
        paste(c(if (isTRUE(p$cytotrace2)) "CytoTRACE2", if (isTRUE(p$monocle3)) "Monocle3"), collapse = "+"),
        if (length(p$traj_lineages)) paste0(" [", paste(p$traj_lineages, collapse = ", "), "]") else " (no lineage ticked)"
      )
    } else {
      "off"
    },
    if (isTRUE(p$clinical_numeric_enable) && length(p$clinical_numeric_cols)) {
      paste0(", clinical numbers [", paste(p$clinical_numeric_cols, collapse = ", "), "]")
    } else if (isTRUE(p$clinical_numeric_enable)) {
      ", clinical numbers on"
    } else {
      ""
    },
    ", RDS ", if (isTRUE(p$save_rds)) "on" else "off",
    "</span>"
  )
}

write_hpc_job_env <- function(path, kv) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- vapply(names(kv), function(nm) sh_export_line(nm, kv[[nm]]), character(1))
  writeLines(c("#!/usr/bin/env bash", lines), path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

write_hpc_sample_map <- function(path, samples, dirs) {
  samples <- as.character(samples)
  dirs <- as.character(dirs)
  if (length(dirs) != length(samples)) stop("Sample folders do not match sample names.")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(paste(samples, dirs, sep = "\t"), path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

hpc_pid_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid)[1])
  if (!is.finite(pid) || pid < 2L) return(FALSE)
  if (.Platform$OS.type == "windows") {
    out <- suppressWarnings(system2(
      "tasklist", c("/FI", sprintf("PID eq %s", pid), "/NH"),
      stdout = TRUE, stderr = FALSE
    ))
    return(any(grepl(as.character(pid), out, fixed = TRUE)))
  }
  identical(system2("kill", c("-0", as.character(pid)), stdout = FALSE, stderr = FALSE), 0L)
}

read_hpc_job_id <- function(outdir) {
  p <- file.path(outdir, "logs", "hpc_job.id")
  if (!file.exists(p)) return("")
  trimws(paste(readLines(p, warn = FALSE), collapse = ""))
}

read_hpc_watcher_pid <- function(outdir) {
  p <- file.path(outdir, "logs", "hpc_watcher.pid")
  if (!file.exists(p)) return(NA_integer_)
  suppressWarnings(as.integer(readLines(p, n = 1L, warn = FALSE)[[1]]))
}

hpc_stage_from_log <- function(outdir) {
  lf <- file.path(outdir, "logs", "pipeline.log")
  if (!file.exists(lf)) return("idle")
  lines <- tryCatch(read_tail_lines(lf, n = 80L, max_bytes = 65536L), error = function(e) character())
  if (!length(lines)) return("idle")
  hits <- grep("SCAUTO_HPC_STAGE=", lines, value = TRUE)
  if (!length(hits)) return("starting")
  sub("^.*SCAUTO_HPC_STAGE=", "", hits[[length(hits)]])
}

hpc_stage_label <- function(stage) {
  switch(
    as.character(stage),
    ssh = "Logging in to the cluster",
    upload = "Uploading scripts and 10x folders",
    install_pkgs = "Installing R packages on the login node (first time only)",
    submit = "Submitting the PBS job",
    queued = "Waiting in the HPC queue",
    running = "Running on an HPC compute node",
    pull = "Copying results back to this computer",
    done = "Finished — results are local",
    failed = "HPC run did not finish",
    starting = "Starting HPC one-click",
    "HPC job in progress"
  )
}

# Wall-clock weights from a 4-sample HPC trial (~62 min): ingest→atlas
# clustering is ~5 min (~8–18% of the bar); five-lineage DEG + pathway is the rest.
analysis_early_progress_steps <- function() {
  list(
    list(pat = "ENVIRONMENT CHECK|Checking every required component", label = "Checking R and project files", pct = 2),
    list(pat = "R packages: ready|No missing packages", label = "R packages ready", pct = 3),
    list(pat = "Literature catalog", label = "Writing literature gene-set tables", pct = 4),
    list(pat = "Samples:", label = "Reading 10x samples", pct = 6),
    list(pat = "Reading [A-Za-z0-9]", label = "Reading 10x samples", pct = 7),
    list(pat = "RSS after ", label = "Quality control", pct = 8),
    list(pat = "Merging ", label = "Merging samples", pct = 9),
    list(pat = "NormalizeData|FindVariableFeatures|ScaleData", label = "Normalising and finding HVGs", pct = 11),
    list(pat = "RunPCA", label = "PCA", pct = 12),
    list(pat = "Harmony atlas|Harmony by sample", label = "Harmony batch correction", pct = 13),
    list(pat = "Neighbors / Leiden|RunUMAP|Atlas clusters:", label = "Clustering and UMAP", pct = 15),
    list(pat = "FindAllMarkers", label = "Finding cluster markers", pct = 17)
  )
}

read_log_for_progress <- function(path, max_bytes = 4e6) {
  if (!nzchar(path) || !file.exists(path)) return("")
  sz <- file.info(path)$size
  if (!is.finite(sz) || sz <= 0) return("")
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  n <- as.integer(min(sz, max_bytes))
  raw <- tryCatch(readBin(con, what = "raw", n = n), error = function(e) raw(0))
  if (!length(raw)) return("")
  txt <- tryCatch(rawToChar(raw, multiple = FALSE), error = function(e) "")
  Encoding(txt) <- "UTF-8"
  txt
}

progress_capture_unique <- function(txt, pattern) {
  m <- gregexpr(pattern, txt, perl = TRUE)
  if (m[[1]][1] < 0) return(character())
  hits <- regmatches(txt, m)[[1]]
  unique(sub(pattern, "\\1", hits, perl = TRUE))
}

analysis_pct_from_text <- function(txt) {
  if (!nzchar(trimws(txt))) {
    return(list(pct = 0, label = "Starting analysis"))
  }
  if (grepl(" ANALYSIS FINISHED |SCAUTO_HPC_DONE|(^| )DONE ", txt, perl = TRUE)) {
    return(list(pct = 100, label = "Writing final tables"))
  }
  rec <- progress_capture_unique(txt, "Recluster ([A-Za-z0-9_]+) n=")
  n_rec <- length(rec)
  if (n_rec > 0L) {
    path_lin <- progress_capture_unique(txt, "Pathway enrichment ([A-Za-z0-9_]+)")
    n_path <- length(intersect(path_lin, rec))
    n_exp <- max(5L, n_rec)
    cur <- rec[[length(rec)]]
    earlier <- rec[-length(rec)]
    completed <- if (!length(earlier)) 0L else sum(earlier %in% path_lin)
    frac <- 0.08
    label <- paste0("Lineage reclustering · ", cur)
    if (grepl(paste0("CASSIA (labelled|subcluster|skip|failed) ", cur), txt)) {
      frac <- max(frac, 0.12)
      label <- paste0("CASSIA subtype annotation · ", cur)
    }
    if (grepl(paste0("DEG (wilcoxon|MAST|skip) ", cur), txt, perl = TRUE)) {
      frac <- 0.28
      label <- paste0("Clinical DEGs · ", cur)
    }
    if (grepl(paste0("DEG MAST ", cur), txt)) {
      frac <- 0.55
      label <- paste0("Clinical DEGs (MAST) · ", cur)
    }
    if (grepl(paste0("Pathway enrichment ", cur), txt)) {
      frac <- 0.88
      label <- paste0("Pathway enrichment · ", cur)
    }
    units <- completed + frac
    pct <- 18 + 78 * units / n_exp
    if (n_path >= n_exp) {
      pct <- max(pct, 96)
      label <- "Writing final tables"
    }
    if (grepl("Trajectory |CytoTRACE2 |Monocle3 ", txt, perl = TRUE)) {
      pct <- max(pct, 96)
      label <- "Lineage trajectory"
    }
    if (grepl("Saving atlas RDS|Saving .*/atlas_seurat", txt)) {
      pct <- max(pct, 98)
      label <- "Saving Seurat RDS"
    }
    return(list(pct = as.integer(round(max(18, min(99, pct)))), label = label))
  }
  best <- list(pct = 2, label = "Starting analysis")
  for (st in analysis_early_progress_steps()) {
    if (grepl(st$pat, txt, perl = TRUE)) {
      best <- list(pct = st$pct, label = st$label)
    }
  }
  best
}

read_log_file_text <- function(path, n = 250L, max_bytes = 196608L) {
  if (!nzchar(path) || !file.exists(path)) return("")
  lines <- tryCatch(
    read_tail_lines(path, n = n, max_bytes = max_bytes),
    error = function(e) character()
  )
  if (!length(lines)) return("")
  paste(lines, collapse = "\n")
}

n_expected_hpc_samples <- function(outdir) {
  p <- file.path(outdir, "logs", "hpc_sample_map.tsv")
  if (!file.exists(p)) return(NA_integer_)
  n <- length(readLines(p, warn = FALSE))
  as.integer(n)
}

pipeline_progress_from_log <- function(outdir, mode = "local") {
  pipe <- if (!nzchar(outdir)) "" else read_log_for_progress(file.path(outdir, "logs", "pipeline.log"))
  compute <- if (!nzchar(outdir)) "" else read_log_for_progress(file.path(outdir, "logs", "compute.log"))
  pbs <- if (!nzchar(outdir)) "" else read_log_for_progress(file.path(outdir, "logs", "pbs_job.out"))
  txt <- paste(c(pipe, compute, pbs)[nzchar(c(pipe, compute, pbs))], collapse = "\n")
  failed <- grepl("SCAUTO_HPC_STAGE=failed|DID NOT FINISH|STOPPED BY USER", pipe)
  done <- grepl("SCAUTO_HPC_DONE| ANALYSIS FINISHED ", pipe) && !failed
  hpc <- identical(mode, "hpc") || grepl("SCAUTO_HPC_STAGE=", pipe)
  ana <- analysis_pct_from_text(if (hpc && nzchar(compute)) compute else txt)
  if (!hpc) {
    pct <- if (done) 100 else ana$pct
    state <- if (failed) "fail" else if (done) "done" else "live"
    return(list(
      pct = as.integer(pct), label = if (done) "Analysis finished" else ana$label,
      detail = "", state = state
    ))
  }
  stage <- "starting"
  hits <- gregexpr("SCAUTO_HPC_STAGE=([a-z_]+)", pipe, perl = TRUE)
  if (hits[[1]][1] > 0) {
    m <- regmatches(pipe, hits)[[1]]
    stage <- sub("SCAUTO_HPC_STAGE=", "", m[[length(m)]])
  }
  up_hits <- gregexpr("  upload ", pipe, fixed = TRUE)[[1]]
  n_up <- if (identical(as.integer(up_hits[[1]]), -1L)) 0L else length(up_hits)
  n_exp <- n_expected_hpc_samples(outdir)
  label <- hpc_stage_label(stage)
  detail <- ""
  pct <- switch(
    stage,
    ssh = 4,
    upload = 8,
    install_pkgs = 22,
    submit = 26,
    queued = 30,
    running = 35,
    pull = 92,
    done = 100,
    failed = 0,
    6
  )
  if (identical(stage, "upload") && is.finite(n_exp) && n_exp > 0) {
    frac <- max(0, min(1, n_up / n_exp))
    pct <- as.integer(8 + 14 * frac)
    label <- sprintf("Uploading samples (%s / %s)", n_up, n_exp)
  }
  if (identical(stage, "running")) {
    pct <- as.integer(35 + 0.55 * ana$pct)
    label <- ana$label
    detail <- "Compute node"
  }
  if (identical(stage, "queued")) {
    detail <- "Waiting for a free HPC node — this can take minutes"
  }
  if (isTRUE(done) || identical(stage, "done")) {
    pct <- 100
    label <- "Finished — results are local"
    stage <- "done"
  }
  if (isTRUE(failed) || identical(stage, "failed")) {
    state <- "fail"
    if (pct < 4) pct <- 4
    label <- if (nzchar(ana$label) && !identical(ana$label, "Starting analysis")) {
      paste0("Stopped during: ", ana$label)
    } else {
      hpc_stage_label("failed")
    }
  } else {
    state <- if (identical(stage, "done")) "done" else "live"
  }
  list(pct = as.integer(max(0, min(100, pct))), label = label, detail = detail, state = state, stage = stage)
}

progress_bar_ui <- function(prog) {
  if (is.null(prog) || !is.finite(prog$pct)) return(NULL)
  cls <- "prog-fill"
  if (identical(prog$state, "done")) cls <- paste(cls, "done")
  else if (identical(prog$state, "fail")) cls <- paste(cls, "fail")
  else cls <- paste(cls, "live")
  extra <- if (!is.null(prog$detail) && nzchar(prog$detail)) {
    span(class = "prog-detail", paste0(" · ", prog$detail))
  } else {
    NULL
  }
  div(
    class = "prog-wrap",
    p(class = "prog-label", prog$label),
    div(
      class = "prog-track",
      div(class = cls, style = sprintf("width:%s%%;", prog$pct),
          role = "progressbar", `aria-valuenow` = prog$pct, `aria-valuemin` = 0, `aria-valuemax` = 100)
    ),
    p(class = "prog-meta", sprintf("%s%%", prog$pct), extra)
  )
}

hpc_read_job_env <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  lines <- lines[grepl("^[A-Za-z_][A-Za-z0-9_]*=", lines)]
  out <- list()
  for (ln in lines) {
    nm <- sub("=.*$", "", ln)
    raw <- sub("^[^=]+=", "", ln)
    if (startsWith(raw, "'") && nchar(raw) >= 2L && endsWith(raw, "'")) {
      raw <- substring(raw, 2L, nchar(raw) - 1L)
      raw <- gsub("'\"'\"'", "'", raw, fixed = TRUE)
    }
    out[[nm]] <- raw
  }
  out
}

# Copy remote pipeline.log → logs/compute.log so the bar can show analysis steps.
# Skips if the local copy is already fresh (the watcher also rsyncs this file).
hpc_refresh_compute_log <- function(outdir, min_age_sec = 18) {
  if (!nzchar(outdir)) return(invisible(FALSE))
  dest <- file.path(outdir, "logs", "compute.log")
  if (file.exists(dest)) {
    age <- as.numeric(difftime(Sys.time(), file.info(dest)$mtime, units = "secs"))
    if (is.finite(age) && age < min_age_sec) return(invisible(FALSE))
  }
  kv <- hpc_read_job_env(file.path(outdir, "logs", "hpc_job.env"))
  ssh <- kv$HPC_SSH
  remote_out <- kv$REMOTE_OUT
  if (is.null(ssh) || is.null(remote_out) || !nzchar(ssh) || !nzchar(remote_out)) {
    return(invisible(FALSE))
  }
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  src <- paste0(ssh, ":", remote_out, "/logs/pipeline.log")
  rsync <- Sys.which("rsync")
  ssh_bin <- Sys.which("ssh")
  if (!nzchar(rsync) || !nzchar(ssh_bin)) return(invisible(FALSE))
  rsh <- paste(
    shQuote(ssh_bin),
    "-o BatchMode=yes -o ConnectTimeout=8 -o LogLevel=ERROR"
  )
  tryCatch(
    processx::process$new(
      rsync,
      c("-a", "--partial", "-e", rsh, src, dest),
      cleanup = TRUE
    ),
    error = function(e) NULL
  )
  invisible(TRUE)
}

hpc_run_is_resumable <- function(outdir) {
  if (!nzchar(outdir) || !dir.exists(outdir)) return(FALSE)
  envf <- file.path(outdir, "logs", "hpc_job.env")
  idf <- file.path(outdir, "logs", "hpc_job.id")
  if (!file.exists(envf) || !file.exists(idf)) return(FALSE)
  job <- read_hpc_job_id(outdir)
  if (!nzchar(job)) return(FALSE)
  if (isTRUE(hpc_pid_alive(read_hpc_watcher_pid(outdir)))) return(FALSE)
  stage <- hpc_stage_from_log(outdir)
  if (identical(stage, "done")) return(FALSE)
  lf <- file.path(outdir, "logs", "pipeline.log")
  if (file.exists(lf)) {
    tailn <- paste(tryCatch(read_tail_lines(lf, n = 40L, max_bytes = 16384L),
                            error = function(e) character()), collapse = "\n")
    if (grepl("SCAUTO_HPC_DONE", tailn, fixed = TRUE)) return(FALSE)
  }
  TRUE
}

hpc_resume_info <- function(project, extra = character()) {
  cands <- unique(c(
    extra,
    if (!is.null(project) && nzchar(project) && dir.exists(file.path(project, "results"))) {
      list.dirs(file.path(project, "results"), recursive = FALSE, full.names = TRUE)
    } else {
      character()
    }
  ))
  cands <- cands[nzchar(cands) & dir.exists(cands)]
  cands <- cands[vapply(cands, hpc_run_is_resumable, logical(1))]
  if (!length(cands)) return(NULL)
  mtimes <- vapply(cands, function(od) {
    inf <- file.info(file.path(od, "logs", "hpc_job.id"))
    as.numeric(inf$mtime)
  }, numeric(1))
  od <- cands[[which.max(mtimes)]]
  list(
    outdir = normalizePath(od, winslash = "/", mustWork = FALSE),
    job_id = read_hpc_job_id(od),
    stage = hpc_stage_from_log(od)
  )
}

start_hpc_one_click <- function(script, env_file, log_path, pid_path, resume = FALSE) {
  bash <- hpc_bash()
  if (!nzchar(bash)) stop("bash is required for HPC one-click (macOS/Linux, or Git Bash on Windows).", call. = FALSE)
  if (!file.exists(script)) stop("Missing HPC script: ", script, call. = FALSE)
  tryCatch(Sys.chmod(script, mode = "0755"), error = function(e) NULL)
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(pid_path), recursive = TRUE, showWarnings = FALSE)
  note <- if (isTRUE(resume)) {
    " Resume: waiting for the existing cluster job, then copying results. No new upload or qsub.\n"
  } else {
    " HPC one-click started. This window can stay open; results are copied back when the job finishes.\n"
  }
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), note, file = log_path, append = TRUE)
  extra <- if (isTRUE(resume)) "--resume" else NULL
  if (.Platform$OS.type == "windows") {
    px <- processx::process$new(
      bash, c(script, extra, env_file),
      stdout = log_path, stderr = "2>&1",
      cleanup = FALSE, cleanup_tree = FALSE
    )
    pid <- tryCatch(px$get_pid(), error = function(e) NA_integer_)
    if (is.finite(pid)) writeLines(as.character(pid), pid_path)
    return(list(pid = pid, process = px))
  }
  cmd <- paste(
    "nohup", shQuote(bash), shQuote(normalizePath(script)),
    if (isTRUE(resume)) "--resume" else "",
    shQuote(normalizePath(env_file)),
    ">>", shQuote(log_path), "2>&1 & echo $!"
  )
  pid <- suppressWarnings(as.integer(system(cmd, intern = TRUE)[[1]]))
  if (is.finite(pid) && pid > 1L) writeLines(as.character(pid), pid_path)
  list(pid = pid, process = NULL)
}

hpc_qdel <- function(user, host, job_id) {
  job_id <- trimws(as.character(job_id)[1])
  if (!nzchar(job_id)) return(invisible(FALSE))
  target <- tryCatch(hpc_ssh_target(user, host), error = function(e) "")
  if (!nzchar(target)) return(invisible(FALSE))
  hpc_run_ssh(
    target,
    sprintf("export PATH=/opt/pbs/bin:$PATH; qdel %s || true", shQuote(job_id, type = "sh")),
    timeout = 20
  )
  invisible(TRUE)
}

kill_hpc_watcher <- function(outdir) {
  pid <- read_hpc_watcher_pid(outdir)
  if (isTRUE(hpc_pid_alive(pid))) {
    if (.Platform$OS.type == "windows") {
      tryCatch(tools::pskill(pid, tools::SIGTERM), error = function(e) NULL)
    } else {
      tryCatch(system2("kill", c("-TERM", as.character(pid)), stdout = FALSE, stderr = FALSE),
               error = function(e) NULL)
    }
  }
  invisible(pid)
}

pipeline_failed_in_log <- function(outdir) {
  lf <- file.path(outdir, "logs", "pipeline.log")
  if (!nzchar(outdir) || !file.exists(lf)) return(FALSE)
  lines <- tryCatch(read_tail_lines(lf, n = 40L, max_bytes = 16384L),
                    error = function(e) character())
  if (!length(lines)) return(FALSE)
  tailn <- paste(lines, collapse = "\n")
  grepl("DID NOT FINISH", tailn, fixed = TRUE) ||
    grepl("SCAUTO_HPC_STAGE=failed", tailn, fixed = TRUE)
}

load_group_test_pair <- function(scauto_root = NULL) {
  if (is.null(scauto_root)) scauto_root <- ui_scauto_root()
  p <- file.path(scauto_root, "examples", "sample_meta.groups_test.csv")
  if (file.exists(p)) {
    return(utils::read.csv(p, stringsAsFactors = FALSE))
  }
  data.frame(
    sample = c("sampleA", "sampleB", "sampleC", "sampleD"),
    group = c("GroupB", "GroupB", "GroupA", "GroupA"),
    Timepoint = "Baseline",
    n_cells = c(4000, 4000, 4000, 4000),
    stringsAsFactors = FALSE
  )
}

trial_set_phrase <- function(pair = NULL, scauto_root = NULL) {
  if (is.null(pair)) pair <- load_group_test_pair(scauto_root)
  ns <- as.character(pair$sample[pair$group == "GroupB"])
  sl <- as.character(pair$sample[pair$group == "GroupA"])
  if (!length(ns)) ns <- as.character(pair$sample)
  if (!length(sl)) sl <- character()
  paste0(
    paste(ns, collapse = ", "), " (GroupB)",
    if (length(sl)) paste0(" + ", paste(sl, collapse = ", "), " (GroupA)") else ""
  )
}

trial_set_hint <- function(pair = NULL, scauto_root = NULL) {
  if (is.null(pair)) pair <- load_group_test_pair(scauto_root)
  bits <- trial_set_phrase(pair)
  tp <- unique(as.character(pair$Timepoint))
  tp <- tp[!is.na(tp) & nzchar(tp)]
  if (length(tp) == 1L) bits <- paste0(bits, "; ", tp)
  n <- if ("n_cells" %in% names(pair)) sum(as.numeric(pair$n_cells), na.rm = TRUE) else NA_real_
  if (is.finite(n) && n > 0) {
    bits <- paste0(bits, "; about ", format(round(n), big.mark = ","), " cells")
  }
  bits
}

find_trial_pair_parent <- function(pair_samples, project, extra = character()) {
  pair_samples <- as.character(pair_samples)
  cands <- unique(c(
    extra,
    if (!is.null(project) && nzchar(project)) file.path(project, "data", "trial_4samples"),
    if (!is.null(project) && nzchar(project)) file.path(project, "data", "mtx_files_per_sample"),
    if (!is.null(project) && nzchar(project)) file.path(project, "mtx_files_per_sample"),
    project
  ))
  cands <- cands[!is.na(cands) & nzchar(cands)]
  is_10x_sample <- function(p) {
    dir.exists(p) && any(grepl("^matrix\\.mtx", list.files(p), ignore.case = TRUE))
  }
  for (root in cands) {
    if (!dir.exists(root)) next
    if (all(vapply(pair_samples, function(s) is_10x_sample(file.path(root, s)), logical(1)))) {
      return(normalizePath(root))
    }
  }
  NULL
}

js_set_input <- function(id, value) {
  payload <- jsonlite::toJSON(value, auto_unbox = TRUE)
  sprintf(
    "Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;",
    id, payload
  )
}

clickable_list <- function(values, input_id, selected = NULL, labels = NULL, meta = NULL,
                           class = "file-list") {
  if (!length(values)) {
    return(p(class = "hint", "Nothing to show yet."))
  }
  if (is.null(labels)) labels <- values
  tags$ul(
    class = class,
    lapply(seq_along(values), function(i) {
      active <- !is.null(selected) && identical(as.character(values[[i]]), as.character(selected))
      extra <- if (!is.null(meta) && length(meta) >= i && nzchar(meta[[i]])) {
        tags$span(class = "meta", meta[[i]])
      } else {
        NULL
      }
      tags$li(
        tags$a(
          href = "#",
          class = if (active) "active" else NULL,
          onclick = js_set_input(input_id, values[[i]]),
          labels[[i]], extra
        )
      )
    })
  )
}

guess_content_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    pdf = "application/pdf",
    csv = "text/csv",
    tsv = "text/tab-separated-values",
    txt = "text/plain",
    log = "text/plain",
    md = "text/markdown",
    json = "application/json",
    mtx = "text/plain",
    rds = "application/octet-stream",
    zip = "application/zip",
    "application/octet-stream"
  )
}

preview_table_head <- function(path, n = 40L) {
  ext <- tolower(tools::file_ext(path))
  df <- tryCatch({
    if (ext %in% c("tsv", "txt") || grepl("barcodes|features|genes", basename(path), ignore.case = TRUE)) {
      utils::read.delim(path, header = grepl("\\.csv$", path, ignore.case = TRUE),
                        nrows = n, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      utils::read.csv(path, nrows = n, stringsAsFactors = FALSE, check.names = FALSE)
    }
  }, error = function(e) NULL)
  df
}

env_check_mark <- function(status) {
  switch(
    as.character(status),
    ok = "OK",
    missing = "NO",
    fail = "NO",
    warn = "!",
    installing = "…",
    checking = "…",
    pending = "·",
    "·"
  )
}

env_check_ui <- function(rows) {
  if (is.null(rows) || !is.data.frame(rows) || !nrow(rows)) {
    return(p(class = "hint", "The environment check has not started yet."))
  }
  groups <- unique(as.character(rows$group))
  blocks <- lapply(groups, function(g) {
    sub <- rows[rows$group == g, , drop = FALSE]
    items <- lapply(seq_len(nrow(sub)), function(i) {
      st <- as.character(sub$status[[i]])
      tags$li(
        class = paste("check-item", paste0("check-", st)),
        span(class = "check-mark", env_check_mark(st)),
        span(class = "check-label", sub$label[[i]]),
        span(class = "check-detail", sub$detail[[i]])
      )
    })
    tagList(
      div(class = "check-group", g),
      tags$ul(class = "check-list", items)
    )
  })
  n_ok <- sum(rows$status %in% "ok")
  n_bad <- sum(rows$status %in% c("missing", "fail"))
  n_warn <- sum(rows$status %in% "warn")
  n_work <- sum(rows$status %in% c("checking", "installing", "pending"))
  summary <- if (n_work > 0L) {
    sprintf("Checking %s / %s items…", n_ok + n_bad + n_warn, nrow(rows))
  } else if (n_bad > 0L) {
    sprintf("%s OK · %s missing · %s warning", n_ok, n_bad, n_warn)
  } else if (n_warn > 0L) {
    sprintf("%s OK · %s warning (analysis can still run)", n_ok, n_warn)
  } else {
    sprintf("All %s items OK", n_ok)
  }
  tagList(
    p(class = "hint", summary),
    blocks
  )
}

zip_folder <- function(folder, zipfile) {
  folder <- normalizePath(folder)
  zipfile <- normalizePath(zipfile, mustWork = FALSE)
  if (file.exists(zipfile)) unlink(zipfile)
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(dirname(folder))
  ok <- tryCatch({
    utils::zip(zipfile, basename(folder), flags = "-r9Xq")
    file.exists(zipfile)
  }, error = function(e) FALSE)
  if (!ok && nzchar(Sys.which("ditto"))) {
    system2("ditto", c("-c", "-k", "--keepParent", folder, zipfile), stdout = FALSE, stderr = FALSE)
  }
  if (!file.exists(zipfile)) stop("Could not create zip. Check that the zip command is installed.")
  zipfile
}
