#!/usr/bin/env Rscript
# Optional diagnostic. You do not need this before analysis:
# click Start analysis in the helper — it installs missing packages.
#
#   Rscript tools/scauto/check_install.R
#   Rscript tools/scauto/check_install.R --install   # install missing CRAN packages now
#
# Safe to run on macOS, Windows, and Linux. Does not start the analysis.

args <- commandArgs(trailingOnly = TRUE)
do_install <- any(args %in% c("--install", "--install-missing"))

find_scauto_root <- function() {
  args_full <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_full, value = TRUE)
  if (length(file_arg)) {
    return(normalizePath(dirname(sub("^--file=", "", file_arg[[1]]))))
  }
  if (file.exists("tools/scauto/check_install.R")) {
    return(normalizePath("tools/scauto"))
  }
  normalizePath(getwd())
}

scauto_root <- find_scauto_root()
source(file.path(scauto_root, "R", "00_packages.R"), local = TRUE)
ui_helpers <- file.path(scauto_root, "ui", "helpers.R")
if (file.exists(ui_helpers)) {
  sys.source(ui_helpers, envir = environment())
}

sets <- scauto_package_sets()
need_gui <- sets$gui
need_pipe <- sets$pipeline
need_rec <- sets$recommended
all_need <- unique(c(need_gui, need_pipe, need_rec))

project_cli <- ""
if (any(grepl("^--project=", args))) {
  project_cli <- sub("^--project=", "", args[grepl("^--project=", args)][[1]])
} else if ("--project" %in% args) {
  project_cli <- args[match("--project", args) + 1L]
}

if (isTRUE(do_install)) {
  ensure_scauto_packages(
    which_hard = c("gui", "pipeline"),
    which_soft = c("recommended"),
    install = TRUE,
    project = if (nzchar(project_cli)) project_cli else NULL
  )
  if (exists("scauto_try_bioc", mode = "function")) {
    scauto_try_bioc(c(
      "clusterProfiler", "org.Hs.eg.db", "ReactomePA", "GSVA", "fgsea", "AnnotationDbi"
    ))
  }
}

os <- Sys.info()[["sysname"]]
ram <- if (exists("host_ram_gb", mode = "function")) host_ram_gb() else NA_real_
rscript <- Sys.which("Rscript")
zip_bin <- Sys.which("zip")
project <- normalizePath(file.path(scauto_root, "..", ".."))
has_viz <- dir.exists(file.path(project, "viz"))
has_launch <- file.exists(file.path(scauto_root, "ui", "launch.R"))

line <- function(ok, text) {
  mark <- if (isTRUE(ok)) "[OK]  " else "[--]  "
  cat(mark, text, "\n", sep = "")
}

cat("\n===== PBMC Auto-Pilot install check =====\n")
cat("OS:          ", os, "\n", sep = "")
cat("R:           ", paste(R.version$major, R.version$minor, sep = "."), "\n", sep = "")
cat("Rscript:     ", if (nzchar(rscript)) rscript else "(not on PATH)", "\n", sep = "")
cat("Project:     ", project, "\n", sep = "")
if (is.finite(ram)) {
  cat("RAM:         ", format(ram, nsmall = 1), " GB\n", sep = "")
} else {
  cat("RAM:         (could not read)\n")
}
cat("\n")

r_ver <- getRversion()
line(r_ver >= "4.3.0", sprintf("R >= 4.3 (found %s)", as.character(r_ver)))
line(nzchar(rscript), "Rscript is on PATH (or use Launch.command / Launch.bat)")
line(has_viz, "Project copy includes viz/ (do not copy app.R alone)")
line(has_launch, "Helper launcher is present (tools/scauto/ui/launch.R)")
line(nzchar(zip_bin) || identical(os, "Darwin"),
     "zip on PATH (needed for Download all results; macOS can use ditto if zip is missing)")

cat("\nRequired for the helper window:\n")
for (p in need_gui) {
  line(isTRUE(scauto_pkg_present(p)[[p]]), p)
}
cat("Required for Start analysis (installed automatically when you click the button):\n")
for (p in need_pipe) {
  line(isTRUE(scauto_pkg_present(p)[[p]]), p)
}
cat("Recommended:\n")
for (p in need_rec) {
  line(
    isTRUE(scauto_pkg_present(p)[[p]]),
    paste(p, if (identical(p, "harmony")) {
      "(multi-sample batch correction)"
    } else if (identical(p, "msigdbr")) {
      "(pathway gene sets)"
    } else if (identical(p, "MAST")) {
      "(clinical DEGs)"
    } else {
      "(optional)"
    })
  )
}

cat("\nHow to start the helper:\n")
if (identical(os, "Windows")) {
  cat("  Double-click  tools\\scauto\\ui\\Launch.bat\n")
  cat("  or:           Rscript tools\\scauto\\ui\\launch.R\n")
} else if (identical(os, "Darwin")) {
  cat("  Double-click  tools/scauto/ui/Launch.command\n")
  cat("  or:           Rscript tools/scauto/ui/launch.R\n")
} else {
  cat("  Rscript tools/scauto/ui/launch.R\n")
}
cat("  Browser:      http://127.0.0.1:8765  (Chrome, Edge, Firefox, or Safari — not Internet Explorer)\n")

cat("\nLaptop size:\n")
cat("  Trial 4 Baseline samples (~24,000 cells) ≈ 13 GB RAM.\n")
cat("  About 30,000 cells ≈ 16 GB. Above ~50,000 cells use HPC, not a laptop.\n")
if (is.finite(ram) && ram < 16) {
  cat("  This machine is under 16 GB — the 4-sample trial is tight; close other programs or use HPC.\n")
}

missing <- names(which(!scauto_pkg_present(all_need)))
n_fail <- sum(!vapply(unique(c(need_gui, need_pipe)), function(p) {
  isTRUE(scauto_pkg_present(p)[[p]])
}, logical(1)))
if (!has_viz) n_fail <- n_fail + 1L
if (r_ver < "4.3.0") n_fail <- n_fail + 1L

cat("\n")
if (n_fail == 0L && !length(missing)) {
  cat("Ready. Start the helper, then choose a 10x folder.\n")
} else if (length(setdiff(missing, need_pipe)) && length(intersect(missing, need_gui))) {
  cat("Missing helper packages: ", paste(intersect(missing, need_gui), collapse = ", "), "\n", sep = "")
  cat("Open Launch.command / Launch.bat while online — it installs them.\n")
} else if (length(intersect(missing, need_pipe))) {
  cat("Missing analysis packages: ", paste(intersect(missing, need_pipe), collapse = ", "), "\n", sep = "")
  cat("You do not need this script. Click Start analysis in the helper (needs internet).\n")
} else {
  cat("See tools/scauto/INSTALL.md for the remaining [--] items.\n")
}
cat("========================================\n")
if (n_fail > 0L && !isTRUE(do_install)) quit(status = 1L)
if (isTRUE(do_install) && length(names(which(!scauto_pkg_present(unique(c(need_gui, need_pipe))))))) {
  quit(status = 1L)
}
quit(status = 0L)
