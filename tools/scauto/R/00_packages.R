# Package lists + item-by-item environment check (always runs every item).
# Start analysis writes logs/env_check.tsv so the helper page can show each step.

scauto_package_sets <- function() {
  list(
    gui = c("shiny", "bslib", "shinyjs", "processx", "jsonlite"),
    pipeline = c("Seurat", "SeuratObject", "ggplot2", "Matrix", "jsonlite", "yaml"),
    recommended = c("harmony", "future", "MAST", "msigdbr")
  )
}

scauto_pkg_present <- function(pkgs) {
  pkgs <- unique(as.character(pkgs))
  pkgs <- pkgs[nzchar(pkgs)]
  if (!length(pkgs)) {
    return(stats::setNames(logical(0), character(0)))
  }
  vapply(pkgs, function(p) requireNamespace(p, quietly = TRUE), logical(1), USE.NAMES = TRUE)
}

scauto_pkg_version <- function(pkg) {
  tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NA_character_)
}

scauto_pkg_note <- function(...) {
  if (exists("log_msg", mode = "function")) {
    log_msg(...)
  } else {
    cat(paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ..., sep = " "), "\n", sep = "")
    flush.console()
  }
}

# Prefer a directory that actually contains viz/palette.R. HPC login shells often
# still have LOCAL_PROJECT_DIR set to a Mac path from hpc/config.env; that path
# does not exist on the cluster.
scauto_resolve_project <- function(project = "") {
  if (is.null(project) || length(project) < 1L || is.na(project[[1]])) {
    project <- ""
  } else {
    project <- as.character(project)[1]
  }
  cands <- unique(c(
    project,
    Sys.getenv("PROJECT_DIR", ""),
    getOption("scauto.project", ""),
    Sys.getenv("LOCAL_PROJECT_DIR", "")
  ))
  wd <- tryCatch(normalizePath(".", winslash = "/", mustWork = FALSE), error = function(e) "")
  if (nzchar(wd)) {
    cands <- c(cands, wd, file.path(wd, ".."), file.path(wd, "..", ".."))
  }
  root_opt <- getOption("scauto.scauto_root", "")
  if (nzchar(root_opt)) {
    cands <- c(cands, file.path(root_opt, "..", ".."))
  }
  has_viz <- function(p) {
    if (is.null(p) || length(p) < 1L || is.na(p[[1]]) || !nzchar(p[[1]])) return(FALSE)
    file.exists(file.path(p, "viz", "palette.R"))
  }
  for (p in cands) {
    p <- as.character(p)[1]
    if (is.na(p) || !nzchar(p) || !dir.exists(p)) next
    np <- tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) p)
    if (has_viz(np)) return(np)
  }
  for (p in cands) {
    p <- as.character(p)[1]
    if (is.na(p) || !nzchar(p) || !dir.exists(p)) next
    return(tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) p))
  }
  wd
}

scauto_host_ram_gb <- function() {
  if (exists("host_ram_gb", mode = "function")) {
    return(host_ram_gb())
  }
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
  NA_real_
}

scauto_env_catalog <- function() {
  sets <- scauto_package_sets()
  env <- data.frame(
    id = c("os", "r_version", "rscript", "ram", "zip", "viz_palette", "viz_markers", "scauto_run"),
    group = c(
      "Computer", "Computer", "Computer", "Computer", "Computer",
      "Project files", "Project files", "Project files"
    ),
    label = c(
      "Operating system",
      "R 4.3 or newer",
      "Rscript",
      "Memory (RAM)",
      "zip (for Download all results)",
      "viz/palette.R",
      "PBMC marker list (JSON)",
      "Analysis script (run_scauto.R)"
    ),
    role = "env",
    pkg = NA_character_,
    stringsAsFactors = FALSE
  )
  pkg_block <- function(pkgs, group, role) {
    pkgs <- unique(as.character(pkgs))
    data.frame(
      id = paste0("pkg_", pkgs),
      group = group,
      label = pkgs,
      role = role,
      pkg = pkgs,
      stringsAsFactors = FALSE
    )
  }
  gui_only <- setdiff(sets$gui, sets$pipeline)
  rec_only <- setdiff(sets$recommended, unique(c(sets$gui, sets$pipeline)))
  rbind(
    env,
    pkg_block(gui_only, "Helper window packages", "gui"),
    pkg_block(sets$pipeline, "Analysis packages", "pipeline"),
    pkg_block(rec_only, "Recommended packages", "recommended")
  )
}

scauto_empty_env_rows <- function() {
  catg <- scauto_env_catalog()
  data.frame(
    id = catg$id,
    group = catg$group,
    label = catg$label,
    role = catg$role,
    pkg = catg$pkg,
    status = "pending",
    detail = "waiting",
    stringsAsFactors = FALSE
  )
}

scauto_eval_env_item <- function(id, project = "") {
  sys <- Sys.info()[["sysname"]]
  if (identical(id, "os")) {
    return(list(status = "ok", detail = paste(sys, Sys.info()[["release"]])))
  }
  if (identical(id, "r_version")) {
    v <- getRversion()
    if (v >= "4.3.0") {
      return(list(status = "ok", detail = paste("found", as.character(v))))
    }
    return(list(status = "fail", detail = paste("found", as.character(v), "(need 4.3 or newer)")))
  }
  if (identical(id, "rscript")) {
    rs <- getOption("scauto.rscript", Sys.which("Rscript"))
    if (is.null(rs) || !nzchar(rs)) rs <- Sys.which("Rscript")
    if (nzchar(rs)) return(list(status = "ok", detail = as.character(rs)))
    return(list(status = "fail", detail = "Rscript was not found on PATH"))
  }
  if (identical(id, "ram")) {
    gb <- scauto_host_ram_gb()
    if (!is.finite(gb)) {
      return(list(status = "warn", detail = "could not read RAM"))
    }
    if (gb < 8) {
      return(list(status = "warn", detail = sprintf("%.1f GB (tight for this pipeline)", gb)))
    }
    return(list(status = "ok", detail = sprintf("%.1f GB", gb)))
  }
  if (identical(id, "zip")) {
    z <- Sys.which("zip")
    if (nzchar(z)) return(list(status = "ok", detail = z))
    if (identical(sys, "Darwin")) {
      return(list(status = "ok", detail = "zip missing; macOS can use ditto"))
    }
    return(list(status = "warn", detail = "zip not on PATH (Download all results may fail)"))
  }
  if (identical(id, "viz_palette")) {
    p <- file.path(project, "viz", "palette.R")
    if (nzchar(project) && file.exists(p)) {
      return(list(status = "ok", detail = p))
    }
    return(list(status = "fail", detail = "missing viz/palette.R (copy the whole project)"))
  }
  if (identical(id, "viz_markers")) {
    p <- file.path(project, "viz", "pbmc_lineage_markers.json")
    if (nzchar(project) && file.exists(p)) {
      return(list(status = "ok", detail = p))
    }
    return(list(status = "fail", detail = "missing viz/pbmc_lineage_markers.json"))
  }
  if (identical(id, "scauto_run")) {
    cands <- c(
      if (nzchar(getOption("scauto.scauto_root", ""))) file.path(getOption("scauto.scauto_root"), "run_scauto.R"),
      if (nzchar(project)) file.path(project, "tools", "scauto", "run_scauto.R"),
      "tools/scauto/run_scauto.R"
    )
    cands <- cands[nzchar(cands)]
    hit <- cands[file.exists(cands)]
    if (length(hit)) {
      return(list(status = "ok", detail = normalizePath(hit[[1]], mustWork = FALSE)))
    }
    return(list(status = "fail", detail = "run_scauto.R not found"))
  }
  if (startsWith(id, "pkg_")) {
    pkg <- sub("^pkg_", "", id)
    if (isTRUE(requireNamespace(pkg, quietly = TRUE))) {
      ver <- scauto_pkg_version(pkg)
      detail <- if (is.na(ver)) "installed" else paste("installed", ver)
      return(list(status = "ok", detail = detail))
    }
    return(list(status = "missing", detail = "not installed"))
  }
  list(status = "warn", detail = paste("unknown item", id))
}

scauto_write_env_check <- function(path, rows) {
  if (is.null(path) || !nzchar(path)) return(invisible(FALSE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  keep <- c("id", "group", "label", "role", "pkg", "status", "detail")
  keep <- intersect(keep, names(rows))
  out <- rows[, keep, drop = FALSE]
  for (nm in names(out)) {
    out[[nm]] <- gsub("[\t\r\n]", " ", as.character(out[[nm]]))
  }
  tmp <- paste0(path, ".tmp")
  utils::write.table(
    out, tmp,
    sep = "\t", row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8"
  )
  if (file.exists(path)) unlink(path)
  ok <- file.rename(tmp, path)
  if (!isTRUE(ok) && file.exists(tmp)) {
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
  invisible(TRUE)
}

scauto_read_env_check <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  info <- file.info(path)
  if (is.na(info$size) || info$size < 8) return(NULL)
  rows <- tryCatch(
    utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE, quote = "\""),
    error = function(e) NULL
  )
  if (is.null(rows) || !nrow(rows) || !"status" %in% names(rows)) return(NULL)
  rows
}

scauto_env_line <- function(row) {
  mark <- switch(
    as.character(row$status),
    ok = "[OK]",
    missing = "[NO]",
    fail = "[NO]",
    warn = "[!!]",
    installing = "[>>]",
    checking = "[..]",
    pending = "[  ]",
    "[--]"
  )
  paste(mark, row$label, "—", row$detail)
}

# Evaluate every catalog item (never skip because something is already OK).
scauto_run_env_check <- function(
    project = "",
    check_path = NULL,
    log = TRUE,
    incremental = TRUE,
    banner = "Checking every required component"
) {
  rows <- scauto_empty_env_rows()
  if (isTRUE(log)) {
    scauto_pkg_note("========== ENVIRONMENT CHECK ==========")
    scauto_pkg_note(banner)
  }
  scauto_write_env_check(check_path, rows)
  for (i in seq_len(nrow(rows))) {
    rows$status[[i]] <- "checking"
    rows$detail[[i]] <- "checking now"
    if (isTRUE(incremental)) scauto_write_env_check(check_path, rows)
    if (isTRUE(log) && isTRUE(incremental)) {
      scauto_pkg_note("[..]", rows$label[[i]])
    }
    got <- scauto_eval_env_item(rows$id[[i]], project = project)
    rows$status[[i]] <- got$status
    rows$detail[[i]] <- got$detail
    if (isTRUE(incremental)) scauto_write_env_check(check_path, rows)
    if (isTRUE(log)) scauto_pkg_note(scauto_env_line(rows[i, , drop = FALSE]))
    if (isTRUE(incremental) && !is.null(check_path) && nzchar(check_path)) {
      Sys.sleep(0.12)
    }
  }
  if (!isTRUE(incremental)) scauto_write_env_check(check_path, rows)
  if (isTRUE(log)) scauto_pkg_note("Environment check finished")
  rows
}

scauto_install_ncpus <- function() {
  ncpu <- tryCatch({
    as.integer(max(1L, min(4L, parallel::detectCores(logical = TRUE) - 1L)))
  }, error = function(e) 1L)
  if (!is.finite(ncpu) || ncpu < 1L) 1L else ncpu
}

# Optional Bioconductor packages (pathway / GSVA). Never abort the pipeline.
scauto_try_bioc <- function(pkgs) {
  pkgs <- unique(as.character(pkgs))
  pkgs <- pkgs[nzchar(pkgs)]
  pkgs <- pkgs[!scauto_pkg_present(pkgs)]
  if (!length(pkgs)) return(character())
  if (identical(Sys.getenv("SCAUTO_NO_PKG_INSTALL"), "1")) {
    scauto_pkg_note("Skip Bioconductor install (no internet):", paste(pkgs, collapse = ", "))
    return(character())
  }
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    tryCatch(
      utils::install.packages("BiocManager", repos = "https://cloud.r-project.org"),
      error = function(e) scauto_pkg_note("BiocManager install failed:", conditionMessage(e))
    )
  }
  if (!requireNamespace("BiocManager", quietly = TRUE)) return(character())
  ok <- character()
  for (p in pkgs) {
    scauto_pkg_note("Installing Bioconductor package", p)
    tryCatch({
      BiocManager::install(p, update = FALSE, ask = FALSE, quiet = TRUE)
      if (isTRUE(requireNamespace(p, quietly = TRUE))) ok <- c(ok, p)
    }, error = function(e) scauto_pkg_note("Bioconductor install failed", p, conditionMessage(e)))
  }
  ok
}

# Always check every component, then install any missing CRAN packages one by one.
# Hard roles abort if they stay missing. Soft roles (harmony, future) warn and continue.
ensure_scauto_packages <- function(
    which_hard = c("pipeline"),
    which_soft = c("recommended"),
    install = TRUE,
    repos = "https://cloud.r-project.org",
    check_path = NULL,
    project = NULL,
    incremental = TRUE
) {
  if (is.null(project) || !nzchar(project) || !file.exists(file.path(project, "viz", "palette.R"))) {
    project <- scauto_resolve_project(project)
  }
  rows <- scauto_run_env_check(
    project = project,
    check_path = check_path,
    log = TRUE,
    incremental = incremental,
    banner = "Checking every required component (even if it was OK last time)"
  )

  allow_install <- isTRUE(install) && !identical(Sys.getenv("SCAUTO_NO_PKG_INSTALL"), "1")
  sets <- scauto_package_sets()
  hard_pkgs <- unique(as.character(unlist(sets[which_hard], use.names = FALSE)))
  soft_pkgs <- unique(as.character(unlist(sets[which_soft], use.names = FALSE)))
  hard_pkgs <- hard_pkgs[nzchar(hard_pkgs)]
  soft_pkgs <- setdiff(soft_pkgs[nzchar(soft_pkgs)], hard_pkgs)
  want_pkgs <- unique(c(hard_pkgs, soft_pkgs))
  pkg_idx <- which(!is.na(rows$pkg) & nzchar(rows$pkg) & rows$pkg %in% want_pkgs)
  missing_idx <- pkg_idx[rows$status[pkg_idx] %in% c("missing", "fail")]
  installed <- character()

  if (length(missing_idx) && allow_install) {
    scauto_pkg_note(
      "Installing missing packages from CRAN (needs internet):",
      paste(rows$pkg[missing_idx], collapse = ", ")
    )
    ncpu <- scauto_install_ncpus()
    for (i in missing_idx) {
      pkg <- rows$pkg[[i]]
      rows$status[[i]] <- "installing"
      rows$detail[[i]] <- "downloading from CRAN (can take several minutes)"
      scauto_write_env_check(check_path, rows)
      scauto_pkg_note("[>>] Installing", pkg, "…")
      ok <- tryCatch({
        utils::install.packages(pkg, repos = repos, quiet = FALSE, Ncpus = ncpu)
        TRUE
      }, error = function(e) {
        scauto_pkg_note("install.packages failed for", pkg, ":", conditionMessage(e))
        FALSE
      })
      got <- scauto_eval_env_item(rows$id[[i]], project = project)
      rows$status[[i]] <- got$status
      rows$detail[[i]] <- if (isTRUE(ok) && identical(got$status, "ok")) {
        paste("installed now —", got$detail)
      } else {
        got$detail
      }
      scauto_write_env_check(check_path, rows)
      scauto_pkg_note(scauto_env_line(rows[i, , drop = FALSE]))
      if (identical(rows$status[[i]], "ok")) installed <- c(installed, pkg)
    }
    scauto_pkg_note("Re-checking every component after install")
    rows <- scauto_run_env_check(
      project = project,
      check_path = check_path,
      log = TRUE,
      incremental = incremental,
      banner = "Re-check of every required component"
    )
  } else if (length(missing_idx) && !allow_install) {
    scauto_pkg_note("Skipping package install (SCAUTO_NO_PKG_INSTALL=1 or install=FALSE)")
  } else {
    scauto_pkg_note("No missing packages to install")
  }

  missing_hard <- hard_pkgs[!scauto_pkg_present(hard_pkgs)]
  missing_soft <- setdiff(soft_pkgs[!scauto_pkg_present(soft_pkgs)], missing_hard)
  env_fail <- rows$id[rows$role == "env" & rows$status %in% c("fail")]

  if (length(missing_hard)) {
    stop(
      "Required R packages are missing: ", paste(missing_hard, collapse = ", "),
      "\nClick Start analysis while this computer is online — it installs them automatically.",
      "\nWindows users who see compile errors should also install Rtools:",
      "\n  https://cran.r-project.org/bin/windows/Rtools/",
      call. = FALSE
    )
  }
  if (length(env_fail) && any(env_fail %in% c("r_version", "viz_palette", "viz_markers", "scauto_run"))) {
    bad <- rows[rows$id %in% env_fail, , drop = FALSE]
    stop(
      "Required environment items failed:\n  ",
      paste(paste(bad$label, bad$detail, sep = " — "), collapse = "\n  "),
      call. = FALSE
    )
  }
  if (length(missing_soft)) {
    scauto_pkg_note(
      "Continuing without:", paste(missing_soft, collapse = ", "),
      "(harmony skip = no sample batch correction)"
    )
  } else {
    scauto_pkg_note("R packages: ready")
  }
  invisible(list(
    rows = rows,
    installed = installed,
    missing_hard = missing_hard,
    missing_soft = missing_soft
  ))
}
