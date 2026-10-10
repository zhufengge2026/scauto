# CASSIA subcluster annotation for lineage-internal clusters.
# Uses the `cassia` Python CLI when an agent/API backend is available.
# HPC compute nodes typically have no internet / no agent CLI; then panels stay.

cassia_cfg_value <- function(cfg, key, default) {
  if (is.null(cfg$cassia) || is.null(cfg$cassia[[key]])) return(default)
  cfg$cassia[[key]]
}

cassia_enable_mode <- function(cfg) {
  v <- cassia_cfg_value(cfg, "enable", "auto")
  v <- tolower(trimws(as.character(v)[1]))
  if (v %in% c("yes", "true", "on")) return("yes")
  if (v %in% c("no", "false", "off")) return("no")
  "auto"
}

find_cassia_bin <- function() {
  cands <- unique(c(
    Sys.which("cassia"),
    file.path(path.expand("~"), "Library/Python/3.10/bin/cassia"),
    file.path(path.expand("~"), "Library/Python/3.11/bin/cassia"),
    file.path(path.expand("~"), "Library/Python/3.12/bin/cassia"),
    file.path(path.expand("~"), ".local/bin/cassia")
  ))
  cands <- cands[nzchar(cands) & file.exists(cands)]
  if (!length(cands)) return("")
  normalizePath(cands[[1]], winslash = "/", mustWork = FALSE)
}

cassia_path_env <- function(bin) {
  extra <- unique(c(
    if (nzchar(bin)) dirname(bin) else "",
    dirname(Sys.which("claude")),
    dirname(Sys.which("cursor-agent")),
    dirname(Sys.which("codex")),
    file.path(path.expand("~"), "Library/Python/3.10/bin"),
    file.path(path.expand("~"), ".local/bin")
  ))
  extra <- extra[nzchar(extra) & dir.exists(extra)]
  paste(c(extra, Sys.getenv("PATH")), collapse = .Platform$path.sep)
}

cassia_system <- function(bin, args, timeout = 1200) {
  old <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = old), add = TRUE)
  Sys.setenv(PATH = cassia_path_env(bin))
  out <- tryCatch(
    system2(bin, args, stdout = TRUE, stderr = TRUE, timeout = as.integer(timeout) + 30L),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )
    list(text = as.character(out), status = as.integer(if (is.null(attr(out, "status"))) 0L else attr(out, "status")))
}

cassia_parse_backends <- function(bin) {
  res <- cassia_system(bin, c("backends", "list"), timeout = 60)
  lines <- res$text
  avail <- character()
  if (!length(lines)) return(avail)
  for (ln in lines) {
    if (!grepl("\\syes\\s", ln, ignore.case = TRUE) && !grepl("\\syes$", ln, ignore.case = TRUE)) next
    nm <- trimws(strsplit(ln, "\\s+")[[1]][[1]])
    if (nzchar(nm) && !identical(nm, "Backend") && !grepl("^-+$", nm)) {
      avail <- c(avail, nm)
    }
  }
  unique(avail)
}

cassia_pick_backend <- function(cfg, bin) {
  want <- trimws(as.character(cassia_cfg_value(cfg, "backend", "auto"))[1])
  avail <- cassia_parse_backends(bin)
  avail <- setdiff(avail, "shell")
  if (nzchar(want) && !identical(tolower(want), "auto")) {
    if (want %in% avail) return(want)
    log_msg("CASSIA requested backend", want, "not available; have:", paste(avail, collapse = ", "))
    return("")
  }
  order <- c("claude-cli", "cursor-agent", "codex-cli", "opencode", "openai", "anthropic", "openrouter")
  hit <- intersect(order, avail)
  if (length(hit)) return(hit[[1]])
  ""
}

load_cassia_lineage_specs <- function(scauto_root) {
  p <- file.path(scauto_root, "config", "cassia_lineage.json")
  if (!file.exists(p)) return(NULL)
  jsonlite::fromJSON(p, simplifyVector = TRUE)
}

cassia_major_info <- function(super, specs) {
  if (!is.null(specs) && !is.null(specs$lineages[[super]]$major)) {
    return(as.character(specs$lineages[[super]]$major)[1])
  }
  paste(gsub("_", " ", super), "from human PBMC (Harmony-integrated lineage reclustering)")
}

cassia_additional_context <- function(sub, super) {
  paste(
    "Harmony-integrated lineage-restricted object.",
    "Call out contamination or a distinct lineage when marker evidence is strong",
    "(T/B/myeloid doublets, platelet, HSPC, RBC).",
    "CD3E/LYZ/HBA can be ambient in 10x PBMC and are not sufficient alone.",
    "Distinguish naive/TCM/TEM/TEMRA, MAIT, gdT, Treg, IFN, cycling,",
    "CD14 vs CD16, cDC/pDC, naive/memory/plasma B when the markers support it.",
    "Parent superlineage:", super, ".",
    "n_cells=", ncol(sub), "."
  )
}

cassia_should_run <- function(cfg) {
  mode <- cassia_enable_mode(cfg)
  if (identical(mode, "no")) {
    log_msg("CASSIA disabled in config")
    return(FALSE)
  }
  if (identical(mode, "auto") && nzchar(Sys.getenv("SCAUTO_NO_PKG_INSTALL"))) {
    log_msg("CASSIA skipped on HPC compute node (no internet / agent CLI); using marker panels")
    return(FALSE)
  }
  TRUE
}

cassia_cluster_short <- function(x) {
  x <- as.character(x)
  ifelse(grepl("c[0-9]+", x), gsub(".*?(c[0-9]+).*", "\\1", x), x)
}

cassia_match_ids <- function(clusters, result_ids) {
  clusters <- as.character(clusters)
  result_ids <- as.character(result_ids)
  i <- match(clusters, result_ids)
  miss <- is.na(i)
  if (any(miss)) {
    i[miss] <- match(cassia_cluster_short(clusters[miss]), cassia_cluster_short(result_ids))
  }
  i
}

read_cassia_results_csv <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  tab <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(tab) || !nrow(tab)) return(NULL)
  nms <- names(tab)
  id_col <- if ("Result ID" %in% nms) "Result ID" else if ("cluster_id" %in% nms) "cluster_id" else nms[[1]]
  sub_col <- if ("sub_cell_type" %in% nms) "sub_cell_type" else if ("subtype" %in% nms) "subtype" else NA_character_
  main_col <- if ("main_cell_type" %in% nms) "main_cell_type" else NA_character_
  if (is.na(sub_col)) return(NULL)
  data.frame(
    cluster_id = as.character(tab[[id_col]]),
    cassia_main = if (is.na(main_col)) NA_character_ else as.character(tab[[main_col]]),
    cassia_sub = as.character(tab[[sub_col]]),
    cassia_markers = if ("key_markers" %in% nms) as.character(tab$key_markers) else NA_character_,
    cassia_reason = if ("reason" %in% nms) as.character(tab$reason) else NA_character_,
    stringsAsFactors = FALSE
  )
}

apply_cassia_labels <- function(sub, cassia_tab) {
  cl <- as.character(sub$subtype_cluster)
  n <- length(cl)
  idx <- cassia_match_ids(cl, cassia_tab$cluster_id)
  hit <- !is.na(idx)
  lab <- rep(NA_character_, n)
  lab[hit] <- as.character(cassia_tab$cassia_sub[idx[hit]])
  use <- hit & !is.na(lab) & nzchar(lab)
  if (!any(use)) {
    log_msg("CASSIA CSV did not match subtype_cluster IDs")
    return(sub)
  }
  if (!"subtype_panel" %in% colnames(sub[[]])) sub$subtype_panel <- sub$subtype
  st <- as.character(sub$subtype)
  st[use] <- lab[use]
  sub$subtype <- st
  fill <- function(col) {
    v <- rep(NA_character_, n)
    v[use] <- as.character(cassia_tab[[col]][idx[use]])
    v
  }
  sub$cassia_main <- fill("cassia_main")
  sub$cassia_sub <- fill("cassia_sub")
  sub$cassia_markers <- fill("cassia_markers")
  sub$cassia_reason <- fill("cassia_reason")
  method <- rep("panel", n)
  method[use] <- "cassia"
  sub$annotation_method <- method
  sub$subtype_label <- paste0(cassia_cluster_short(cl), " · ", sub$subtype)
  log_msg("CASSIA labelled", sum(use), "/", n, "cells (", length(unique(cl[use])), "clusters)")
  attr(sub, "cassia_table") <- cassia_tab
  sub
}

run_cassia_subcluster <- function(sub, super, markers, cfg, scauto_root, lin_dir) {
  if (is.null(markers) || !nrow(markers) || !"cluster" %in% names(markers)) {
    log_msg("CASSIA skip", super, "- marker table empty")
    return(list(ok = FALSE))
  }
  bin <- find_cassia_bin()
  if (!nzchar(bin)) {
    log_msg("CASSIA skip", super, "- cassia CLI not found")
    return(list(ok = FALSE))
  }
  backend <- cassia_pick_backend(cfg, bin)
  if (!nzchar(backend)) {
    log_msg("CASSIA skip", super, "- no agent/API backend (install Claude Code, or set an API key)")
    return(list(ok = FALSE))
  }
  specs <- load_cassia_lineage_specs(scauto_root)
  cassia_dir <- file.path(lin_dir, "cassia")
  run_dir <- file.path(cassia_dir, "cassia_run")
  dir.create(file.path(cassia_dir, "input"), recursive = TRUE, showWarnings = FALSE)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  mark_path <- file.path(cassia_dir, "input", "findallmarkers.csv")
  utils::write.csv(markers, mark_path, row.names = FALSE)
  val <- cassia_system(bin, c("validate", mark_path), timeout = 60)
  if (!identical(val$status, 0L)) {
    log_msg("CASSIA validate failed", super, paste(utils::tail(val$text, 8), collapse = " | "))
    writeLines(val$text, file.path(cassia_dir, "validate.log"))
    return(list(ok = FALSE))
  }
  major <- cassia_major_info(super, specs)
  ctx <- cassia_additional_context(sub, super)
  tissue <- cassia_cfg_value(cfg, "tissue", if (!is.null(specs$tissue)) specs$tissue else "peripheral blood")
  species <- cassia_cfg_value(cfg, "species", if (!is.null(specs$species)) specs$species else "human")
  n_genes <- as.integer(cassia_cfg_value(cfg, "n_genes", 50L))
  timeout <- as.integer(cassia_cfg_value(cfg, "timeout", 1200L))
  args <- c(
    "subcluster", "run",
    "--markers", mark_path,
    "--full-markers", mark_path,
    "--major-cluster-info", major,
    "--tissue", as.character(tissue),
    "--species", as.character(species),
    "--backend", backend,
    "--n-genes", as.character(n_genes),
    "--cluster-column", "cluster",
    "--gene-column", "gene",
    "--ranking-method", "avg_log2FC",
    "--additional-context", ctx,
    "--timeout", as.character(timeout),
    "--out", run_dir
  )
  if (isTRUE(cassia_cfg_value(cfg, "use_reference", FALSE))) {
    args <- c(args, "--use-reference")
    hint <- if (!is.null(specs$lineages[[super]]$hint)) specs$lineages[[super]]$hint else super
    args <- c(args, "--reference-cell-type-hint", as.character(hint))
  }
  log_msg("CASSIA subcluster", super, "backend", backend, "timeout", timeout, "s")
  run <- cassia_system(bin, args, timeout = timeout)
  writeLines(run$text, file.path(cassia_dir, "subcluster.log"))
  csv <- file.path(run_dir, "subcluster_results.csv")
  if (!file.exists(csv)) {
    alt <- list.files(run_dir, pattern = "subcluster_results\\.csv$", recursive = TRUE, full.names = TRUE)
    if (length(alt)) csv <- alt[[1]]
  }
  tab <- read_cassia_results_csv(csv)
  if (is.null(tab)) {
    log_msg("CASSIA failed", super, "- no subcluster_results.csv. Last log:",
            paste(utils::tail(run$text, 6), collapse = " | "))
    return(list(ok = FALSE, log = run$text))
  }
  dest <- file.path(cassia_dir, "subcluster_results.csv")
  if (!identical(normalizePath(csv, winslash = "/", mustWork = FALSE),
                 normalizePath(dest, winslash = "/", mustWork = FALSE))) {
    file.copy(csv, dest, overwrite = TRUE)
  }
  html <- file.path(run_dir, "subcluster_report.html")
  if (file.exists(html)) file.copy(html, file.path(cassia_dir, "subcluster_report.html"), overwrite = TRUE)
  list(ok = TRUE, table = tab, dir = cassia_dir, backend = backend)
}
