# Shared helpers for PBMC Auto-Pilot.

log_msg <- function(...) {
  msg <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ..., sep = " ")
  cat(msg, "\n", sep = "")
  flush.console()
  log_file <- getOption("scauto.log_file", "")
  if (nzchar(log_file)) {
    cat(msg, "\n", file = log_file, append = TRUE, sep = "")
  }
}

mem_gb <- function() {
  if (.Platform$OS.type == "unix") {
    rss <- tryCatch(
      {
        line <- system(paste("ps -o rss= -p", Sys.getpid()), intern = TRUE)
        as.numeric(trimws(line)) / (1024^2)
      },
      error = function(e) NA_real_
    )
    if (!is.na(rss)) return(round(rss, 2))
  }
  round(sum(gc()[, 2]) / 1024, 2)
}

gc_soft <- function() {
  invisible(gc(verbose = FALSE))
}

scauto_disk_free_gb <- function(path) {
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

scauto_want_atlas_rds <- function(cfg) {
  mem <- cfg$memory
  if (is.null(mem)) return(TRUE)
  if (!is.null(mem$save_rds)) {
    v <- mem$save_rds
    if (is.logical(v)) return(isTRUE(v[1]))
    s <- tolower(trimws(as.character(v)[1]))
    if (s %in% c("false", "no", "off", "0")) return(FALSE)
    if (s %in% c("true", "yes", "on", "1")) return(TRUE)
  }
  maxc <- suppressWarnings(as.numeric(mem$save_rds_max_cells)[1])
  if (is.finite(maxc) && maxc <= 0) return(FALSE)
  TRUE
}

scauto_slim_seurat_for_rds <- function(obj) {
  reducs <- tryCatch(Seurat::Reductions(obj), error = function(e) NULL)
  slim <- tryCatch(
    Seurat::DietSeurat(
      obj,
      layers = c("counts", "data"),
      dimreducs = reducs,
      graphs = character(0),
      misc = FALSE
    ),
    error = function(e) {
      log_msg("DietSeurat failed:", conditionMessage(e), "— drop scale.data/graphs by hand")
      NULL
    }
  )
  if (!is.null(slim)) return(slim)
  obj2 <- obj
  obj2 <- tryCatch({
    if ("scale.data" %in% tryCatch(SeuratObject::Layers(obj2[["RNA"]]), error = function(e) character())) {
      SeuratObject::LayerData(obj2, assay = "RNA", layer = "scale.data") <- NULL
    }
    obj2
  }, error = function(e) obj2)
  tryCatch({
    obj2@graphs <- list()
    obj2
  }, error = function(e) obj2)
}

scauto_save_atlas_rds <- function(obj, outdir, slim = TRUE) {
  n <- ncol(obj)
  dest_dir <- file.path(outdir, "objects")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, "atlas_seurat.rds")
  est_gb <- round(0.7 + 0.11 * (n / 1000), 1)
  free_gb <- scauto_disk_free_gb(dest_dir)
  reserve_gb <- max(5, round(est_gb * 0.25, 1))
  if (is.finite(free_gb) && is.finite(est_gb) && free_gb < est_gb + reserve_gb) {
    log_msg(
      "Skip atlas RDS: disk has", free_gb, "GB free; a slimmed object of",
      format(n, big.mark = ","), "cells may need about", est_gb, "GB plus",
      reserve_gb, "GB headroom. Tables and figures are already written."
    )
    return(invisible(FALSE))
  }
  log_msg(
    "Saving atlas RDS (", format(n, big.mark = ","), "cells); slim counts/data/reductions,",
    "drop scale.data and neighbor graphs. RSS", mem_gb(), "GB"
  )
  if (isTRUE(slim)) {
    obj <- scauto_slim_seurat_for_rds(obj)
    gc_soft()
  }
  free2 <- scauto_disk_free_gb(dest_dir)
  if (is.finite(free2) && is.finite(est_gb) && free2 < est_gb + 3) {
    log_msg(
      "Skip atlas RDS after slimming: disk has", free2, "GB free, estimated", est_gb, "GB."
    )
    return(invisible(FALSE))
  }
  tmp <- paste0(dest, ".tmp")
  if (file.exists(tmp)) unlink(tmp)
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  ok <- FALSE
  tryCatch({
    saveRDS(obj, tmp, compress = "gzip")
    if (file.exists(dest)) unlink(dest)
    renamed <- file.rename(tmp, dest)
    if (!isTRUE(renamed) || !file.exists(dest)) {
      file.copy(tmp, dest, overwrite = TRUE)
      unlink(tmp)
    }
    ok <- file.exists(dest)
  }, error = function(e) {
    log_msg("ERROR saving atlas RDS:", conditionMessage(e))
    if (file.exists(tmp)) unlink(tmp)
  })
  gc_soft()
  if (isTRUE(ok) && file.exists(dest)) {
    mb <- round(file.info(dest)$size / 1024^2, 1)
    log_msg("Wrote", dest, mb, "MB. RSS", mem_gb(), "GB")
    return(invisible(TRUE))
  }
  log_msg("Atlas RDS was not written.")
  invisible(FALSE)
}

scauto_parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- list(
    input = NULL,
    samples = NULL,
    meta = NULL,
    outdir = NULL,
    config = NULL,
    project = NULL,
    allow_large = FALSE,
    stages = NULL
  )
  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]
    if (a %in% c("--allow-large", "--allow_large")) {
      out$allow_large <- TRUE
      i <- i + 1L
      next
    }
    if (!startsWith(a, "--") || i == length(args)) {
      stop("Unexpected CLI token: ", a)
    }
    key <- sub("^--", "", a)
    key <- gsub("-", "_", key)
    val <- args[[i + 1L]]
    if (!key %in% names(out)) stop("Unknown flag: --", key)
    out[[key]] <- val
    i <- i + 2L
  }
  out
}

deep_merge <- function(base, over) {
  if (!is.list(over)) return(over)
  if (!is.list(base)) return(over)
  for (nm in names(over)) {
    if (nm %in% names(base) && is.list(base[[nm]]) && is.list(over[[nm]])) {
      base[[nm]] <- deep_merge(base[[nm]], over[[nm]])
    } else {
      base[[nm]] <- over[[nm]]
    }
  }
  base
}

scauto_normalize_tissue <- function(x) {
  t <- tolower(trimws(paste(as.character(x), collapse = " ")))
  t <- gsub("\\s+", " ", t)
  if (!nzchar(t) || identical(t, "na")) return("pbmc")
  if (grepl("liver|hepatic|hepatocyte", t)) return("liver")
  "pbmc"
}

scauto_tissue_from_cfg <- function(cfg) {
  scauto_normalize_tissue(c(
    if (!is.null(cfg$tissue)) cfg$tissue else "",
    if (!is.null(cfg$qc$tissue)) cfg$qc$tissue else "",
    if (!is.null(cfg$cassia$tissue)) cfg$cassia$tissue else ""
  ))
}

scauto_default_mt_for_tissue <- function(tissue, cfg = NULL) {
  if (identical(scauto_normalize_tissue(tissue), "liver")) {
    v <- if (!is.null(cfg$qc$max_percent_mt_liver)) {
      suppressWarnings(as.numeric(cfg$qc$max_percent_mt_liver)[1])
    } else {
      NA_real_
    }
    if (is.finite(v) && v > 0) return(v)
    return(50)
  }
  10
}

apply_tissue_qc_defaults <- function(cfg) {
  tissue <- scauto_tissue_from_cfg(cfg)
  cfg$tissue <- tissue
  if (is.null(cfg$qc) || !is.list(cfg$qc)) cfg$qc <- list()
  cfg$qc$tissue <- tissue
  mt <- suppressWarnings(as.numeric(cfg$qc$max_percent_mt)[1])
  liver_mt <- scauto_default_mt_for_tissue("liver", cfg)
  blood_mt <- 10
  if (identical(tissue, "liver") && (!is.finite(mt) || mt <= blood_mt)) {
    cfg$qc$max_percent_mt <- liver_mt
  }
  if (is.null(cfg$cassia) || !is.list(cfg$cassia)) cfg$cassia <- list()
  if (identical(tissue, "liver")) {
    cfg$cassia$tissue <- "liver"
  }
  cfg
}

load_scauto_config <- function(scauto_root, override = NULL) {
  def <- file.path(scauto_root, "config", "default.yaml")
  stopifnot(file.exists(def))
  cfg <- yaml::read_yaml(def)
  if (!is.null(override) && nzchar(override)) {
    stopifnot(file.exists(override))
    cfg <- deep_merge(cfg, yaml::read_yaml(override))
  }
  apply_tissue_qc_defaults(cfg)
}

ensure_dirs <- function(outdir) {
  subs <- c(
    "tables", "figures", "objects", "lineage", "clinical", "logs"
  )
  for (s in subs) dir.create(file.path(outdir, s), recursive = TRUE, showWarnings = FALSE)
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  invisible(outdir)
}

write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE)
  log_msg("Wrote", path)
  path
}

scauto_reserved_meta_cols <- function() {
  c(
    "sample", "group", "n_barcodes", "donor", "Sex", "sex",
    "Timepoint", "timepoint", "cohort",
    "annotation_method", "cell", "barcode"
  )
}

scauto_parse_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  if (is.logical(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x <- gsub(",", "", x, fixed = TRUE)
  x[x %in% c("", "NA", "NaN", "None", "none", ".", "-", "null")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

scauto_is_numeric_meta <- function(x, min_n = 3L) {
  v <- scauto_parse_numeric(x)
  n <- sum(is.finite(v))
  n >= as.integer(min_n) && length(unique(v[is.finite(v)])) >= 2L
}

scauto_numeric_meta_cols <- function(df, min_n = 3L, extra_skip = character()) {
  if (is.null(df) || !nrow(df) || !ncol(df)) return(character())
  skip <- unique(c(scauto_reserved_meta_cols(), extra_skip))
  nms <- setdiff(names(df), skip)
  nms[vapply(nms, function(nm) scauto_is_numeric_meta(df[[nm]], min_n), logical(1))]
}

scauto_merge_by_sample <- function(base, extra) {
  if (is.null(base) || !nrow(base)) return(base)
  if (is.null(extra) || !nrow(extra) || !"sample" %in% names(extra)) return(base)
  extra$sample <- as.character(extra$sample)
  base$sample <- as.character(base$sample)
  add <- setdiff(names(extra), names(base))
  if (!length(add)) return(base)
  i <- match(base$sample, extra$sample)
  for (nm in add) {
    base[[nm]] <- extra[[nm]][i]
  }
  base
}

scauto_overlay_sample_cols <- function(base, extra, skip = c("sample", "n_barcodes")) {
  if (is.null(base) || !nrow(base)) return(base)
  if (is.null(extra) || !nrow(extra) || !"sample" %in% names(extra)) return(base)
  extra$sample <- as.character(extra$sample)
  base$sample <- as.character(base$sample)
  add <- setdiff(names(extra), skip)
  if (!length(add)) return(base)
  i <- match(base$sample, extra$sample)
  hit <- !is.na(i)
  for (nm in add) {
    vals <- extra[[nm]][i]
    if (!nm %in% names(base)) {
      base[[nm]] <- vals
    } else if (any(hit)) {
      base[[nm]][hit] <- vals[hit]
    }
  }
  base
}

scauto_reserved_meta_cols <- function() {
  c("sample", "analysis_name", "folder", "n_barcodes", "cell", "group")
}

scauto_sanitize_meta_col <- function(nm) {
  nm <- trimws(as.character(nm)[1])
  if (!nzchar(nm)) return("")
  nm <- gsub("[^A-Za-z0-9_]+", "_", nm)
  nm <- gsub("^_+|_+$", "", nm)
  if (!nzchar(nm)) return("")
  if (grepl("^[0-9]", nm)) nm <- paste0("X", nm)
  if (tolower(nm) %in% tolower(scauto_reserved_meta_cols())) return("")
  nm
}

scauto_col_looks_numeric <- function(x) {
  v <- trimws(as.character(x))
  v <- v[!is.na(v) & nzchar(v) & !v %in% c("NA", "NaN")]
  if (!length(v)) return(FALSE)
  nums <- suppressWarnings(as.numeric(gsub(",", "", v)))
  mean(is.finite(nums)) > 0.8
}

scauto_detect_extra_group_cols <- function(sample_meta, cfg = NULL) {
  primary <- "group"
  if (!is.null(cfg) && !is.null(cfg$clinical$group_col) && nzchar(as.character(cfg$clinical$group_col)[1])) {
    primary <- as.character(cfg$clinical$group_col)[1]
  }
  extra_cfg <- character()
  if (!is.null(cfg) && !is.null(cfg$clinical$extra_group_cols)) {
    extra_cfg <- unique(as.character(unlist(cfg$clinical$extra_group_cols, use.names = FALSE)))
    extra_cfg <- extra_cfg[nzchar(trimws(extra_cfg))]
  }
  if (is.null(sample_meta) || !nrow(sample_meta)) {
    return(setdiff(extra_cfg, c(primary, scauto_reserved_meta_cols())))
  }
  cand <- setdiff(names(sample_meta), c(scauto_reserved_meta_cols(), primary))
  keep <- cand[vapply(cand, function(cc) {
    !scauto_col_looks_numeric(sample_meta[[cc]]) &&
      any(nzchar(trimws(as.character(sample_meta[[cc]]))))
  }, logical(1))]
  unique(c(extra_cfg, keep))
}

scauto_group_contrasts <- function(cfg, obj = NULL) {
  primary <- "group"
  if (!is.null(cfg) && !is.null(cfg$clinical$group_col) && nzchar(as.character(cfg$clinical$group_col)[1])) {
    primary <- as.character(cfg$clinical$group_col)[1]
  }
  extra <- character()
  if (!is.null(cfg) && !is.null(cfg$clinical$extra_group_cols)) {
    extra <- unique(as.character(unlist(cfg$clinical$extra_group_cols, use.names = FALSE)))
    extra <- extra[nzchar(trimws(extra))]
  }
  extra <- setdiff(extra, c(primary, scauto_reserved_meta_cols()))
  if (!is.null(obj)) {
    have <- colnames(obj[[]])
    extra <- intersect(extra, have)
    if (!primary %in% have && "group" %in% have) primary <- "group"
  }
  list(primary = primary, extra = extra, all = unique(c(primary, extra)))
}

save_plot <- function(p, stem, width = 10, height = 7, dpi = 200) {
  png <- paste0(stem, ".png")
  pdf <- paste0(stem, ".pdf")
  ggplot2::ggsave(png, p, width = width, height = height, dpi = dpi)
  ggplot2::ggsave(pdf, p, width = width, height = height)
  log_msg("Wrote", png)
  invisible(stem)
}

assay_layer_names <- function(obj, kind = c("counts", "data")) {
  kind <- match.arg(kind)
  ly <- tryCatch(SeuratObject::Layers(obj[["RNA"]]), error = function(e) character(0))
  if (!length(ly)) return(character(0))
  grep(paste0("^", kind, "(\\.|$)"), ly, value = TRUE)
}

layers_are_split <- function(obj) {
  rna <- obj[["RNA"]]
  if (!inherits(rna, "Assay5")) return(FALSE)
  ly <- tryCatch(SeuratObject::Layers(rna), error = function(e) character(0))
  if (!length(ly)) return(FALSE)
  sum(grepl("^counts(\\.|$)", ly)) > 1L || sum(grepl("^data(\\.|$)", ly)) > 1L
}

cbind_named_layers <- function(obj, layer_names) {
  mats <- lapply(layer_names, function(l) {
    SeuratObject::LayerData(obj, assay = "RNA", layer = l)
  })
  genes <- Reduce(intersect, lapply(mats, rownames))
  mats <- lapply(mats, function(m) m[genes, , drop = FALSE])
  mat <- do.call(cbind, mats)
  cells <- colnames(obj)
  have <- intersect(cells, colnames(mat))
  if (!length(have)) return(mat)
  missing <- setdiff(cells, colnames(mat))
  if (length(missing)) {
    log_msg("Split layers missing", length(missing), "cells after cbind")
  }
  mat[, have, drop = FALSE]
}

get_layer <- function(obj, kind = c("counts", "data")) {
  kind <- match.arg(kind)
  hits <- assay_layer_names(obj, kind)
  if (length(hits) > 1L) {
    log_msg("Assay5", kind, "is split across", paste(hits, collapse = ", "),
            "— binding all layers")
    return(cbind_named_layers(obj, hits))
  }
  if (length(hits) == 1L) {
    return(SeuratObject::LayerData(obj, assay = "RNA", layer = hits[[1]]))
  }
  slot <- if (identical(kind, "counts")) "counts" else "data"
  tryCatch(
    Seurat::GetAssayData(obj, assay = "RNA", slot = slot),
    error = function(e) {
      SeuratObject::LayerData(obj, assay = "RNA", layer = kind)
    }
  )
}

get_counts <- function(obj) get_layer(obj, "counts")
get_data <- function(obj) get_layer(obj, "data")

join_rna_if_needed <- function(obj, max_cells = Inf) {
  rna <- obj[["RNA"]]
  if (!inherits(rna, "Assay5")) return(obj)
  if (!layers_are_split(obj)) return(obj)
  ly <- tryCatch(SeuratObject::Layers(rna), error = function(e) character(0))
  if (is.finite(max_cells) && ncol(obj) > max_cells) {
    log_msg(
      "Skip JoinLayers:", ncol(obj), "cells >", max_cells,
      "; will bind split layers on read. Layers:", paste(ly, collapse = ", ")
    )
    return(obj)
  }
  log_msg("JoinLayers RNA (", paste(ly, collapse = ", "), ")")
  joined <- tryCatch(SeuratObject::JoinLayers(rna), error = function(e) {
    log_msg("JoinLayers failed:", conditionMessage(e), "— will bind layers on read")
    NULL
  })
  if (!is.null(joined)) obj[["RNA"]] <- joined
  obj
}

present_genes <- function(obj, genes) {
  intersect(as.character(genes), rownames(obj))
}

# Mitochondrial + ribosomal (cytosolic RPL/RPS, mito-ribo MRPL/MRPS,
# RPLP/RPSA, FAU=RPS30, UBA52 ubiquitin–ribosome fusion).
# Broader than QC percent.ribo (^RP[SL][0-9]) so RPLP0/RPSA/MTRNR are also dropped.
mt_ribo_gene_mask <- function(genes, cfg = NULL) {
  genes <- as.character(genes)
  pats <- c(
    if (!is.null(cfg$qc$mt_pattern) && nzchar(cfg$qc$mt_pattern)) cfg$qc$mt_pattern,
    if (!is.null(cfg$qc$ribo_pattern) && nzchar(cfg$qc$ribo_pattern)) cfg$qc$ribo_pattern,
    "^(MT-|MT\\.|MTRNR)",
    "^(RPL|RPS)",
    "^(MRPL|MRPS)",
    "^(FAU|UBA52)$"
  )
  pats <- unique(pats[nzchar(as.character(pats))])
  drop <- rep(FALSE, length(genes))
  for (p in pats) {
    drop <- drop | grepl(p, genes, ignore.case = TRUE)
  }
  drop
}

genes_without_mt_ribo <- function(genes, cfg = NULL) {
  as.character(genes)[!mt_ribo_gene_mask(genes, cfg)]
}

# Extra symbols to keep off DotPlots: Ensembl BAC/PAC clones (RP11-*, RP4-*,
# RP5-*) are not ribosomal subunits but read as "RP" and crowd the y-axis.
dotplot_exclude_gene_mask <- function(genes, cfg = NULL) {
  genes <- as.character(genes)
  mt_ribo_gene_mask(genes, cfg) |
    grepl("^RP[0-9]+-", genes, ignore.case = TRUE)
}

cluster_tail_number <- function(x) {
  x <- as.character(x)
  vapply(x, function(s) {
    m <- regexpr("_c([0-9]+)", s, perl = TRUE)
    if (m[[1]] > 0L) {
      return(as.numeric(sub(".*_c", "", regmatches(s, m))))
    }
    m2 <- regexpr("(^|\\s)c([0-9]+)", s, perl = TRUE)
    if (m2[[1]] > 0L) {
      hit <- regmatches(s, m2)
      return(as.numeric(sub(".*c", "", hit)))
    }
    n <- suppressWarnings(as.numeric(s))
    if (!is.na(n)) return(n)
    NA_real_
  }, numeric(1), USE.NAMES = FALSE)
}

natural_cluster_order <- function(ids) {
  ids <- unique(as.character(ids))
  num <- cluster_tail_number(ids)
  if (all(!is.na(num))) ids[order(num, ids)] else sort(ids)
}

safe_file_stem <- function(x) {
  x <- as.character(x)[[1]]
  x <- gsub(" · ", "_", x, fixed = TRUE)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) "unnamed" else x
}

sort_cluster_ids <- function(x) natural_cluster_order(x)

# Drop doublets / leaks / debris from proportion and DEG denominators.
# Matches both panel names (Doublet) and CASSIA phrases (doublet, contaminant, leak).
subtype_is_noise <- function(x) {
  grepl(
    "low.?complex|doublet|contaminant|\\bleak\\b|debris|aggregate|platelet",
    as.character(x),
    ignore.case = TRUE
  )
}

lineage_fresh_object <- function(obj, cells, project = "lineage") {
  cells <- intersect(as.character(cells), colnames(obj))
  tmp <- subset(obj, cells = cells)
  tmp <- join_rna_if_needed(tmp, Inf)
  counts <- get_counts(tmp)
  md <- tmp[[]]
  md <- md[colnames(counts), , drop = FALSE]
  drop <- grep("^RNA_snn_res", colnames(md), value = TRUE)
  if (length(drop)) md <- md[, setdiff(colnames(md), drop), drop = FALSE]
  if ("seurat_clusters" %in% names(md)) md$seurat_clusters <- NULL
  if (!identical(ncol(counts), nrow(md))) {
    stop(
      "lineage_fresh_object: counts has ", ncol(counts), " cells but metadata has ",
      nrow(md), ". Split Assay5 layers were not fully joined."
    )
  }
  Seurat::CreateSeuratObject(
    counts = counts,
    meta.data = md,
    project = project,
    min.cells = 0,
    min.features = 0
  )
}

write_manifest <- function(path, x) {
  jsonlite::write_json(x, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  log_msg("Wrote", path)
}

source_palette <- function(project) {
  pal <- file.path(project, "viz", "palette.R")
  if (!file.exists(pal)) {
    stop("Missing viz/palette.R under ", project)
  }
  Sys.setenv(PROJECT_DIR = project, LOCAL_PROJECT_DIR = project)
  source(pal, local = FALSE)
  invisible(TRUE)
}

harmony_cfg_value <- function(cfg, name, default) {
  v <- cfg$harmony[[name]]
  if (is.null(v) || (length(v) == 1L && is.na(v))) default else v
}

log_harmony_mixing <- function(obj, batch, reduction = "harmony") {
  sm <- as.numeric(factor(obj[[batch, drop = TRUE]]))
  cor_max <- function(red, n = 5L) {
    if (!red %in% Seurat::Reductions(obj)) return(NA_real_)
    emb <- Seurat::Embeddings(obj, red)
    k <- min(n, ncol(emb))
    max(abs(stats::cor(emb[, seq_len(k), drop = FALSE], sm)))
  }
  pc_cor <- cor_max("pca")
  h_cor <- cor_max(reduction)
  log_msg(
    "Harmony mixing: max |cor PC1-5 vs", batch, "|", round(pc_cor, 3),
    "→ |cor", reduction, "1-5|", round(h_cor, 3)
  )
  if (is.finite(h_cor) && is.finite(pc_cor) && h_cor > 0.55 && h_cor > 0.85 * pc_cor) {
    log_msg(
      "WARNING: Harmony left a strong", batch, "axis (theta too weak or early_stop). ",
      "Clusters may stay sample-private."
    )
  }
}

log_cluster_batch_mixing <- function(obj, cluster_col, batch = "sample") {
  if (!cluster_col %in% colnames(obj[[]]) || !batch %in% colnames(obj[[]])) return(invisible(NULL))
  tab <- table(obj[[cluster_col, drop = TRUE]], obj[[batch, drop = TRUE]])
  if (!nrow(tab) || !ncol(tab)) return(invisible(NULL))
  maj <- apply(tab, 1, function(x) max(x) / sum(x))
  log_msg(
    "Cluster x", batch, ":", ncol(tab), "batches,", nrow(tab), "clusters,",
    sum(maj > 0.9), ">90% one", batch, "; median majority", round(stats::median(maj), 2)
  )
  as.data.frame.matrix(tab)
}

run_harmony_on_object <- function(obj, cfg, npcs, label = "atlas") {
  do_harm <- cfg$harmony$enable
  n_batch_col <- cfg$harmony$batch_var
  if (is.null(n_batch_col) || !nzchar(n_batch_col)) n_batch_col <- "sample"
  n_sample <- length(unique(obj$sample))
  if (identical(do_harm, "auto")) do_harm <- n_sample > 1L
  do_harm <- isTRUE(do_harm) || identical(do_harm, TRUE)
  if (!do_harm || n_sample <= 1L) {
    log_msg("Harmony skipped (", label, "; n_sample=", n_sample, ")")
    return(obj)
  }
  if (!requireNamespace("harmony", quietly = TRUE)) {
    log_msg("harmony package missing; skip batch correction")
    return(obj)
  }
  if (!n_batch_col %in% colnames(obj[[]])) {
    log_msg("Harmony batch", n_batch_col, "not in metadata; skip")
    return(obj)
  }
  theta <- harmony_cfg_value(cfg, "theta", 4)
  max_iter <- as.integer(harmony_cfg_value(cfg, "max_iter", 20L))
  early_stop <- isTRUE(harmony_cfg_value(cfg, "early_stop", FALSE))
  project_dim <- isTRUE(harmony_cfg_value(cfg, "project_dim", FALSE))
  log_msg(
    "Harmony", label, "by", n_batch_col, "theta", paste(theta, collapse = ","),
    "max_iter", max_iter, "early_stop", early_stop, "project.dim", project_dim
  )
  obj <- tryCatch(
    harmony::RunHarmony(
      obj,
      group.by.vars = n_batch_col,
      reduction.use = "pca",
      dims.use = seq_len(as.integer(npcs)),
      reduction.save = "harmony",
      project.dim = project_dim,
      theta = theta,
      max_iter = max_iter,
      early_stop = early_stop,
      verbose = TRUE
    ),
    error = function(e) {
      log_msg("Harmony failed in", label, ":", conditionMessage(e))
      obj
    }
  )
  if ("harmony" %in% Seurat::Reductions(obj)) {
    log_harmony_mixing(obj, n_batch_col)
  } else {
    log_msg("Harmony did not write a harmony reduction in", label)
  }
  obj
}

choose_compute_mode <- function(n_cells, cfg, allow_large = FALSE) {
  hard <- cfg$memory$hpc_required_cells
  warn <- cfg$memory$warn_cells
  mode <- "laptop"
  note <- sprintf("n_cells=%s within laptop budget", format(n_cells, big.mark = ","))
  if (n_cells > warn && n_cells <= hard) {
    mode <- "laptop_tight"
    note <- sprintf(
      "n_cells=%s is tight for 16GB; Azimuth off; consider HPC",
      format(n_cells, big.mark = ",")
    )
  }
  if (n_cells > hard) {
    if (!isTRUE(allow_large)) {
      stop(
        "This dataset has ", n_cells, " cells after QC, above the ",
        hard, " laptop ceiling. Submit on your HPC with tools/scauto/hpc/submit_scauto.sh ",
        "or pass --allow-large if you really mean to continue."
      )
    }
    mode <- "forced_large"
    note <- "n_cells exceeds laptop ceiling; continuing because --allow-large"
  }
  list(mode = mode, note = note, n_cells = n_cells)
}
