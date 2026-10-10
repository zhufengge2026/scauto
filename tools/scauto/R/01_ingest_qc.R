# Discover 10x triplet folders; QC one sample at a time.

has_10x_triplet <- function(path) {
  files <- list.files(path)
  mtx <- any(grepl("^matrix\\.mtx(\\.gz)?$", files))
  bc <- any(grepl("^barcodes\\.tsv(\\.gz)?$", files))
  ft <- any(grepl("^(features|genes)\\.tsv(\\.gz)?$", files))
  mtx && bc && ft
}

resolve_10x_dir <- function(path) {
  if (has_10x_triplet(path)) return(normalizePath(path))
  nested <- c(
    file.path(path, "filtered_feature_bc_matrix"),
    file.path(path, "filtered_gene_bc_matrices"),
    file.path(path, "outs", "filtered_feature_bc_matrix")
  )
  hit <- nested[dir.exists(nested) & vapply(nested, has_10x_triplet, logical(1))]
  if (length(hit)) return(normalizePath(hit[[1]]))
  NULL
}

discover_sample_dirs <- function(input, samples = NULL) {
  input <- normalizePath(input, mustWork = TRUE)
  one <- resolve_10x_dir(input)
  if (!is.null(one)) {
    sid <- basename(input)
    if (basename(one) %in% c("filtered_feature_bc_matrix", "filtered_gene_bc_matrices")) {
      sid <- basename(dirname(one))
      if (sid == "outs") sid <- basename(dirname(dirname(one)))
    }
    dirs <- one
    names(dirs) <- sid
  } else {
    kids <- list.dirs(input, full.names = TRUE, recursive = FALSE)
    dirs <- vapply(kids, function(d) {
      r <- resolve_10x_dir(d)
      if (is.null(r)) NA_character_ else r
    }, character(1))
    names(dirs) <- basename(kids)
    dirs <- dirs[!is.na(dirs)]
    if (!length(dirs)) {
      stop("No 10x triplet (matrix.mtx + barcodes + features/genes) under: ", input)
    }
  }
  if (!is.null(samples) && nzchar(samples)) {
    want <- trimws(unlist(strsplit(samples, ",")))
    missing <- setdiff(want, names(dirs))
    if (length(missing)) {
      stop("Requested samples not found: ", paste(missing, collapse = ", "),
           "\nAvailable: ", paste(names(dirs), collapse = ", "))
    }
    dirs <- dirs[want]
  }
  dirs
}

read_sample_meta <- function(path, sample_ids) {
  if (is.null(path) || !nzchar(path)) {
    log_msg("No --meta; sample table will only have sample IDs")
    return(data.frame(sample = sample_ids, stringsAsFactors = FALSE))
  }
  meta <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"sample" %in% names(meta)) {
    stop("sample_meta.csv must have a 'sample' column")
  }
  meta$sample <- as.character(meta$sample)
  extra <- setdiff(sample_ids, meta$sample)
  if (length(extra)) {
    stop("These sample folders have no row in --meta: ", paste(extra, collapse = ", "))
  }
  meta[match(sample_ids, meta$sample), , drop = FALSE]
}

scauto_analysis_labels <- function(sample_ids, sample_meta) {
  sample_ids <- as.character(sample_ids)
  labs <- sample_ids
  names(labs) <- sample_ids
  if (is.null(sample_meta) || !nrow(sample_meta) || !"sample" %in% names(sample_meta)) {
    return(labs)
  }
  if (!"analysis_name" %in% names(sample_meta)) return(labs)
  sm <- as.character(sample_meta$sample)
  raw <- trimws(as.character(sample_meta$analysis_name))
  for (sid in sample_ids) {
    i <- match(sid, sm)
    if (is.na(i)) next
    an <- raw[[i]]
    if (!is.na(an) && nzchar(an)) labs[[sid]] <- an
  }
  if (anyDuplicated(unname(labs))) {
    log_msg("Duplicate analysis names; making unique")
    u <- make.unique(unname(labs), sep = "_")
    names(u) <- names(labs)
    labs <- u
  }
  labs
}

scauto_apply_analysis_names <- function(sample_meta) {
  if (is.null(sample_meta) || !nrow(sample_meta) || !"sample" %in% names(sample_meta)) {
    return(sample_meta)
  }
  sid <- as.character(sample_meta$sample)
  labs <- scauto_analysis_labels(sid, sample_meta)
  sample_meta$folder <- sid
  sample_meta$sample <- unname(labs[sid])
  sample_meta
}

find_10x_file <- function(path, patterns) {
  files <- list.files(path, full.names = TRUE)
  bn <- basename(files)
  for (p in patterns) {
    hit <- files[grepl(p, bn, ignore.case = TRUE)]
    if (length(hit)) return(hit[[1]])
  }
  NULL
}

read_one_10x <- function(mat_dir, sample_id) {
  log_msg("Reading", sample_id, mat_dir)
  mtx <- find_10x_file(mat_dir, c("^matrix\\.mtx\\.gz$", "^matrix\\.mtx$"))
  bc <- find_10x_file(mat_dir, c("^barcodes\\.tsv\\.gz$", "^barcodes\\.tsv$"))
  ft <- find_10x_file(mat_dir, c(
    "^features\\.tsv\\.gz$", "^features\\.tsv$",
    "^genes\\.tsv\\.gz$", "^genes\\.tsv$"
  ))
  if (any(vapply(list(mtx, bc, ft), is.null, logical(1)))) {
    stop("Incomplete 10x triplet in ", mat_dir)
  }
  counts <- Seurat::ReadMtx(
    mtx = mtx,
    cells = bc,
    features = ft,
    feature.column = 2,
    cell.column = 1,
    mtx.transpose = FALSE
  )
  if (is.list(counts)) {
    if ("Gene Expression" %in% names(counts)) {
      counts <- counts[["Gene Expression"]]
    } else {
      counts <- counts[[1]]
    }
  }
  genes <- rownames(counts)
  if (anyDuplicated(genes)) {
    log_msg("Duplicate gene symbols in", sample_id, "- keep highest total UMI")
    tot <- Matrix::rowSums(counts)
    keep <- !duplicated(genes) | (seq_along(genes) %in% match(unique(genes), genes))
    # keep max-UMI per symbol
    ord <- order(genes, -tot)
    counts <- counts[ord, , drop = FALSE]
    genes <- rownames(counts)
    counts <- counts[!duplicated(genes), , drop = FALSE]
  }
  bc <- colnames(counts)
  prefix <- paste0(sample_id, "_")
  if (!all(startsWith(bc, prefix))) {
    colnames(counts) <- paste0(prefix, bc)
  }
  obj <- Seurat::CreateSeuratObject(
    counts = counts,
    project = sample_id,
    min.cells = 0,
    min.features = 0
  )
  obj$sample <- sample_id
  obj
}

add_qc_metrics <- function(obj, cfg) {
  qc <- cfg$qc
  obj[["percent.mt"]] <- Seurat::PercentageFeatureSet(obj, pattern = qc$mt_pattern)
  obj[["percent.ribo"]] <- Seurat::PercentageFeatureSet(obj, pattern = qc$ribo_pattern)
  obj[["percent.hb"]] <- Seurat::PercentageFeatureSet(obj, pattern = qc$hb_pattern)
  hk <- present_genes(obj, unlist(qc$hk_genes))
  if (length(hk) >= 3L) {
    counts <- get_counts(obj)
    obj$hk_score <- as.numeric(Matrix::colSums(counts[hk, , drop = FALSE]) / obj$nCount_RNA * 100)
  } else {
    obj$hk_score <- NA_real_
  }
  obj
}

filter_one_sample <- function(obj, cfg) {
  qc <- cfg$qc
  md <- obj[[]]
  keep <- md$percent.mt <= qc$max_percent_mt &
    md$nCount_RNA >= qc$min_ncount &
    md$nCount_RNA <= qc$max_ncount &
    md$nFeature_RNA <= qc$max_nfeature
  min_nf <- qc$min_nfeature
  if (!is.null(min_nf) && is.finite(as.numeric(min_nf)[1]) && as.numeric(min_nf)[1] > 0) {
    keep <- keep & md$nFeature_RNA >= as.numeric(min_nf)[1]
  }
  n_before <- ncol(obj)
  obj <- subset(obj, cells = colnames(obj)[keep])
  log_msg(
    unique(obj$sample), "raw", n_before, "→ QC", ncol(obj),
    sprintf("(dropped %s)", n_before - ncol(obj))
  )
  list(
    obj = obj,
    summary = data.frame(
      sample = unique(obj$sample),
      n_raw = n_before,
      n_qc = ncol(obj),
      dropped = n_before - ncol(obj),
      median_nCount = stats::median(obj$nCount_RNA),
      median_nFeature = stats::median(obj$nFeature_RNA),
      median_percent.mt = stats::median(obj$percent.mt),
      median_percent.ribo = stats::median(obj$percent.ribo),
      median_percent.hb = stats::median(obj$percent.hb),
      stringsAsFactors = FALSE
    )
  )
}

ingest_and_qc <- function(sample_dirs, sample_meta, cfg, outdir) {
  log_msg(
    "QC filters: percent.mt ≤", cfg$qc$max_percent_mt,
    "% (", if (!is.null(cfg$tissue)) cfg$tissue else "pbmc", ")",
    "nCount", cfg$qc$min_ncount, "–", cfg$qc$max_ncount,
    "nFeature ≤", cfg$qc$max_nfeature
  )
  objs <- list()
  summaries <- list()
  extra_all <- setdiff(names(sample_meta), c("sample", "analysis_name", "folder"))
  extra_keep <- extra_all[vapply(extra_all, function(cc) {
    v <- trimws(as.character(sample_meta[[cc]]))
    any(!is.na(v) & nzchar(v) & !v %in% c("NA", "NaN"))
  }, logical(1))]
  extra_skip <- setdiff(extra_all, extra_keep)
  if (length(extra_skip)) {
    log_msg("Skip empty metadata columns:", paste(extra_skip, collapse = ", "))
  }
  labels <- scauto_analysis_labels(names(sample_dirs), sample_meta)
  for (sid in names(sample_dirs)) {
    obj <- read_one_10x(sample_dirs[[sid]], sid)
    row <- sample_meta[sample_meta$sample == sid, , drop = FALSE]
    for (cc in extra_keep) {
      obj[[cc]] <- row[[cc]][1]
    }
    obj$sample <- unname(labels[[sid]])
    obj <- add_qc_metrics(obj, cfg)
    res <- filter_one_sample(obj, cfg)
    objs[[sid]] <- res$obj
    summaries[[sid]] <- res$summary
    rm(obj)
    gc_soft()
    log_msg("RSS after", sid, mem_gb(), "GB")
  }
  qc_tbl <- do.call(rbind, summaries)
  write_csv(qc_tbl, file.path(outdir, "tables", "qc_by_sample.csv"))
  n_qc <- sum(qc_tbl$n_qc)
  list(objs = objs, qc_by_sample = qc_tbl, n_raw = sum(qc_tbl$n_raw), n_qc = n_qc)
}

merge_qc_objects <- function(objs, cfg) {
  stopifnot(length(objs) >= 1L)
  if (length(objs) == 1L) {
    obj <- objs[[1]]
  } else {
    log_msg("Merging", length(objs), "QC'd samples")
    obj <- merge(objs[[1]], y = objs[-1], project = "scauto", merge.data = FALSE)
  }
  rm(objs)
  gc_soft()
  obj <- join_rna_if_needed(obj, cfg$memory$join_layers_max_cells)
  counts <- get_counts(obj)
  min_cells <- max(
    as.integer(cfg$qc$min_cells_per_gene),
    ceiling(cfg$qc$min_cells_per_gene_frac * ncol(obj))
  )
  n_expressed <- Matrix::rowSums(counts > 0)
  keep <- names(n_expressed)[n_expressed >= min_cells]
  log_msg("Gene filter keep", length(keep), "/", nrow(obj), "min_cells", min_cells)
  obj <- obj[intersect(rownames(obj), keep), ]
  obj
}

plot_qc_violins <- function(obj, outdir) {
  source_palette(getOption("scauto.project"))
  md <- obj[[]]
  long <- rbind(
    data.frame(metric = "nCount_RNA", value = md$nCount_RNA, sample = md$sample),
    data.frame(metric = "nFeature_RNA", value = md$nFeature_RNA, sample = md$sample),
    data.frame(metric = "percent.mt", value = md$percent.mt, sample = md$sample),
    data.frame(metric = "percent.ribo", value = md$percent.ribo, sample = md$sample)
  )
  pal <- pal_for(unique(as.character(md$sample)))
  p <- ggplot2::ggplot(long, ggplot2::aes(sample, value, fill = sample)) +
    ggplot2::geom_violin(scale = "width", colour = pal_ink, linewidth = 0.2) +
    ggplot2::facet_wrap(~metric, scales = "free_y", ncol = 2) +
    ggplot2::scale_fill_manual(values = pal, guide = "none") +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = pal_ink),
      strip.background = ggplot2::element_rect(fill = pal_paper, colour = NA),
      plot.background = ggplot2::element_rect(fill = pal_paper, colour = NA),
      panel.background = ggplot2::element_rect(fill = pal_paper)
    ) +
    ggplot2::labs(title = "QC metrics after default filters", x = NULL, y = NULL)
  save_plot(p, file.path(outdir, "figures", "qc_violins_by_sample"), width = 10, height = 7)
}
