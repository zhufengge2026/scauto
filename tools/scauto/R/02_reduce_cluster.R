# Normalize, HVG, PCA, optional Harmony, Leiden, UMAP.

drop_mt_ribo_hvg <- function(obj, cfg) {
  vf <- Seurat::VariableFeatures(obj)
  drop <- rep(FALSE, length(vf))
  if (isTRUE(cfg$normalize$drop_mt_from_hvg)) {
    drop <- drop | grepl(cfg$qc$mt_pattern, vf)
  }
  if (isTRUE(cfg$normalize$drop_ribo_from_hvg)) {
    drop <- drop | grepl(cfg$qc$ribo_pattern, vf)
  }
  vf <- vf[!drop]
  nkeep <- min(as.integer(cfg$normalize$n_hvg), length(vf))
  Seurat::VariableFeatures(obj) <- head(vf, nkeep)
  log_msg("HVG after dropping MT/RP:", length(Seurat::VariableFeatures(obj)))
  obj
}

run_reduce_cluster <- function(obj, cfg) {
  seed <- as.integer(cfg$normalize$seed)
  set.seed(seed)
  npcs <- as.integer(cfg$normalize$npcs)
  dims <- seq_len(as.integer(cfg$normalize$cluster_dims))
  n_hvg_raw <- as.integer(cfg$normalize$n_hvg_raw)

  obj <- join_rna_if_needed(obj, cfg$memory$join_layers_max_cells)
  log_msg("NormalizeData")
  obj <- Seurat::NormalizeData(
    obj,
    normalization.method = "LogNormalize",
    scale.factor = cfg$normalize$scale_factor,
    verbose = FALSE
  )
  obj <- join_rna_if_needed(obj, cfg$memory$join_layers_max_cells)
  log_msg("FindVariableFeatures", n_hvg_raw)
  obj <- Seurat::FindVariableFeatures(
    obj,
    selection.method = "vst",
    nfeatures = n_hvg_raw,
    verbose = FALSE
  )
  obj <- drop_mt_ribo_hvg(obj, cfg)
  hvg <- Seurat::VariableFeatures(obj)

  log_msg("ScaleData HVG only (", length(hvg), " genes); no covariate regression")
  obj <- Seurat::ScaleData(obj, features = hvg, verbose = FALSE)

  log_msg("RunPCA npcs=", npcs)
  obj <- Seurat::RunPCA(obj, features = hvg, npcs = npcs, verbose = FALSE, seed.use = seed)

  obj <- run_harmony_on_object(obj, cfg, npcs, label = "atlas")

  red <- if ("harmony" %in% Seurat::Reductions(obj)) "harmony" else "pca"
  log_msg("Neighbors / Leiden on", red, "dims 1:", max(dims))
  obj <- Seurat::FindNeighbors(
    obj,
    reduction = red,
    dims = dims,
    k.param = as.integer(cfg$cluster$n_neighbors),
    verbose = FALSE
  )
  algo <- as.integer(cfg$cluster$algorithm)
  res <- cfg$cluster$resolution
  clustered <- tryCatch(
    Seurat::FindClusters(
      obj,
      resolution = res,
      algorithm = algo,
      random.seed = seed,
      verbose = FALSE
    ),
    error = function(e) {
      log_msg("Leiden failed (", conditionMessage(e), "); falling back to Louvain")
      Seurat::FindClusters(
        obj,
        resolution = res,
        algorithm = 1,
        random.seed = seed,
        verbose = FALSE
      )
    }
  )
  obj <- clustered
  obj$cluster_atlas <- as.character(Seurat::Idents(obj))
  log_cluster_batch_mixing(obj, "cluster_atlas", harmony_cfg_value(cfg, "batch_var", "sample"))

  log_msg("RunUMAP")
  obj <- Seurat::RunUMAP(
    obj,
    reduction = red,
    dims = dims,
    verbose = FALSE,
    seed.use = seed
  )
  log_msg("Atlas clusters:", length(unique(obj$cluster_atlas)), "RSS", mem_gb(), "GB")
  obj
}

plot_atlas_umaps <- function(obj, outdir) {
  source_palette(getOption("scauto.project"))
  has_values <- function(col) {
    if (!col %in% colnames(obj[[]])) return(FALSE)
    v <- trimws(as.character(obj[[col, drop = TRUE]]))
    any(!is.na(v) & nzchar(v) & !v %in% c("NA", "NaN"))
  }
  jobs <- list(
    list("cluster_atlas", "umap_atlas_cluster", "UMAP · atlas Leiden"),
    list("sample", "umap_atlas_sample", "UMAP · sample")
  )
  if (has_values("group")) {
    jobs <- c(jobs, list(list("group", "umap_atlas_group", "UMAP · clinical group")))
  } else if ("group" %in% colnames(obj[[]])) {
    log_msg("Skip UMAP · clinical group: group column is empty")
  }
  for (item in jobs) {
    if (!has_values(item[[1]])) next
    tryCatch(
      {
        p <- scauto_dimplot(obj, item[[1]], item[[3]], label = item[[1]] == "cluster_atlas")
        save_plot(p, file.path(outdir, "figures", item[[2]]), width = 10, height = 7.5)
      },
      error = function(e) log_msg("ERROR atlas UMAP", item[[1]], conditionMessage(e))
    )
  }
}
