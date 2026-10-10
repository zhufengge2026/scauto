# Optional lineage trajectory (CytoTRACE2 + Monocle3).
# Off unless Advanced Settings turns it on and ticks lineages of interest.

trajectory_cfg_value <- function(cfg, key, default) {
  if (is.null(cfg$trajectory) || is.null(cfg$trajectory[[key]])) return(default)
  cfg$trajectory[[key]]
}

trajectory_enabled <- function(cfg) {
  isTRUE(trajectory_cfg_value(cfg, "enable", FALSE))
}

trajectory_wanted_lineages <- function(cfg, available) {
  if (!trajectory_enabled(cfg)) return(character())
  want_cyto <- !isFALSE(trajectory_cfg_value(cfg, "cytotrace2", TRUE))
  want_mono <- !isFALSE(trajectory_cfg_value(cfg, "monocle3", TRUE))
  if (!want_cyto && !want_mono) {
    log_msg("Trajectory enable is on but CytoTRACE2 and Monocle3 are both off — skip")
    return(character())
  }
  lin <- unlist(cfg$trajectory$lineages, use.names = FALSE)
  lin <- as.character(lin)
  lin <- lin[!is.na(lin) & nzchar(lin)]
  if (!length(lin)) {
    log_msg("Trajectory: no lineages selected in Advanced Settings — skip")
    return(character())
  }
  hit <- intersect(lin, available)
  miss <- setdiff(lin, available)
  if (length(miss)) {
    log_msg("Trajectory skip lineages not present:", paste(miss, collapse = ", "))
  }
  hit
}

traj_out_dir <- function(outdir, super) {
  d <- file.path(outdir, "lineage", super, "trajectory")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

guess_root_subtype <- function(labels) {
  labels <- unique(as.character(labels))
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (!length(labels)) return(NA_character_)
  pat <- "naive|stem|CD56.?bright|TCM|central memory|progenitor"
  hit <- grep(pat, labels, ignore.case = TRUE, value = TRUE)
  if (length(hit)) hit[[1]] else NA_character_
}

plot_umap_continuous <- function(obj, values, title, stem, legend = "score") {
  source_palette(getOption("scauto.project"))
  red <- if ("umap" %in% Seurat::Reductions(obj)) "umap" else Seurat::Reductions(obj)[[1]]
  xy <- as.data.frame(Seurat::Embeddings(obj, red))
  colnames(xy)[1:2] <- c("UMAP_1", "UMAP_2")
  xy$cell <- rownames(xy)
  names(values) <- if (is.null(names(values))) colnames(obj) else names(values)
  xy$score <- as.numeric(values[xy$cell])
  p <- ggplot2::ggplot(xy, ggplot2::aes(UMAP_1, UMAP_2, colour = score)) +
    ggplot2::geom_point(size = 0.35, stroke = 0) +
    ggplot2::scale_colour_gradientn(colours = pal_sequential, na.value = "#D9D4CC", name = legend) +
    ggplot2::ggtitle(title) +
    theme_scauto_umap()
  save_plot(p, stem, width = 8.2, height = 6.6)
}

run_cytotrace2_lineage <- function(sub, super, cfg, outdir) {
  d <- traj_out_dir(outdir, super)
  if (!requireNamespace("CytoTRACE2", quietly = TRUE)) {
    log_msg("CytoTRACE2 skip", super, "- package not installed (GitHub digitalcytometry/cytotrace2)")
    writeLines(
      "CytoTRACE2 is not installed. On a machine with internet: remotes::install_github('digitalcytometry/cytotrace2').",
      file.path(d, "cytotrace2_skip.txt")
    )
    return(NULL)
  }
  min_cells <- as.integer(trajectory_cfg_value(cfg, "min_cells", 200L))
  if (ncol(sub) < min_cells) {
    log_msg("CytoTRACE2 skip", super, "n=", ncol(sub), "<", min_cells)
    return(NULL)
  }
  max_cells <- as.integer(trajectory_cfg_value(cfg, "cytotrace2_max_cells", 20000L))
  cells <- colnames(sub)
  if (length(cells) > max_cells) {
    log_msg("CytoTRACE2 subsample", max_cells, "of", length(cells), "in", super)
    set.seed(as.integer(cfg$normalize$seed %||% 42L))
    ident <- if ("subtype_label" %in% colnames(sub[[]])) as.character(sub$subtype_label) else as.character(sub$subtype)
    names(ident) <- colnames(sub)
    keep <- unlist(lapply(split(cells, ident[cells]), function(x) {
      if (length(x) <= 2L) return(x)
      n <- max(2L, round(length(x) * max_cells / length(cells)))
      sample(x, min(length(x), n))
    }), use.names = FALSE)
    if (length(keep) > max_cells) keep <- sample(keep, max_cells)
    cells <- keep
  }
  counts <- get_counts(sub)
  mat <- counts[, cells, drop = FALSE]
  # CytoTRACE2 expects a dense gene x cell data.frame of raw counts
  df <- as.data.frame(as.matrix(mat))
  ncores <- 1L
  env_n <- suppressWarnings(as.integer(Sys.getenv("NCPUS", "")))
  if (is.finite(env_n) && env_n > 1L) ncores <- min(8L, env_n)
  log_msg("CytoTRACE2", super, "cells=", ncol(df), "genes=", nrow(df), "ncores=", ncores)
  res <- tryCatch(
    CytoTRACE2::cytotrace2(
      df,
      species = "human",
      slot_type = "counts",
      ncores = ncores
    ),
    error = function(e) {
      log_msg("CytoTRACE2 failed", super, conditionMessage(e))
      NULL
    }
  )
  if (is.null(res)) return(NULL)
  if (is.list(res) && !is.data.frame(res)) {
    score <- res$CytoTRACE2_Score
    if (is.null(score) && length(res)) score <- res[[1]]
    tab <- data.frame(
      cell = names(score),
      CytoTRACE2_Score = as.numeric(score),
      stringsAsFactors = FALSE
    )
  } else {
    tab <- as.data.frame(res, stringsAsFactors = FALSE)
    if (!"cell" %in% names(tab)) tab$cell <- rownames(tab)
  }
  score_col <- intersect(c("CytoTRACE2_Score", "cytotrace2_score", "score"), names(tab))
  if (!length(score_col)) {
    num <- names(tab)[vapply(tab, is.numeric, logical(1))]
    score_col <- if (length(num)) num[[1]] else NA_character_
  }
  write_csv(tab, file.path(d, "cytotrace2_scores.csv"))
  sc <- stats::setNames(rep(NA_real_, ncol(sub)), colnames(sub))
  if (!is.na(score_col) && length(score_col)) {
    sc[as.character(tab$cell)] <- as.numeric(tab[[score_col[[1]]]])
  }
  sub$CytoTRACE2_Score <- unname(sc[colnames(sub)])
  plot_umap_continuous(
    sub, sub$CytoTRACE2_Score,
    paste0("CytoTRACE2 · ", super),
    file.path(d, "umap_cytotrace2"),
    legend = "CytoTRACE2"
  )
  list(object = sub, table = tab, score = sc)
}

monocle3_pick_roots <- function(cds, labels, score) {
  closest <- tryCatch(
    cds@principal_graph_aux[["UMAP"]]$pr_graph_cell_proj_closest_vertex,
    error = function(e) NULL
  )
  if (is.null(closest)) return(NULL)
  closest <- as.matrix(closest[colnames(cds), , drop = FALSE])
  pg <- monocle3::principal_graph(cds)[["UMAP"]]
  vnames <- igraph::V(pg)$name
  parts <- as.character(monocle3::partitions(cds))
  names(score) <- if (is.null(names(score))) colnames(cds) else names(score)
  sc <- as.numeric(score[colnames(cds)])
  st <- as.character(labels[colnames(cds)])
  naive <- guess_root_subtype(st)
  roots <- character(0)
  for (p in unique(parts)) {
    idx <- which(parts == p)
    pool <- idx
    if (!is.na(naive) && naive %in% st[idx]) pool <- idx[st[idx] == naive]
    verts <- as.character(closest[pool, 1])
    if (all(!is.finite(sc[pool]))) {
      tab <- table(verts)
      pick <- names(tab)[which.max(tab)]
    } else {
      byv <- tapply(sc[pool], verts, function(z) mean(z, na.rm = TRUE))
      pick <- names(byv)[which.max(byv)]
    }
    vi <- suppressWarnings(as.integer(pick))
    vname <- if (is.finite(vi) && vi >= 1L && vi <= length(vnames)) {
      vnames[vi]
    } else if (pick %in% vnames) {
      pick
    } else {
      NA_character_
    }
    if (!is.na(vname)) roots <- c(roots, vname)
  }
  unique(roots[nzchar(roots)])
}

export_monocle3_graph <- function(cds, d) {
  pg <- monocle3::principal_graph(cds)[["UMAP"]]
  aux <- monocle3::principal_graph_aux(cds)[["UMAP"]]
  dp <- aux[["dp_mst"]]
  edges <- igraph::as_data_frame(pg, what = "edges")
  node_xy <- as.data.frame(t(dp))
  colnames(node_xy)[1:2] <- c("x", "y")
  node_xy$node <- rownames(node_xy)
  seg <- merge(edges, node_xy[, c("node", "x", "y")], by.x = "from", by.y = "node", all.x = TRUE)
  seg <- merge(seg, node_xy[, c("node", "x", "y")], by.x = "to", by.y = "node", all.x = TRUE, suffixes = c("_from", "_to"))
  write_csv(seg, file.path(d, "monocle3_principal_graph_segments.csv"))
  write_csv(node_xy, file.path(d, "monocle3_principal_graph_nodes.csv"))
}

run_monocle3_lineage <- function(sub, super, cfg, outdir, cyto_score = NULL) {
  d <- traj_out_dir(outdir, super)
  if (!requireNamespace("monocle3", quietly = TRUE)) {
    log_msg("Monocle3 skip", super, "- package not installed")
    writeLines(
      "Monocle3 is not installed. On a machine with internet install monocle3 (cole-trapnell-lab), then re-run Advanced trajectory.",
      file.path(d, "monocle3_skip.txt")
    )
    return(invisible(NULL))
  }
  min_cells <- as.integer(trajectory_cfg_value(cfg, "min_cells", 200L))
  if (ncol(sub) < min_cells) {
    log_msg("Monocle3 skip", super, "n=", ncol(sub), "<", min_cells)
    return(invisible(NULL))
  }
  if (!"umap" %in% Seurat::Reductions(sub)) {
    log_msg("Monocle3 skip", super, "- no UMAP on lineage object")
    return(invisible(NULL))
  }
  counts <- get_counts(sub)
  gene_meta <- data.frame(gene_short_name = rownames(counts), row.names = rownames(counts), stringsAsFactors = FALSE)
  cell_meta <- sub[[]]
  cds <- monocle3::new_cell_data_set(
    counts,
    cell_metadata = cell_meta,
    gene_metadata = gene_meta
  )
  umap <- Seurat::Embeddings(sub, "umap")
  SingleCellExperiment::reducedDims(cds)[["UMAP"]] <- umap[colnames(cds), , drop = FALSE]
  log_msg("Monocle3 cluster/learn", super, "n=", ncol(cds))
  cds <- tryCatch({
    cds <- monocle3::cluster_cells(cds, reduction_method = "UMAP")
    monocle3::learn_graph(cds, use_partition = TRUE, verbose = FALSE)
  }, error = function(e) {
    log_msg("Monocle3 graph failed", super, conditionMessage(e))
    NULL
  })
  if (is.null(cds)) return(invisible(NULL))
  labels <- if ("subtype_label" %in% colnames(sub[[]])) {
    stats::setNames(as.character(sub$subtype_label), colnames(sub))
  } else {
    stats::setNames(as.character(sub$subtype), colnames(sub))
  }
  if (is.null(cyto_score)) {
    cyto_score <- stats::setNames(rep(NA_real_, ncol(sub)), colnames(sub))
    if ("CytoTRACE2_Score" %in% colnames(sub[[]])) {
      cyto_score <- stats::setNames(as.numeric(sub$CytoTRACE2_Score), colnames(sub))
    }
  }
  roots <- tryCatch(monocle3_pick_roots(cds, labels, cyto_score), error = function(e) {
    log_msg("Monocle3 root pick failed:", conditionMessage(e))
    NULL
  })
  if (is.null(roots) || !length(roots)) {
    log_msg("Monocle3", super, "- order_cells without a prescribed root")
    cds <- tryCatch(monocle3::order_cells(cds), error = function(e) {
      log_msg("Monocle3 order_cells failed", conditionMessage(e))
      NULL
    })
  } else {
    log_msg("Monocle3 root nodes", paste(roots, collapse = ", "))
    cds <- tryCatch(
      monocle3::order_cells(cds, root_pr_nodes = roots),
      error = function(e) {
        log_msg("Monocle3 order_cells with roots failed:", conditionMessage(e))
        tryCatch(monocle3::order_cells(cds), error = function(e2) NULL)
      }
    )
  }
  if (is.null(cds)) return(invisible(NULL))
  pt <- tryCatch(as.numeric(monocle3::pseudotime(cds)), error = function(e) NULL)
  if (!is.null(pt)) {
    names(pt) <- colnames(cds)
    pt[!is.finite(pt)] <- NA_real_
    tab <- data.frame(
      cell = colnames(sub),
      subtype = unname(labels[colnames(sub)]),
      pseudotime = unname(pt[colnames(sub)]),
      CytoTRACE2_Score = unname(cyto_score[colnames(sub)]),
      stringsAsFactors = FALSE
    )
    write_csv(tab, file.path(d, "monocle3_pseudotime.csv"))
    plot_umap_continuous(
      sub, pt[colnames(sub)],
      paste0("Monocle3 pseudotime · ", super),
      file.path(d, "umap_monocle3_pseudotime"),
      legend = "pseudotime"
    )
  }
  tryCatch(export_monocle3_graph(cds, d), error = function(e) {
    log_msg("Monocle3 graph export failed:", conditionMessage(e))
  })
  max_save <- as.integer(trajectory_cfg_value(cfg, "save_cds_max_cells", 40000L))
  if (ncol(cds) <= max_save) {
    saveRDS(cds, file.path(d, "monocle3_cds.rds"))
    log_msg("Wrote", file.path(d, "monocle3_cds.rds"))
  } else {
    log_msg("Skip Monocle3 CDS RDS (n_cells >", max_save, ")")
  }
  invisible(cds)
}

run_lineage_trajectory <- function(sub, assignments, super, cfg, outdir) {
  if (!trajectory_enabled(cfg)) return(invisible(NULL))
  want_cyto <- !isFALSE(trajectory_cfg_value(cfg, "cytotrace2", TRUE))
  want_mono <- !isFALSE(trajectory_cfg_value(cfg, "monocle3", TRUE))
  if (!want_cyto && !want_mono) return(invisible(NULL))
  log_msg("Trajectory", super, "CytoTRACE2=", want_cyto, "Monocle3=", want_mono, "n=", ncol(sub))
  d <- traj_out_dir(outdir, super)
  ident_col <- if ("subtype_label" %in% names(assignments)) "subtype_label" else "subtype"
  write_csv(
    data.frame(
      superlineage = super,
      n_cells = ncol(sub),
      n_subtypes = length(unique(assignments[[ident_col]])),
      cytotrace2 = want_cyto,
      monocle3 = want_mono,
      stringsAsFactors = FALSE
    ),
    file.path(d, "00_run_info.csv")
  )
  cyto <- NULL
  if (want_cyto) {
    cyto <- tryCatch(
      run_cytotrace2_lineage(sub, super, cfg, outdir),
      error = function(e) {
        log_msg("ERROR CytoTRACE2", super, conditionMessage(e))
        NULL
      }
    )
    if (!is.null(cyto) && !is.null(cyto$object)) sub <- cyto$object
  }
  if (want_mono) {
    tryCatch(
      run_monocle3_lineage(
        sub, super, cfg, outdir,
        cyto_score = if (!is.null(cyto)) cyto$score else NULL
      ),
      error = function(e) log_msg("ERROR Monocle3", super, conditionMessage(e))
    )
  }
  gc_soft()
  invisible(NULL)
}
