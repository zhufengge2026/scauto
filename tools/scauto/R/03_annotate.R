# Atlas markers, lineage module scores, superlineage map, subtype recluster.

SUPERLINEAGE <- c(
  CD4_T = "CD4_T",
  CD8_T = "CD8_T_unconv",
  MAIT = "CD8_T_unconv",
  gdT = "CD8_T_unconv",
  NK = "NK",
  CD14_Mono = "Mono_DC",
  CD16_Mono = "Mono_DC",
  cDC = "Mono_DC",
  pDC = "Mono_DC",
  B = "B_plasma",
  Plasma = "B_plasma",
  Platelet = "Other",
  RBC = "Other",
  Prolif = "Other"
)

run_findallmarkers <- function(obj, group.by, cfg, out_csv, ident_prefix = NULL,
                               drop_mt_ribo = FALSE) {
  obj <- join_rna_if_needed(obj, Inf)
  Seurat::Idents(obj) <- obj[[group.by]][, 1]
  features <- NULL
  if (isTRUE(drop_mt_ribo)) {
    allg <- rownames(obj)
    features <- genes_without_mt_ribo(allg, cfg)
    log_msg(
      "FindAllMarkers drop MT/RB:", length(allg) - length(features),
      "genes; test", length(features)
    )
    if (length(features) < 50L) {
      log_msg("Too few genes after MT/RB drop; testing all genes")
      features <- NULL
    }
  }
  log_msg("FindAllMarkers", group.by, "idents", length(unique(Seurat::Idents(obj))))
  mk_args <- list(
    object = obj,
    assay = "RNA",
    test.use = cfg$markers$test,
    min.pct = cfg$markers$min_pct,
    logfc.threshold = cfg$markers$logfc_threshold,
    return.thresh = cfg$markers$return_thresh,
    only.pos = isTRUE(cfg$markers$only_pos),
    verbose = FALSE
  )
  if (!is.null(features)) mk_args$features <- features
  mk <- do.call(Seurat::FindAllMarkers, mk_args)
  if (!nrow(mk)) {
    log_msg("FindAllMarkers returned 0 rows")
    write_csv(mk, out_csv)
    return(mk)
  }
  names(mk)[names(mk) == "gene"] <- "gene"
  if (isTRUE(drop_mt_ribo) && "gene" %in% names(mk)) {
    n0 <- nrow(mk)
    mk <- mk[!mt_ribo_gene_mask(mk$gene, cfg), , drop = FALSE]
    if (nrow(mk) < n0) {
      log_msg("FindAllMarkers removed", n0 - nrow(mk), "MT/RB rows from table")
    }
  }
  if (!is.null(ident_prefix) && "cluster" %in% names(mk)) {
    mk$cluster_id <- paste0(ident_prefix, mk$cluster)
  }
  mk <- mk[order(mk$cluster, mk$p_val_adj, -abs(mk$avg_log2FC)), ]
  write_csv(mk, out_csv)
  mk
}

markers_n_top <- function(cfg, default = 100L) {
  n <- suppressWarnings(as.integer(cfg$markers$n_top))
  if (!length(n) || is.na(n) || n < 1L) default else n
}

annotate_findallmarkers_subtypes <- function(mk, asg) {
  if (is.null(mk) || !nrow(mk) || is.null(asg) || !nrow(asg)) return(mk)
  keep <- intersect(
    c("subtype_cluster", "subtype", "subtype_label", "superlineage"),
    names(asg)
  )
  if (!length(keep) || !"subtype_cluster" %in% keep) return(mk)
  map <- unique(asg[, keep, drop = FALSE])
  mk$cluster <- as.character(mk$cluster)
  i <- match(mk$cluster, as.character(map$subtype_cluster))
  if (all(is.na(i)) && "cluster_id" %in% names(mk)) {
    i <- match(as.character(mk$cluster_id), as.character(map$subtype_cluster))
  }
  if ("superlineage" %in% names(map)) {
    mk$superlineage <- ifelse(is.na(i), NA_character_, as.character(map$superlineage[i]))
  }
  mk$subtype_cluster <- ifelse(is.na(i), mk$cluster, as.character(map$subtype_cluster[i]))
  if ("subtype" %in% names(map)) {
    mk$subtype <- ifelse(is.na(i), NA_character_, as.character(map$subtype[i]))
  }
  if ("subtype_label" %in% names(map)) {
    mk$subtype_label <- ifelse(is.na(i), NA_character_, as.character(map$subtype_label[i]))
  }
  mk
}

rank_top_markers <- function(d, n_top = 100L) {
  if (is.null(d) || !nrow(d) || !"avg_log2FC" %in% names(d)) {
    if (!is.null(d) && nrow(d)) d$marker_rank <- integer(0)
    return(d)
  }
  ok <- is.finite(d$avg_log2FC) & d$avg_log2FC > 0
  d <- d[ok, , drop = FALSE]
  if (!nrow(d)) {
    d$marker_rank <- integer(0)
    return(d)
  }
  padj <- if ("p_val_adj" %in% names(d)) d$p_val_adj else d$p_val
  pval <- if ("p_val" %in% names(d)) d$p_val else padj
  o <- order(padj, -d$avg_log2FC, pval)
  d <- d[o, , drop = FALSE]
  d <- utils::head(d, as.integer(n_top))
  d$marker_rank <- seq_len(nrow(d))
  d
}

write_subtype_marker_exports <- function(mk, asg, lin_dir, n_top = 100L) {
  dest <- file.path(lin_dir, "markers")
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  n_top <- as.integer(n_top)
  if (is.na(n_top) || n_top < 1L) n_top <- 100L
  if (is.null(mk) || !nrow(mk)) {
    log_msg("No subtype markers to export in", lin_dir)
    return(invisible(mk))
  }
  mk <- annotate_findallmarkers_subtypes(mk, asg)
  ord <- if ("subtype_cluster" %in% names(mk)) mk$subtype_cluster else mk$cluster
  mk <- mk[order(cluster_tail_number(ord), mk$p_val_adj, -abs(mk$avg_log2FC)), ]
  write_csv(mk, file.path(lin_dir, "findallmarkers_subtype.csv"))
  write_csv(mk, file.path(dest, "00_all_genes.csv"))
  keys <- unique(as.character(if ("subtype_cluster" %in% names(mk)) mk$subtype_cluster else mk$cluster))
  keys <- natural_cluster_order(keys)
  top_list <- list()
  for (cl in keys) {
    cl_hit <- as.character(mk$cluster) == cl
    if ("subtype_cluster" %in% names(mk)) {
      cl_hit <- cl_hit | as.character(mk$subtype_cluster) == cl
    }
    d <- mk[cl_hit, , drop = FALSE]
    lab <- if ("subtype_label" %in% names(d)) {
      hit <- unique(as.character(d$subtype_label))
      hit <- hit[!is.na(hit) & nzchar(hit)]
      if (length(hit)) hit[[1]] else cl
    } else {
      cl
    }
    stem <- paste0("subtype_", safe_file_stem(lab))
    write_csv(d, file.path(dest, paste0(stem, "_all.csv")))
    top <- rank_top_markers(d, n_top)
    write_csv(top, file.path(dest, paste0(stem, "_top", n_top, ".csv")))
    if (nrow(top)) top_list[[length(top_list) + 1L]] <- top
  }
  top_all <- if (length(top_list)) do.call(rbind, top_list) else mk[0, , drop = FALSE]
  write_csv(top_all, file.path(dest, paste0("00_top", n_top, ".csv")))
  write_csv(top_all, file.path(lin_dir, paste0("findallmarkers_subtype_top", n_top, ".csv")))
  log_msg(paste0("Wrote subtype markers (all + top ", n_top, ") for ", length(keys), " subtypes → ", dest))
  invisible(mk)
}

write_combined_subtype_marker_tables <- function(outdir, n_top = 100L) {
  root <- file.path(outdir, "lineage")
  if (!dir.exists(root)) return(invisible(NULL))
  n_top <- as.integer(n_top)
  if (is.na(n_top) || n_top < 1L) n_top <- 100L
  bind_write <- function(files, dest) {
    files <- files[file.exists(files)]
    if (!length(files)) return(invisible(NULL))
    tabs <- lapply(files, function(f) {
      utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
    })
    tabs <- tabs[vapply(tabs, nrow, integer(1)) > 0L]
    if (!length(tabs)) return(invisible(NULL))
    cols <- unique(unlist(lapply(tabs, names), use.names = FALSE))
    tabs <- lapply(tabs, function(d) {
      miss <- setdiff(cols, names(d))
      for (m in miss) d[[m]] <- NA
      d[, cols, drop = FALSE]
    })
    write_csv(do.call(rbind, tabs), dest)
    invisible(dest)
  }
  alls <- list.files(root, pattern = "^00_all_genes\\.csv$", recursive = TRUE, full.names = TRUE)
  alls <- alls[basename(dirname(alls)) == "markers"]
  tops <- list.files(
    root, pattern = paste0("^00_top", n_top, "\\.csv$"),
    recursive = TRUE, full.names = TRUE
  )
  tops <- tops[basename(dirname(tops)) == "markers"]
  tab_dir <- file.path(outdir, "tables")
  dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
  if (length(alls)) {
    bind_write(alls, file.path(tab_dir, "all_lineage_subtype_markers_all.csv"))
  }
  if (length(tops)) {
    bind_write(tops, file.path(tab_dir, paste0("all_lineage_subtype_markers_top", n_top, ".csv")))
  }
  invisible(TRUE)
}

score_lineage_modules <- function(obj, marker_json) {
  obj <- join_rna_if_needed(obj, Inf)
  mk <- jsonlite::fromJSON(marker_json, simplifyVector = TRUE)
  lineage_genes <- lapply(mk$lineages, function(x) x$genes)
  for (nm in names(lineage_genes)) {
    g <- present_genes(obj, lineage_genes[[nm]])
    if (length(g) < 2L) {
      log_msg("Skip lineage score", nm, "n_genes", length(g))
      next
    }
    tmp <- paste0("lin_", nm, "_")
    obj <- Seurat::AddModuleScore(obj, features = list(g), name = tmp, search = FALSE)
    src <- paste0(tmp, "1")
    dst <- paste0("score_", nm)
    obj[[dst]] <- obj[[src]]
    obj[[src]] <- NULL
  }
  obj
}

cluster_lineage_table <- function(obj, cluster_col, cfg, marker_json) {
  mk <- jsonlite::fromJSON(marker_json, simplifyVector = TRUE)
  cl <- as.character(obj[[cluster_col]][, 1])
  score_cols <- grep("^score_", colnames(obj[[]]), value = TRUE)
  stopifnot(length(score_cols) >= 2L)
  score_mat <- as.matrix(obj[[]][, score_cols, drop = FALSE])
  colnames(score_mat) <- sub("^score_", "", colnames(score_mat))
  agg <- rowsum(score_mat, group = cl, reorder = FALSE)
  n_cl <- as.numeric(table(cl)[rownames(agg)])
  agg <- sweep(agg, 1, n_cl, "/")
  best <- colnames(agg)[max.col(agg, ties.method = "first")]
  best_val <- apply(agg, 1, max)
  second <- apply(agg, 1, function(x) sort(x, decreasing = TRUE)[min(2L, length(x))])
  margin <- best_val - second
  label <- ifelse(
    best_val > cfg$lineage$score_min & margin > cfg$lineage$margin_min,
    best,
    paste0(best, "_lowconf")
  )
  super <- unname(SUPERLINEAGE[gsub("_lowconf$", "", best)])
  super[is.na(super)] <- "Other"
  tab <- data.frame(
    cluster = rownames(agg),
    n_cells = as.integer(n_cl),
    lineage = label,
    lineage_best = best,
    lineage_score = round(best_val, 4),
    lineage_margin = round(margin, 4),
    superlineage = super,
    stringsAsFactors = FALSE
  )
  tab <- tab[order(suppressWarnings(as.numeric(tab$cluster)), tab$cluster), ]
  scores <- data.frame(cluster = rownames(agg), round(agg, 4), stringsAsFactors = FALSE)
  list(table = tab, scores = scores, dot_order = mk$dotplot_order, lineage_genes = lapply(mk$lineages, `[[`, "genes"))
}

attach_cluster_map <- function(obj, cluster_col, tab) {
  cl <- as.character(obj[[cluster_col]][, 1])
  m <- match(cl, tab$cluster)
  obj$lineage <- tab$lineage[m]
  obj$lineage_best <- tab$lineage_best[m]
  obj$superlineage <- tab$superlineage[m]
  obj
}

ident_plot_labels <- function(obj, ident_col, label_col = NULL) {
  ids <- as.character(obj[[ident_col]][, 1])
  ord <- natural_cluster_order(ids)
  labs <- ord
  if (!is.null(label_col) && label_col %in% colnames(obj[[]])) {
    lab <- as.character(obj[[label_col]][, 1])
    labs <- vapply(ord, function(k) {
      u <- unique(lab[ids == k])
      u <- u[nzchar(u) & !is.na(u)]
      if (!length(u)) return(k)
      paste0(sub(".*_c", "c", k), " · ", u[[1]])
    }, character(1))
  }
  names(labs) <- ord
  labs
}

plot_diagonal_dotplot <- function(obj, group.by, features, title, stem,
                                  ident_labels = NULL, cfg = NULL) {
  obj <- join_rna_if_needed(obj, Inf)
  source_palette(getOption("scauto.project"))
  features <- unique(present_genes(obj, features))
  drop <- dotplot_exclude_gene_mask(features, cfg)
  if (any(drop)) {
    log_msg(
      "DotPlot removed", sum(drop), "MT/RB (or RP##- clone) genes:",
      paste(head(features[drop], 12L), collapse = ", ")
    )
    features <- features[!drop]
  }
  if (length(features) < 3L) return(invisible(NULL))
  ident <- as.character(obj[[group.by]][, 1])
  ident_order <- natural_cluster_order(ident)
  dat <- get_data(obj)
  features <- intersect(features, rownames(dat))
  avg <- vapply(ident_order, function(k) {
    Matrix::rowMeans(dat[features, ident == k, drop = FALSE])
  }, numeric(length(features)))
  if (is.null(dim(avg))) {
    avg <- matrix(avg, ncol = 1, dimnames = list(features, ident_order))
  } else {
    rownames(avg) <- features
    colnames(avg) <- ident_order
  }
  cnt <- get_counts(obj)
  features <- intersect(features, rownames(cnt))
  avg <- avg[features, ident_order, drop = FALSE]
  pct <- vapply(ident_order, function(k) {
    as.numeric(Matrix::rowMeans(cnt[features, ident == k, drop = FALSE] > 0) * 100)
  }, numeric(length(features)))
  if (is.null(dim(pct))) {
    pct <- matrix(pct, ncol = 1, dimnames = list(features, ident_order))
  } else {
    rownames(pct) <- features
    colnames(pct) <- ident_order
  }
  avg_scaled <- t(scale(t(as.matrix(avg))))
  avg_scaled[is.na(avg_scaled)] <- 0
  peak_idx <- apply(avg_scaled, 1, which.max)
  peak_val <- apply(avg_scaled, 1, max)
  gene_order <- rownames(avg_scaled)[order(peak_idx, -peak_val)]
  xlab <- ident_order
  if (!is.null(ident_labels)) {
    xlab <- unname(ident_labels[ident_order])
    xlab[is.na(xlab)] <- ident_order[is.na(xlab)]
  }
  dd <- expand.grid(gene = gene_order, ident = ident_order, stringsAsFactors = FALSE)
  dd$avg_scaled <- as.numeric(avg_scaled[cbind(dd$gene, dd$ident)])
  dd$pct <- as.numeric(pct[cbind(dd$gene, dd$ident)])
  dd$gene <- factor(dd$gene, levels = rev(gene_order))
  dd$xlab <- factor(xlab[match(dd$ident, ident_order)], levels = xlab)
  p <- ggplot2::ggplot(dd, ggplot2::aes(xlab, gene)) +
    ggplot2::geom_point(
      ggplot2::aes(size = pct, fill = avg_scaled),
      shape = 21,
      colour = "black",
      stroke = 0.4
    ) +
    ggplot2::scale_fill_gradientn(
      colours = pal_sequential,
      name = "row-scaled\nmean"
    ) +
    ggplot2::scale_size_area(max_size = 9, name = "% expressed") +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = pal_ink, size = 15),
      axis.text.y = ggplot2::element_text(colour = pal_ink, size = 15),
      axis.title = ggplot2::element_text(colour = pal_ink, size = 16),
      legend.text = ggplot2::element_text(size = 13, colour = pal_ink),
      legend.title = ggplot2::element_text(size = 14, colour = pal_ink),
      plot.title = ggplot2::element_text(colour = pal_ink, hjust = 0, size = 17),
      panel.grid.major = ggplot2::element_line(colour = "#EDE8DF", linewidth = 0.3),
      plot.background = ggplot2::element_rect(fill = pal_paper, colour = NA),
      panel.background = ggplot2::element_rect(fill = pal_paper, colour = NA)
    ) +
    ggplot2::labs(x = NULL, y = NULL)
  save_plot(
    p, stem,
    width = max(10, 0.55 * length(ident_order) + 5),
    height = max(7, 0.32 * length(gene_order) + 3.5)
  )
}

plot_lineage_dotplot <- function(obj, cluster_col, lin, outdir) {
  genes <- unique(unlist(lapply(lin$dot_order, function(nm) {
    head(present_genes(obj, lin$lineage_genes[[nm]]), 4L)
  })))
  labs <- ident_plot_labels(obj, cluster_col, "lineage")
  plot_diagonal_dotplot(
    obj, cluster_col, genes,
    "Classic PBMC lineage markers (clusters in natural order; genes diagonal-aligned)",
    file.path(outdir, "figures", "dotplot_atlas_lineage_markers"),
    ident_labels = labs
  )
}

# State / activation panels must not beat identity markers via z-score when
# their raw means are near zero (trial bug: CD8 TEMRA labelled CD8_prolif).
SUBTYPE_STATE_PANELS <- c(
  "CD4_IFN", "CD4_prolif", "CD8_IFN", "CD8_prolif",
  "NK_IFN", "NK_prolif", "IFN_Mono", "B_IFN"
)

# Rare / specialised identities. Claim the panel's peak cluster when that
# cluster is an outlier (Treg, cDC, gdT, …) even if a common programme
# (TCM, CD14, TEM) still has a higher raw mean.
SUBTYPE_SPECIAL_PANELS <- c(
  "CD4_Treg", "CD4_Tfh", "CD4_CTL", "CD8_like",
  "MAIT", "gdT", "NK_like",
  "NK_CD56bright", "NK_adaptive",
  "cDC", "pDC", "mregDC",
  "Plasma", "ABC_like", "B_switched_memory"
)

panel_peak_stats <- function(v) {
  v <- as.numeric(v)
  ok <- which(is.finite(v))
  if (length(ok) < 2L) {
    return(list(i = NA_integer_, raw = NA_real_, enrich = NA_real_, z = NA_real_))
  }
  i <- ok[[which.max(v[ok])]]
  others <- v[ok[ok != i]]
  mu <- mean(v[ok])
  sdv <- stats::sd(v[ok])
  z <- if (is.finite(sdv) && sdv > 1e-8) (v[[i]] - mu) / sdv else 0
  enrich <- v[[i]] / max(mean(others), 1e-4)
  list(i = i, raw = unname(v[[i]]), enrich = unname(enrich), z = unname(z))
}

# Assign cluster labels from a clusters × panels score matrix (no Seurat).
assign_subtypes_from_matrix <- function(
    mat,
    special_min_raw = 0.12,
    special_min_z = 1.15,
    special_min_enrich = 1.75,
    state_min_raw = 0.20,
    state_min_enrich = 1.55,
    identity_margin = 0.10
) {
  if (is.null(dim(mat))) {
    stop("assign_subtypes_from_matrix expects a matrix")
  }
  n <- nrow(mat)
  assigned <- rep(NA_character_, n)
  mode <- rep(NA_character_, n)
  claimed_enrich <- rep(-Inf, n)
  specials <- intersect(SUBTYPE_SPECIAL_PANELS, colnames(mat))
  states <- intersect(SUBTYPE_STATE_PANELS, colnames(mat))
  identity <- setdiff(colnames(mat), states)
  if (!length(identity)) identity <- colnames(mat)

  for (pn in specials) {
    st <- panel_peak_stats(mat[, pn])
    if (is.na(st$i)) next
    if (is.finite(st$raw) && st$raw >= special_min_raw &&
        is.finite(st$z) && st$z >= special_min_z &&
        is.finite(st$enrich) && st$enrich >= special_min_enrich) {
      i <- st$i
      if (is.na(assigned[[i]]) || st$enrich > claimed_enrich[[i]]) {
        assigned[[i]] <- pn
        mode[[i]] <- "peak_special"
        claimed_enrich[[i]] <- st$enrich
      }
    }
  }

  unclaimed <- which(is.na(assigned))
  if (length(unclaimed) && length(states)) {
    for (pn in states) {
      v <- as.numeric(mat[unclaimed, pn])
      if (!any(is.finite(v))) next
      st <- panel_peak_stats(v)
      if (is.na(st$i)) next
      if (is.finite(st$raw) && st$raw >= state_min_raw &&
          is.finite(st$enrich) && st$enrich >= state_min_enrich) {
        i <- unclaimed[[st$i]]
        if (is.na(assigned[[i]]) || st$enrich > claimed_enrich[[i]]) {
          assigned[[i]] <- pn
          mode[[i]] <- "state"
          claimed_enrich[[i]] <- st$enrich
        }
      }
    }
    unclaimed <- which(is.na(assigned))
  }

  best <- assigned
  best_val <- rep(NA_real_, n)
  margin <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    scores <- as.numeric(mat[i, ])
    names(scores) <- colnames(mat)
    scores[!is.finite(scores)] <- -Inf
    if (is.na(best[[i]])) {
      id_best <- identity[[which.max(scores[identity])]]
      best[[i]] <- id_best
      mode[[i]] <- "identity"
    }
    best_val[[i]] <- unname(scores[[best[[i]]]])
    rest <- scores[names(scores) != best[[i]]]
    sec <- if (length(rest)) max(rest, na.rm = TRUE) else -Inf
    margin[[i]] <- best_val[[i]] - sec
  }
  lab <- best
  lowconf <- mode == "identity" & !(is.finite(best_val) & margin > identity_margin)
  lab[lowconf] <- paste0(best[lowconf], "_lowconf")
  data.frame(
    cluster = rownames(mat),
    suggested_subtype = unname(lab),
    subtype_best = unname(best),
    subtype_score = round(unname(best_val), 4),
    subtype_margin = round(unname(margin), 4),
    assign_mode = unname(mode),
    stringsAsFactors = FALSE
  )
}

recluster_setting <- function(cfg, super, name, default) {
  by <- cfg$recluster$by_lineage
  if (!is.null(by) && !is.null(by[[super]]) && !is.null(by[[super]][[name]])) {
    return(by[[super]][[name]])
  }
  if (identical(name, "resolution") && identical(super, "NK") &&
      !is.null(cfg$recluster$nk_resolution)) {
    return(cfg$recluster$nk_resolution)
  }
  v <- cfg$recluster[[name]]
  if (is.null(v)) default else v
}

load_subtype_panels <- function(scauto_root, super) {
  js <- jsonlite::fromJSON(
    file.path(scauto_root, "config", "subtype_markers.json"),
    simplifyVector = TRUE
  )
  js$lineages[[super]]
}

mean_panel_score <- function(dat, genes) {
  g <- intersect(genes, rownames(dat))
  if (length(g) < 2L) return(rep(NA_real_, ncol(dat)))
  Matrix::colMeans(dat[g, , drop = FALSE])
}

annotate_subtypes <- function(sub, super, scauto_root, cluster_col) {
  panels <- load_subtype_panels(scauto_root, super)
  cl <- as.character(sub[[cluster_col]][, 1])
  if (is.null(panels) || !length(panels)) {
    log_msg("No subtype panels for", super)
    sub$subtype <- cl
    sub$subtype_label <- sub(".*_c", "c", cl)
    return(sub)
  }
  dat <- get_data(sub)
  mat <- sapply(names(panels), function(nm) {
    sc <- mean_panel_score(dat, panels[[nm]])
    tapply(sc, cl, mean, na.rm = TRUE)
  })
  if (is.null(dim(mat))) {
    mat <- matrix(mat, nrow = 1, dimnames = list(unique(cl), names(panels)))
  }
  map <- assign_subtypes_from_matrix(mat)
  if (any(map$assign_mode %in% c("peak_special", "state"))) {
    hit <- map[map$assign_mode %in% c("peak_special", "state"), , drop = FALSE]
    log_msg(
      "  ", super, "peak/state subtype claims:",
      paste(paste0(sub(".*_c", "c", hit$cluster), "=", hit$suggested_subtype),
            collapse = "; ")
    )
  }
  sub$subtype <- map$suggested_subtype[match(cl, map$cluster)]
  short <- sub(".*_c", "c", cl)
  sub$subtype_label <- paste0(short, " · ", sub$subtype)
  attr(sub, "subtype_map") <- cbind(map, as.data.frame(mat, check.names = FALSE))
  sub
}

recluster_one_lineage <- function(obj, super, cfg, scauto_root, outdir,
                                  keep_object = FALSE) {
  cells <- colnames(obj)[obj$superlineage == super]
  if (length(cells) < cfg$recluster$min_cells) {
    log_msg("Skip recluster", super, "n=", length(cells))
    return(NULL)
  }
  log_msg("Recluster", super, "n=", length(cells), "(fresh object, no atlas PCA)")
  sub <- lineage_fresh_object(obj, cells, project = super)
  seed <- as.integer(cfg$normalize$seed)
  set.seed(seed)
  sub <- Seurat::NormalizeData(sub, verbose = FALSE)
  n_hvg <- as.integer(cfg$recluster$n_hvg)
  sub <- Seurat::FindVariableFeatures(
    sub, nfeatures = n_hvg + 500L, verbose = FALSE
  )
  sub <- drop_mt_ribo_hvg(sub, cfg)
  hvg <- Seurat::VariableFeatures(sub)
  sub <- Seurat::ScaleData(sub, features = hvg, verbose = FALSE)
  npcs <- min(as.integer(cfg$recluster$npcs), ncol(sub) - 1L, length(hvg) - 1L)
  sub <- Seurat::RunPCA(sub, features = hvg, npcs = npcs, verbose = FALSE, seed.use = seed)
  sub <- run_harmony_on_object(sub, cfg, npcs, label = super)
  red <- if ("harmony" %in% Seurat::Reductions(sub)) "harmony" else "pca"
  log_msg("Recluster", super, "neighbors on", red)
  dims <- seq_len(min(as.integer(cfg$recluster$dims), npcs))
  res0 <- as.numeric(recluster_setting(cfg, super, "resolution", 0.4))
  min_k <- as.integer(recluster_setting(cfg, super, "min_clusters", 3L))
  max_res <- as.numeric(recluster_setting(cfg, super, "max_resolution", 1.4))
  algo <- as.integer(recluster_setting(
    cfg, super, "algorithm",
    if (is.null(cfg$cluster$algorithm)) 4L else cfg$cluster$algorithm
  ))
  knn <- as.integer(recluster_setting(cfg, super, "n_neighbors", 20L))
  knn <- max(10L, min(knn, ncol(sub) - 1L))
  sub <- Seurat::FindNeighbors(
    sub, reduction = red, dims = dims,
    k.param = knn,
    verbose = FALSE
  )
  used <- as.numeric(res0)
  k <- 1L
  log_msg(
    "  ", super, "target min_clusters=", min_k,
    "start_res=", used, "max_res=", max_res, "algorithm=", algo, "k.param=", knn
  )
  while (TRUE) {
    clustered <- tryCatch(
      Seurat::FindClusters(
        sub, resolution = used, algorithm = algo, random.seed = seed, verbose = FALSE
      ),
      error = function(e) {
        fallback <- if (identical(as.integer(algo), 4L)) 1L else 4L
        log_msg("FindClusters algorithm", algo, "failed; trying", fallback)
        Seurat::FindClusters(
          sub, resolution = used, algorithm = fallback, random.seed = seed, verbose = FALSE
        )
      }
    )
    sub <- clustered
    k <- length(unique(Seurat::Idents(sub)))
    log_msg("  ", super, "res=", used, "clusters=", k)
    if (k >= min_k || used >= max_res) break
    used <- min(max_res, used + 0.2)
  }
  raw <- as.character(Seurat::Idents(sub))
  tab <- sort(table(raw), decreasing = TRUE)
  map <- stats::setNames(paste0(super, "_c", seq_along(tab) - 1L), names(tab))
  sub$subtype_cluster <- unname(map[raw])
  Seurat::Idents(sub) <- factor(
    sub$subtype_cluster,
    levels = natural_cluster_order(sub$subtype_cluster)
  )
  log_cluster_batch_mixing(sub, "subtype_cluster", harmony_cfg_value(cfg, "batch_var", "sample"))
  sub <- Seurat::RunUMAP(sub, reduction = red, dims = dims, verbose = FALSE, seed.use = seed)
  sub <- annotate_subtypes(sub, super, scauto_root, "subtype_cluster")
  sub$subtype_panel <- sub$subtype
  sub$annotation_method <- "panel"

  lin_dir <- file.path(outdir, "lineage", super)
  dir.create(lin_dir, recursive = TRUE, showWarnings = FALSE)
  mk <- run_findallmarkers(
    sub, "subtype_cluster", cfg,
    file.path(lin_dir, "findallmarkers_subtype.csv"),
    drop_mt_ribo = !isFALSE(cfg$markers$drop_mt_ribo)
  )
  if (exists("cassia_should_run", mode = "function") && isTRUE(cassia_should_run(cfg))) {
    cassia_res <- tryCatch(
      run_cassia_subcluster(sub, super, mk, cfg, scauto_root, lin_dir),
      error = function(e) {
        log_msg("CASSIA error", super, conditionMessage(e))
        list(ok = FALSE)
      }
    )
    if (isTRUE(cassia_res$ok)) {
      sub <- apply_cassia_labels(sub, cassia_res$table)
      panel_map <- attr(sub, "subtype_map")
      cassia_tab <- cassia_res$table
      if (!is.null(panel_map) && !is.null(cassia_tab)) {
        cmp <- data.frame(
          cluster = as.character(panel_map$cluster),
          panel_subtype = as.character(panel_map$suggested_subtype),
          stringsAsFactors = FALSE
        )
        i <- cassia_match_ids(cmp$cluster, cassia_tab$cluster_id)
        cmp$cassia_main <- ifelse(is.na(i), NA_character_, cassia_tab$cassia_main[i])
        cmp$cassia_sub <- ifelse(is.na(i), NA_character_, cassia_tab$cassia_sub[i])
        write_csv(cmp, file.path(lin_dir, "cassia_vs_panel.csv"))
      }
    }
  }

  map_tab <- attr(sub, "subtype_map")
  if (!is.null(map_tab)) write_csv(map_tab, file.path(lin_dir, "subtype_annotation.csv"))
  cassia_tab <- attr(sub, "cassia_table")
  if (!is.null(cassia_tab)) write_csv(cassia_tab, file.path(lin_dir, "subtype_cassia.csv"))
  asg <- data.frame(
    cell = colnames(sub),
    sample = sub$sample,
    superlineage = super,
    subtype_cluster = sub$subtype_cluster,
    subtype = sub$subtype,
    subtype_label = sub$subtype_label,
    subtype_panel = if ("subtype_panel" %in% colnames(sub[[]])) sub$subtype_panel else sub$subtype,
    annotation_method = if ("annotation_method" %in% colnames(sub[[]])) sub$annotation_method else "panel",
    cassia_main = if ("cassia_main" %in% colnames(sub[[]])) sub$cassia_main else NA_character_,
    cassia_sub = if ("cassia_sub" %in% colnames(sub[[]])) sub$cassia_sub else NA_character_,
    stringsAsFactors = FALSE
  )
  write_csv(asg, file.path(lin_dir, "cell_assignments.csv"))
  mk <- write_subtype_marker_exports(mk, asg, lin_dir, markers_n_top(cfg))

  source_palette(getOption("scauto.project"))
  used_cassia <- any(as.character(asg$annotation_method) == "cassia")
  umap_title <- paste0("UMAP · ", super, " subtypes", if (used_cassia) " (CASSIA)" else "")
  p <- scauto_dimplot(
    sub, "subtype_label", umap_title,
    label = TRUE, label_as = "cluster_id"
  )
  save_plot(p, file.path(lin_dir, "umap_subtype"), width = 10, height = 7.5)

  feats <- character()
  if (nrow(mk)) {
    top <- do.call(rbind, lapply(split(mk, mk$cluster), function(d) {
      d <- d[d$avg_log2FC > 0, ]
      d <- d[!dotplot_exclude_gene_mask(d$gene, cfg), , drop = FALSE]
      head(d[order(d$p_val_adj, -d$avg_log2FC), ], 5L)
    }))
    feats <- unique(top$gene)
  }
  labs <- ident_plot_labels(sub, "subtype_cluster", "subtype")
  if (length(feats) >= 3L) {
    plot_diagonal_dotplot(
      sub, "subtype_cluster", feats,
      paste0(super, " subtype markers", if (used_cassia) " (CASSIA)" else "",
             " (clusters in natural order; genes diagonal-aligned)"),
      file.path(lin_dir, "dotplot_subtype_markers"),
      ident_labels = labs,
      cfg = cfg
    )
  }
  gc_soft()
  out <- list(
    assignments = asg, markers = mk, map = map_tab, n_cells = ncol(sub),
    n_clusters = k, resolution = used, annotation_method = if (used_cassia) "cassia" else "panel"
  )
  if (isTRUE(keep_object)) out$object <- sub
  out
}
