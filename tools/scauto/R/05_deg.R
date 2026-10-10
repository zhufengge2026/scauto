# Clinical-group DEGs (Wilcoxon + MAST) per lineage pool and per subtype.

deg_cfg_value <- function(cfg, key, default) {
  if (is.null(cfg$deg) || is.null(cfg$deg[[key]])) return(default)
  cfg$deg[[key]]
}

deg_file_stem <- function(x) {
  x <- as.character(x)[[1]]
  x <- gsub(" · ", "_", x, fixed = TRUE)
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (!nzchar(x)) "unnamed" else x
}

deg_normalize_methods <- function(cfg) {
  raw <- deg_cfg_value(cfg, "methods", c("wilcoxon", "MAST"))
  raw <- unlist(raw, use.names = FALSE)
  raw <- as.character(raw)
  out <- list()
  for (m in raw) {
    ml <- tolower(m)
    if (ml %in% c("wilcox", "wilcoxon", "wilcoxon-rank-sum")) {
      out[[length(out) + 1L]] <- list(tag = "wilcoxon", test = "wilcox")
    } else if (ml %in% c("mast")) {
      out[[length(out) + 1L]] <- list(tag = "MAST", test = "MAST")
    } else {
      log_msg("Unknown DEG method", m, "- skip")
    }
  }
  out
}

pick_deg_groups <- function(groups, ref = NULL) {
  groups <- unique(as.character(groups))
  groups <- groups[!is.na(groups) & nzchar(groups) & !identical(groups, "all")]
  if (length(groups) < 2L) return(NULL)
  if (!is.null(ref) && nzchar(as.character(ref)[[1]]) && ref %in% groups) {
    ident2 <- as.character(ref)[[1]]
  } else if (all(c("GroupA", "GroupB") %in% groups)) {
    ident2 <- "GroupB"
  } else {
    ident2 <- sort(groups)[[1]]
  }
  rest <- setdiff(groups, ident2)
  if (!length(rest)) return(NULL)
  if (length(rest) > 1L) {
    log_msg(
      "DEG uses one pair:", rest[[1]], "vs", ident2,
      "(other groups not tested:", paste(rest[-1], collapse = ", "), ")"
    )
  }
  list(ident.1 = rest[[1]], ident.2 = ident2)
}

deg_direction_cols <- function(ident.1, ident.2) {
  source_palette(getOption("scauto.project"))
  c1 <- pal_match_one(ident.1)
  c2 <- pal_match_one(ident.2)
  if (is.na(c1) || !nzchar(c1)) c1 <- "#1F3A5F"
  if (is.na(c2) || !nzchar(c2)) c2 <- "#C45C26"
  stats::setNames(
    c(c1, c2, "#B8B2A7"),
    c("up_in_1", "up_in_2", "ns")
  )
}

deg_out_dirs <- function(outdir, level_name, method_tag, group_subdir = NULL) {
  root <- file.path(outdir, "clinical", level_name)
  if (!is.null(group_subdir) && nzchar(group_subdir)) {
    root <- file.path(root, group_subdir)
  }
  dirs <- file.path(root, paste0("deg_", method_tag))
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  dirs
}

deg_write_csv <- function(x, fname, dirs) {
  for (d in dirs) write_csv(x, file.path(d, fname))
}

deg_save_plot <- function(p, fname, dirs, width = 7.2, height = 6.4) {
  for (d in dirs) save_plot(p, file.path(d, fname), width = width, height = height)
}

plot_deg_volcano <- function(df, title, stem_name, dirs, ident.1, ident.2,
                             padj, lfc, n_lab) {
  if (is.null(df) || !nrow(df)) return(invisible(NULL))
  d <- df
  d$padj_plot <- pmax(d$p_val_adj, 1e-300)
  d$neglog <- -log10(d$padj_plot)
  d$neglog[!is.finite(d$neglog)] <- 0
  sig <- !is.na(d$significant) & d$significant
  d$direction <- ifelse(
    !sig, "ns",
    ifelse(d$avg_log2FC > 0, "up_in_1", "up_in_2")
  )
  d$direction <- factor(d$direction, levels = c("up_in_1", "up_in_2", "ns"))
  up <- d[as.character(d$direction) == "up_in_1", , drop = FALSE]
  dn <- d[as.character(d$direction) == "up_in_2", , drop = FALSE]
  n_each <- max(1L, as.integer(ceiling(n_lab / 2)))
  pick <- function(x) {
    if (!nrow(x)) return(x[0, , drop = FALSE])
    x <- x[order(x$p_val_adj, -abs(x$avg_log2FC)), , drop = FALSE]
    head(x, n_each)
  }
  lab_df <- rbind(pick(up), pick(dn))
  if (!nrow(lab_df)) {
    lab_df <- d[order(d$p_val), , drop = FALSE]
    lab_df <- head(lab_df, min(n_lab, nrow(lab_df)))
  }
  xmax <- suppressWarnings(max(abs(d$avg_log2FC), na.rm = TRUE))
  if (!is.finite(xmax) || xmax < 1) xmax <- 1
  cols <- deg_direction_cols(ident.1, ident.2)
  p <- ggplot2::ggplot(d, ggplot2::aes(avg_log2FC, neglog)) +
    ggplot2::geom_point(
      ggplot2::aes(colour = direction),
      size = 1.25, alpha = 0.72, stroke = 0
    ) +
    ggplot2::geom_vline(
      xintercept = c(-lfc, lfc), linetype = "dashed",
      colour = pal_ink, linewidth = 0.35
    ) +
    ggplot2::geom_hline(
      yintercept = -log10(padj), linetype = "dashed",
      colour = pal_ink, linewidth = 0.35
    ) +
    ggplot2::scale_colour_manual(
      values = cols, drop = FALSE,
      labels = c(
        paste0("Up in ", ident.1),
        paste0("Up in ", ident.2),
        "NS"
      ),
      name = NULL
    ) +
    ggplot2::coord_cartesian(xlim = c(-xmax * 1.15, xmax * 1.15), clip = "off") +
    ggplot2::labs(
      title = title,
      x = paste0("avg log2 FC (", ident.1, " / ", ident.2, ")"),
      y = expression(-log[10]("adjusted P"))
    ) +
    ggplot2::theme_classic(base_size = 15) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 15, colour = pal_ink, hjust = 0),
      axis.title = ggplot2::element_text(size = 13, colour = pal_ink),
      axis.text = ggplot2::element_text(size = 12, colour = pal_ink),
      legend.position = "top",
      legend.justification = "left",
      plot.background = ggplot2::element_rect(fill = pal_paper, colour = NA),
      panel.background = ggplot2::element_rect(fill = pal_paper, colour = NA),
      legend.background = ggplot2::element_rect(fill = pal_paper, colour = NA)
    )
  if (nrow(lab_df)) {
    if (requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(
        data = lab_df, ggplot2::aes(label = gene),
        size = 3.2, colour = pal_ink, max.overlaps = 40,
        min.segment.length = 0, seed = 1L
      )
    } else {
      p <- p + ggplot2::geom_text(
        data = lab_df, ggplot2::aes(label = gene),
        size = 3.0, colour = pal_ink, vjust = -0.55
      )
    }
  }
  deg_save_plot(p, stem_name, dirs, width = 7.2, height = 6.4)
}

findmarkers_two_groups <- function(obj, cells, ident.1, ident.2, group_col,
                                   test.use, features, min_pct, logfc_threshold,
                                   min_n, max_cells) {
  cells <- intersect(as.character(cells), colnames(obj))
  if (length(cells) < 2L * min_n) {
    return(list(table = NULL, n1 = 0L, n2 = 0L, skip = "too_few_cells"))
  }
  g <- as.character(obj[[group_col]][cells, 1])
  keep <- g %in% c(ident.1, ident.2)
  cells <- cells[keep]
  g <- g[keep]
  n1 <- sum(g == ident.1)
  n2 <- sum(g == ident.2)
  if (n1 < min_n || n2 < min_n) {
    return(list(table = NULL, n1 = n1, n2 = n2, skip = "too_few_per_group"))
  }
  sub <- obj[, cells]
  Seurat::Idents(sub) <- factor(g, levels = c(ident.1, ident.2))
  args <- list(
    object = sub,
    ident.1 = ident.1,
    ident.2 = ident.2,
    test.use = test.use,
    min.pct = min_pct,
    logfc.threshold = logfc_threshold,
    verbose = FALSE
  )
  if (!is.null(features)) args$features <- features
  if (is.finite(max_cells) && max_cells > 0L) {
    args$max.cells.per.ident <- as.integer(max_cells)
  }
  mk <- tryCatch(
    do.call(Seurat::FindMarkers, args),
    error = function(e) {
      log_msg("FindMarkers", test.use, "failed:", conditionMessage(e))
      NULL
    }
  )
  if (is.null(mk) || !nrow(mk)) {
    return(list(table = NULL, n1 = n1, n2 = n2, skip = "no_genes"))
  }
  if (!"gene" %in% names(mk)) mk$gene <- rownames(mk)
  rownames(mk) <- NULL
  list(table = mk, n1 = n1, n2 = n2, skip = NA_character_)
}

annotate_deg_table <- function(mk, scope, ident, ident.1, ident.2, n1, n2,
                               method_tag, padj, lfc) {
  if (is.null(mk) || !nrow(mk)) return(NULL)
  mk$scope <- scope
  mk$ident <- ident
  mk$method <- method_tag
  mk$ident.1 <- ident.1
  mk$ident.2 <- ident.2
  mk$n_ident.1 <- as.integer(n1)
  mk$n_ident.2 <- as.integer(n2)
  mk$significant <- !is.na(mk$p_val_adj) & mk$p_val_adj < padj &
    abs(mk$avg_log2FC) >= lfc
  mk[order(mk$p_val_adj, -abs(mk$avg_log2FC)), , drop = FALSE]
}

run_one_deg_method <- function(obj, asg, pair, group_col, method, cfg, outdir,
                               level_name, features, group_subdir = NULL) {
  method_tag <- method$tag
  test.use <- method$test
  min_n <- as.integer(deg_cfg_value(cfg, "min_cells_per_group", 10L))
  min_pct <- as.numeric(deg_cfg_value(cfg, "min_pct", 0.1))
  lfc_fm <- as.numeric(deg_cfg_value(cfg, "logfc_threshold", 0.1))
  padj <- as.numeric(deg_cfg_value(cfg, "padj_cutoff", 0.05))
  lfc_sig <- as.numeric(deg_cfg_value(cfg, "lfc_cutoff", 0.25))
  n_lab <- as.integer(deg_cfg_value(cfg, "volcano_n_label", 12L))
  max_cells <- deg_cfg_value(cfg, "max_cells_per_ident", Inf)
  if (is.null(max_cells) || !is.finite(as.numeric(max_cells)[[1]])) {
    max_cells <- Inf
  } else {
    max_cells <- as.numeric(max_cells)[[1]]
  }
  ident.1 <- pair$ident.1
  ident.2 <- pair$ident.2
  dirs <- deg_out_dirs(outdir, level_name, method_tag, group_subdir = group_subdir)
  log_msg(
    "DEG", method_tag, level_name, ident.1, "vs", ident.2,
    if (!is.null(group_subdir) && nzchar(group_subdir)) paste0("(", group_subdir, ")") else ""
  )

  jobs <- list(
    list(scope = "lineage", ident = level_name, cells = asg$cell)
  )
  st_lev <- natural_cluster_order(unique(as.character(asg$subtype)))
  for (st in st_lev) {
    jobs[[length(jobs) + 1L]] <- list(
      scope = "subtype", ident = st, cells = asg$cell[asg$subtype == st]
    )
  }

  sum_rows <- list()
  all_rows <- list()
  if (identical(test.use, "MAST")) options(mc.cores = 1L)

  for (job in jobs) {
    stem <- if (identical(job$scope, "lineage")) {
      "lineage"
    } else {
      paste0("subtype_", deg_file_stem(job$ident))
    }
    log_msg("  ", method_tag, job$scope, job$ident)
    hit <- findmarkers_two_groups(
      obj, job$cells, ident.1, ident.2, group_col,
      test.use, features, min_pct, lfc_fm, min_n, max_cells
    )
    mk <- annotate_deg_table(
      hit$table, job$scope, job$ident, ident.1, ident.2,
      hit$n1, hit$n2, method_tag, padj, lfc_sig
    )
    n_sig <- if (is.null(mk)) 0L else sum(mk$significant, na.rm = TRUE)
    n_tested <- if (is.null(mk)) 0L else nrow(mk)
    sum_rows[[length(sum_rows) + 1L]] <- data.frame(
      method = method_tag,
      test.use = test.use,
      scope = job$scope,
      ident = job$ident,
      ident.1 = ident.1,
      ident.2 = ident.2,
      n_ident.1 = as.integer(hit$n1),
      n_ident.2 = as.integer(hit$n2),
      n_tested = as.integer(n_tested),
      n_significant = as.integer(n_sig),
      skip = if (is.na(hit$skip)) "" else hit$skip,
      stringsAsFactors = FALSE
    )
    if (is.null(mk)) next
    all_rows[[length(all_rows) + 1L]] <- mk
    deg_write_csv(mk, paste0(stem, "_all_genes.csv"), dirs)
    sig <- mk[mk$significant, , drop = FALSE]
    deg_write_csv(sig, paste0(stem, "_significant.csv"), dirs)
    title <- paste0(
      level_name, " · ", job$ident, " · ", method_tag, " (",
      ident.1, " vs ", ident.2, ")"
    )
    plot_deg_volcano(
      mk, title, paste0("volcano_", stem), dirs,
      ident.1, ident.2, padj, lfc_sig, n_lab
    )
    gc_soft()
  }

  summary <- do.call(rbind, sum_rows)
  deg_write_csv(summary, "00_summary.csv", dirs)
  if (length(all_rows)) {
    long <- do.call(rbind, all_rows)
    deg_write_csv(long, "00_all_genes_long.csv", dirs)
    deg_write_csv(long[long$significant, , drop = FALSE], "00_significant_long.csv", dirs)
  }
  summary
}

run_clinical_deg <- function(obj, assignments, cfg, outdir, level_name) {
  if (isFALSE(deg_cfg_value(cfg, "enable", TRUE))) {
    log_msg("DEG disabled in config for", level_name)
    return(invisible(NULL))
  }
  spec <- scauto_group_contrasts(cfg, obj)
  for (gc in spec$all) {
    sub <- if (identical(gc, spec$primary)) NULL else paste0("by_", gc)
    tryCatch(
      run_clinical_deg_one(obj, assignments, cfg, outdir, level_name,
                           group_col = gc, group_subdir = sub),
      error = function(e) log_msg("ERROR DEG", level_name, gc, conditionMessage(e))
    )
  }
  invisible(NULL)
}

run_clinical_deg_one <- function(obj, assignments, cfg, outdir, level_name,
                                 group_col, group_subdir = NULL) {
  asg <- assignments
  ident_col <- if ("subtype_label" %in% names(asg)) "subtype_label" else "subtype"
  asg$subtype <- as.character(asg[[ident_col]])
  asg <- asg[!is.na(asg$subtype) & nzchar(asg$subtype), ]
  asg <- asg[!subtype_is_noise(asg$subtype), ]
  if (!nrow(asg)) {
    log_msg("DEG skip", level_name, "- no subtype assignments")
    return(invisible(NULL))
  }
  if (group_col %in% colnames(obj[[]])) {
    asg[[group_col]] <- as.character(obj[[group_col]][match(asg$cell, colnames(obj)), 1])
  } else {
    log_msg("DEG skip", level_name, "- no", group_col, "on cells")
    return(invisible(NULL))
  }
  asg <- asg[!is.na(asg[[group_col]]) & nzchar(asg[[group_col]]), ]
  pair <- pick_deg_groups(asg[[group_col]], cfg$clinical$reference_group)
  if (is.null(pair)) {
    log_msg("DEG skip", level_name, group_col, "- need two clinical groups")
    return(invisible(NULL))
  }
  asg$cell <- as.character(asg$cell)
  asg <- asg[asg$cell %in% colnames(obj), ]
  if (!nrow(asg)) return(invisible(NULL))

  obj <- join_rna_if_needed(obj, Inf)
  features <- NULL
  if (isTRUE(deg_cfg_value(cfg, "drop_mt_ribo", TRUE))) {
    allg <- rownames(obj)
    features <- genes_without_mt_ribo(allg, cfg)
    log_msg("DEG drop MT/RB:", length(allg) - length(features), "genes; test", length(features))
    if (length(features) < 50L) features <- NULL
  }

  methods <- deg_normalize_methods(cfg)
  summaries <- list()
  for (method in methods) {
    if (identical(method$test, "MAST") && !requireNamespace("MAST", quietly = TRUE)) {
      log_msg("MAST package not installed; skip deg_MAST for", level_name)
      next
    }
    tab <- tryCatch(
      run_one_deg_method(
        obj, asg, pair, group_col, method, cfg, outdir, level_name, features,
        group_subdir = group_subdir
      ),
      error = function(e) {
        log_msg("ERROR DEG", method$tag, level_name, group_col, conditionMessage(e))
        NULL
      }
    )
    if (!is.null(tab)) summaries[[method$tag]] <- tab
  }
  if (length(summaries)) {
    comb <- do.call(rbind, summaries)
    dest <- file.path(outdir, "clinical", level_name)
    if (!is.null(group_subdir) && nzchar(group_subdir)) dest <- file.path(dest, group_subdir)
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    write_csv(comb, file.path(dest, "deg_summary_both_methods.csv"))
  }
  invisible(NULL)
}
