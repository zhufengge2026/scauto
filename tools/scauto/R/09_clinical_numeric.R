# Subtype proportions vs sample-level clinical numbers (Spearman matrix).
# Heatmap format matches the CD8 T + unconv clinical-numeric figure:
# tiles of rho, uncorrected stars in cells, BH in the table, diverging fill.

clin_num_cfg <- function(cfg, key, default) {
  if (is.null(cfg$clinical_numeric) || is.null(cfg$clinical_numeric[[key]])) return(default)
  cfg$clinical_numeric[[key]]
}

clin_num_enabled <- function(cfg, sample_meta = NULL) {
  en <- clin_num_cfg(cfg, "enable", "auto")
  if (isFALSE(en) || identical(tolower(as.character(en)[1]), "false") ||
      identical(tolower(as.character(en)[1]), "no") ||
      identical(tolower(as.character(en)[1]), "off")) {
    return(FALSE)
  }
  cols <- clin_num_columns(cfg, sample_meta)
  if (isTRUE(en) || identical(tolower(as.character(en)[1]), "true") ||
      identical(tolower(as.character(en)[1]), "yes") ||
      identical(tolower(as.character(en)[1]), "on")) {
    return(length(cols) > 0L)
  }
  length(cols) > 0L
}

clin_num_columns <- function(cfg, sample_meta) {
  raw <- unlist(clin_num_cfg(cfg, "columns", character()), use.names = FALSE)
  raw <- unique(as.character(raw))
  raw <- raw[nzchar(raw)]
  if (length(raw)) {
    if (is.null(sample_meta)) return(raw)
    return(intersect(raw, names(sample_meta)))
  }
  if (is.null(sample_meta)) return(character())
  min_n <- as.integer(clin_num_cfg(cfg, "min_samples", 3L))
  if (!is.finite(min_n) || min_n < 3L) min_n <- 3L
  scauto_numeric_meta_cols(sample_meta, min_n = min_n)
}

clin_pretty_label <- function(x) {
  x <- as.character(x)
  x <- gsub("_", " ", x, fixed = TRUE)
  x <- sub(" log10$", " (log10)", x, ignore.case = TRUE)
  x
}

clin_sig_stars <- function(p) {
  p <- as.numeric(p)
  out <- rep("", length(p))
  out[!is.na(p) & p < 0.05] <- "*"
  out[!is.na(p) & p < 0.01] <- "**"
  out[!is.na(p) & p < 0.001] <- "***"
  out
}

spearman_safe <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  n <- sum(ok)
  if (n < 4L) return(list(rho = NA_real_, p = NA_real_, n = n))
  if (length(unique(x[ok])) < 2L || length(unique(y[ok])) < 2L) {
    return(list(rho = NA_real_, p = 1, n = n))
  }
  ct <- tryCatch(
    suppressWarnings(stats::cor.test(x[ok], y[ok], method = "spearman", exact = FALSE)),
    error = function(e) NULL
  )
  if (is.null(ct)) return(list(rho = NA_real_, p = NA_real_, n = n))
  list(rho = unname(ct$estimate), p = ct$p.value, n = n)
}

merge_clinical_numeric_meta <- function(sample_meta, cfg, outdir = NULL) {
  csv <- clin_num_cfg(cfg, "csv", NULL)
  if (!is.null(csv) && nzchar(as.character(csv)[1]) && file.exists(csv)) {
    extra <- tryCatch(
      utils::read.csv(csv, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) NULL
    )
    sample_meta <- scauto_merge_by_sample(sample_meta, extra)
    log_msg("Merged clinical numeric CSV", csv)
  }
  cols <- clin_num_columns(cfg, sample_meta)
  for (nm in cols) {
    sample_meta[[nm]] <- scauto_parse_numeric(sample_meta[[nm]])
  }
  sample_meta <- clin_add_log10_columns(sample_meta, cols)
  if (!is.null(outdir)) {
    write_csv(sample_meta, file.path(outdir, "tables", "sample_metadata_used.csv"))
  }
  sample_meta
}

clin_add_log10_columns <- function(df, cols) {
  if (is.null(df) || !length(cols)) return(df)
  for (nm in cols) {
    if (grepl("log10$", nm, ignore.case = TRUE)) next
    if (!grepl("viral|load|copies|titer|count", nm, ignore.case = TRUE)) next
    v <- scauto_parse_numeric(df[[nm]])
    if (!any(is.finite(v) & v > 0)) next
    if (any(is.finite(v) & v <= 0)) next
    new_nm <- paste0(nm, "_log10")
    if (new_nm %in% names(df)) next
    df[[new_nm]] <- ifelse(is.finite(v) & v > 0, log10(v), NA_real_)
    log_msg("Added", new_nm, "for Spearman")
  }
  df
}

clin_timepoint_col <- function(df) {
  hit <- intersect(c("Timepoint", "timepoint"), names(df))
  if (length(hit)) hit[[1]] else NA_character_
}

complete_sample_prop_grid <- function(pr) {
  samples <- unique(as.character(pr$sample))
  subtypes <- unique(as.character(pr$subtype))
  grid <- expand.grid(sample = samples, subtype = subtypes, stringsAsFactors = FALSE)
  keep <- setdiff(names(pr), c("sample", "subtype", "n", "prop"))
  sm <- unique(pr[, c("sample", keep), drop = FALSE])
  sm <- sm[!duplicated(sm$sample), , drop = FALSE]
  out <- merge(grid, sm, by = "sample", all.x = TRUE)
  hit <- merge(
    out[, c("sample", "subtype"), drop = FALSE],
    pr[, c("sample", "subtype", "n", "prop"), drop = FALSE],
    by = c("sample", "subtype"),
    all.x = TRUE
  )
  out$n <- hit$n
  out$prop <- hit$prop
  out$n[is.na(out$n)] <- 0
  if (!"n_total" %in% names(out)) {
    tot <- unique(pr[, c("sample", "n_total"), drop = FALSE])
    out$n_total <- tot$n_total[match(out$sample, tot$sample)]
  }
  out$prop[is.na(out$prop)] <- 0
  out
}

plot_spearman_heatmap <- function(stats, title, stem, subtype_levels, var_levels) {
  if (is.null(stats) || !nrow(stats)) return(invisible(NULL))
  source_palette(getOption("scauto.project"))
  div_cols <- if (exists("pal_diverging") && length(pal_diverging) >= 3L) {
    pal_diverging
  } else {
    c("#1F3A5F", "#8FA4BE", "#F5F2EB", "#E09A6A", "#C45C26")
  }
  d <- stats
  d$subtype_lab <- factor(as.character(d$subtype), levels = rev(subtype_levels))
  d$clinical_label <- factor(as.character(d$clinical_label), levels = var_levels)
  d$lab <- ifelse(
    is.na(d$rho),
    "-",
    sprintf("%.2f%s", d$rho, d$sig_raw)
  )
  tps <- unique(as.character(d$Timepoint))
  tps <- tps[!is.na(tps) & nzchar(tps)]
  tp_pref <- c("Baseline", "Followup")
  tps <- c(intersect(tp_pref, tps), setdiff(tps, tp_pref))
  d$Timepoint <- factor(d$Timepoint, levels = tps)
  n_st <- length(subtype_levels)
  n_var <- length(var_levels)
  n_tp <- max(1L, length(tps))
  facet <- n_tp > 1L && !identical(tps, "All_samples")
  p <- ggplot2::ggplot(d, ggplot2::aes(clinical_label, subtype_lab, fill = rho)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = lab), size = 3.2, colour = "black") +
    ggplot2::scale_fill_gradientn(
      colours = div_cols, limits = c(-1, 1), na.value = "#EEEEEE",
      name = "Spearman rho"
    ) +
    ggplot2::labs(
      x = NULL, y = NULL, title = title,
      subtitle = "* p<0.05  ** p<0.01  *** p<0.001  (uncorrected; BH in tables)"
    ) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(colour = "black"),
      axis.title = ggplot2::element_text(colour = "black"),
      axis.text.x = ggplot2::element_text(angle = 40, hjust = 1, vjust = 1, size = 11),
      axis.text.y = ggplot2::element_text(size = 12),
      strip.text = ggplot2::element_text(size = 13, colour = "black"),
      legend.text = ggplot2::element_text(colour = "black"),
      plot.title = ggplot2::element_text(size = 16, colour = pal_ink, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 11, colour = pal_ink),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.position = "right"
    )
  if (facet) {
    p <- p + ggplot2::facet_wrap(~ Timepoint, ncol = min(3L, n_tp))
  }
  w <- max(8.2, 1.25 * n_var * (if (facet) n_tp else 1) + 4.2)
  h <- max(4.8, 0.42 * n_st + 2.8)
  save_plot(p, stem, width = w, height = h)
}

run_clinical_numeric <- function(obj, assignments, sample_meta, cfg, outdir, level_name) {
  if (identical(level_name, "atlas_lineage")) return(invisible(NULL))
  if (!clin_num_enabled(cfg, sample_meta)) return(invisible(NULL))
  cols <- clin_num_columns(cfg, sample_meta)
  extra_log <- paste0(cols, "_log10")
  cols <- unique(c(cols, intersect(extra_log, names(sample_meta))))
  if (!length(cols)) {
    log_msg("Clinical numeric: no usable number columns for", level_name)
    return(invisible(NULL))
  }
  ident_col <- if ("subtype_label" %in% names(assignments)) "subtype_label" else "subtype"
  group_col <- if (!is.null(cfg$clinical$group_col) && nzchar(cfg$clinical$group_col)) {
    cfg$clinical$group_col
  } else {
    "group"
  }
  asg <- assignments
  asg$subtype <- as.character(asg[[ident_col]])
  if (inherits(obj, "Seurat")) {
    if (group_col %in% colnames(obj[[]])) {
      asg[[group_col]] <- obj[[group_col]][match(asg$cell, colnames(obj)), 1]
    }
    asg$sample <- obj$sample[match(asg$cell, colnames(obj))]
  } else if (!"sample" %in% names(asg)) {
    log_msg("Clinical numeric skip", level_name, "- no sample column")
    return(invisible(NULL))
  }
  if (!group_col %in% names(asg)) asg[[group_col]] <- "all"
  pr <- prop_by_sample(asg, group_col, ident_col = "subtype")
  if (is.null(pr) || !nrow(pr) || length(unique(pr$sample)) < 4L) {
    log_msg(
      "Clinical numeric skip", level_name,
      "- need at least 4 samples with subtype proportions"
    )
    return(invisible(NULL))
  }
  pr <- complete_sample_prop_grid(pr)
  sm <- sample_meta
  sm$sample <- as.character(sm$sample)
  join_cols <- unique(c("sample", cols, intersect(c("Timepoint", "timepoint", "donor"), names(sm))))
  sm <- sm[, intersect(join_cols, names(sm)), drop = FALSE]
  pr <- merge(pr, sm, by = "sample", all.x = TRUE)
  tp_col <- clin_timepoint_col(pr)
  if (is.na(tp_col)) {
    pr$Timepoint <- "All_samples"
  } else {
    pr$Timepoint <- as.character(pr[[tp_col]])
    pr$Timepoint[!nzchar(pr$Timepoint) | is.na(pr$Timepoint)] <- "All_samples"
  }
  for (nm in cols) {
    if (!nm %in% names(pr)) next
    pr[[nm]] <- scauto_parse_numeric(pr[[nm]])
  }
  cols <- cols[cols %in% names(pr)]
  if (!length(cols)) return(invisible(NULL))

  subtype_levels <- natural_cluster_order(unique(as.character(pr$subtype)))
  var_labels <- clin_pretty_label(cols)
  names(var_labels) <- cols
  tps <- unique(as.character(pr$Timepoint))
  tp_pref <- c("Baseline", "Followup", "All_samples")
  tps <- c(intersect(tp_pref, tps), setdiff(tps, tp_pref))

  rows <- list()
  add_block <- function(d, tp_lab) {
    for (st in subtype_levels) {
      sub <- d[as.character(d$subtype) == st, , drop = FALSE]
      for (vn in cols) {
        sp <- spearman_safe(sub$prop, sub[[vn]])
        rows[[length(rows) + 1L]] <<- data.frame(
          metric = "prop_within_lineage",
          clinical_var = vn,
          clinical_label = unname(var_labels[[vn]]),
          Timepoint = tp_lab,
          subtype = st,
          n_samples = sp$n,
          rho = sp$rho,
          p_value = sp$p,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  for (tp in tps) {
    add_block(pr[as.character(pr$Timepoint) == tp, , drop = FALSE], tp)
  }
  if (length(tps) > 1L) add_block(pr, "All_samples")
  stats <- do.call(rbind, rows)
  if (is.null(stats) || !nrow(stats)) return(invisible(NULL))
  stats$p_adj_BH <- NA_real_
  split_id <- paste(stats$clinical_var, stats$Timepoint, sep = "\t")
  for (sid in unique(split_id)) {
    i <- which(split_id == sid)
    stats$p_adj_BH[i] <- stats::p.adjust(stats$p_value[i], method = "BH")
  }
  stats$sig_raw <- clin_sig_stars(stats$p_value)
  stats$sig_adj <- clin_sig_stars(stats$p_adj_BH)
  stats$significant_nominal_0.05 <- !is.na(stats$p_value) & stats$p_value < 0.05
  stats$significant_FDR_0.05 <- !is.na(stats$p_adj_BH) & stats$p_adj_BH < 0.05

  dest <- file.path(outdir, "clinical", level_name, "clinical_numeric")
  fig_dir <- file.path(dest, "figures")
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(pr, file.path(dest, "01_sample_proportions_with_clinical.csv"))
  write_csv(stats, file.path(dest, "02_spearman_clinical_vs_proportion.csv"))
  hits <- stats[stats$significant_nominal_0.05 | stats$significant_FDR_0.05, , drop = FALSE]
  write_csv(hits, file.path(dest, "04_significant_hits_nominal_or_FDR.csv"))
  summ <- data.frame(
    lineage = level_name,
    n_samples = length(unique(pr$sample)),
    n_subtypes = length(subtype_levels),
    n_clinical_vars = length(cols),
    spearman_nominal = sum(stats$significant_nominal_0.05, na.rm = TRUE),
    spearman_FDR = sum(stats$significant_FDR_0.05, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  write_csv(summ, file.path(dest, "00_analysis_summary.csv"))

  hm <- stats
  if (length(tps) > 1L) {
    hm <- hm[hm$Timepoint %in% tps, , drop = FALSE]
  }
  plot_spearman_heatmap(
    hm,
    paste0(level_name, " subtype proportion vs clinical numbers"),
    file.path(fig_dir, "heatmap_spearman_prop_within_lineage"),
    subtype_levels,
    unname(var_labels)
  )
  lin_dir <- file.path(outdir, "lineage", level_name)
  dir.create(lin_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(
    file.path(dest, "02_spearman_clinical_vs_proportion.csv"),
    file.path(lin_dir, "spearman_clinical_vs_proportion.csv"),
    overwrite = TRUE
  )
  png_src <- file.path(fig_dir, "heatmap_spearman_prop_within_lineage.png")
  if (file.exists(png_src)) {
    file.copy(png_src, file.path(lin_dir, "heatmap_spearman_clinical.png"), overwrite = TRUE)
  }
  log_msg(
    "Clinical numeric", level_name, "nominal/FDR",
    summ$spearman_nominal, "/", summ$spearman_FDR
  )
  invisible(stats)
}
