# Pathway enrichment (GO, KEGG, GSEA, GSVA, Reactome).
# Each database is written to its own folder. Missing packages skip that
# database so the rest of the pipeline still finishes (HPC compute has no net).

pathway_cfg_value <- function(cfg, key, default) {
  if (is.null(cfg$pathway) || is.null(cfg$pathway[[key]])) return(default)
  cfg$pathway[[key]]
}

pathway_enabled <- function(cfg) {
  !isFALSE(pathway_cfg_value(cfg, "enable", TRUE))
}

pathway_db_on <- function(cfg, key) {
  pathway_enabled(cfg) && !isFALSE(pathway_cfg_value(cfg, key, TRUE))
}

pathway_out_dirs <- function(outdir, level_name, db) {
  dirs <- file.path(outdir, "clinical", level_name, "pathway", db)
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  dirs
}

pathway_write <- function(x, fname, dirs) {
  if (is.null(x)) return(invisible(NULL))
  if (is.data.frame(x) && !nrow(x)) return(invisible(NULL))
  for (d in dirs) write_csv(x, file.path(d, fname))
  invisible(x)
}

pathway_note <- function(dirs, fname, text) {
  for (d in dirs) {
    p <- file.path(d, fname)
    dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
    writeLines(text, p)
  }
  invisible(TRUE)
}

pathway_allow_install <- function() {
  !identical(Sys.getenv("SCAUTO_NO_PKG_INSTALL"), "1")
}

pathway_ensure_pkgs <- function() {
  if (isTRUE(getOption("scauto.pathway_pkgs_tried", FALSE))) return(invisible(NULL))
  options(scauto.pathway_pkgs_tried = TRUE)
  if (!requireNamespace("msigdbr", quietly = TRUE) && pathway_allow_install()) {
    log_msg("Installing msigdbr from CRAN for pathway gene sets")
    tryCatch(
      utils::install.packages("msigdbr", repos = "https://cloud.r-project.org"),
      error = function(e) log_msg("msigdbr install failed:", conditionMessage(e))
    )
  }
  bioc <- c("clusterProfiler", "org.Hs.eg.db", "ReactomePA", "GSVA", "fgsea", "AnnotationDbi")
  if (exists("scauto_try_bioc", mode = "function")) {
    scauto_try_bioc(bioc)
  }
  invisible(NULL)
}

# msigdbr 7 uses category/subcategory; 10 uses collection/subcollection.
msig_fetch <- function(collection, subcollection = NULL) {
  if (!requireNamespace("msigdbr", quietly = TRUE)) return(NULL)
  fn <- msigdbr::msigdbr
  fml <- names(formals(fn))
  build_args <- function(species) {
    args <- list()
    if ("species" %in% fml && !is.null(species)) args$species <- species
    if ("collection" %in% fml) {
      args$collection <- collection
      if (!is.null(subcollection) && "subcollection" %in% fml) args$subcollection <- subcollection
    } else {
      args$category <- collection
      if (!is.null(subcollection)) args$subcategory <- subcollection
    }
    args
  }
  out <- NULL
  err <- NULL
  for (sp in c("Homo sapiens", "human")) {
    out <- tryCatch(do.call(fn, build_args(sp)), error = function(e) {
      err <<- conditionMessage(e)
      NULL
    })
    if (!is.null(out) && nrow(out)) break
  }
  if (is.null(out) || !nrow(out)) {
    if (!is.null(err)) {
      log_msg("msigdbr", collection, if (is.null(subcollection)) "" else subcollection, "failed:", err)
    }
    return(NULL)
  }
  gcol <- intersect(c("gene_symbol", "symbol"), names(out))
  tcol <- intersect(c("gs_name", "gs_exact_source", "gs_id"), names(out))
  if (!length(gcol) || !length(tcol)) return(NULL)
  data.frame(
    term = as.character(out[[tcol[[1]]]]),
    gene = as.character(out[[gcol[[1]]]]),
    stringsAsFactors = FALSE
  )
}

msig_term2gene <- function(collection, sub_options = NULL) {
  if (is.null(sub_options) || !length(sub_options)) {
    t2g <- msig_fetch(collection, NULL)
    if (!is.null(t2g) && nrow(t2g)) {
      t2g <- unique(t2g[nzchar(t2g$term) & nzchar(t2g$gene), , drop = FALSE])
      log_msg("Gene sets", collection, "n_terms=", length(unique(t2g$term)))
      return(t2g)
    }
    return(NULL)
  }
  for (sub in sub_options) {
    t2g <- msig_fetch(collection, sub)
    if (!is.null(t2g) && nrow(t2g)) {
      t2g <- unique(t2g[nzchar(t2g$term) & nzchar(t2g$gene), , drop = FALSE])
      log_msg("Gene sets", collection, sub, "n_terms=", length(unique(t2g$term)))
      return(t2g)
    }
  }
  NULL
}

pathway_load_sets <- function() {
  cache <- getOption("scauto.pathway_sets", NULL)
  if (is.list(cache) && length(cache)) return(cache)
  sets <- list(
    GO = msig_term2gene("C5", c("GO:BP", "BP", "GO_BIOLOGICAL_PROCESS")),
    KEGG = msig_term2gene("C2", c("CP:KEGG_LEGACY", "CP:KEGG", "KEGG", "CP:KEGG_MEDICUS")),
    Reactome = msig_term2gene("C2", c("CP:REACTOME", "REACTOME")),
    Hallmark = msig_term2gene("H", NULL)
  )
  if (!is.null(sets$Hallmark) && nrow(sets$Hallmark)) {
    sets$Hallmark <- unique(sets$Hallmark[nzchar(sets$Hallmark$term) & nzchar(sets$Hallmark$gene), ])
  }
  options(scauto.pathway_sets = sets)
  sets
}

pathway_read_deg <- function(outdir, level_name) {
  roots <- c(
    file.path(outdir, "clinical", level_name, "deg_wilcoxon"),
    file.path(outdir, "lineage", level_name, "deg_wilcoxon"),
    file.path(outdir, "clinical", level_name, "deg_MAST"),
    file.path(outdir, "lineage", level_name, "deg_MAST")
  )
  for (d in roots) {
    p <- file.path(d, "00_all_genes_long.csv")
    if (!file.exists(p)) next
    tab <- tryCatch(utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (!is.null(tab) && nrow(tab) && "gene" %in% names(tab)) {
      log_msg("Pathway DEGs from", p, "n=", nrow(tab))
      return(tab)
    }
  }
  NULL
}

pathway_ora_simple <- function(genes, t2g, universe, min_gs = 10L, max_gs = 500L) {
  genes <- unique(intersect(as.character(genes), universe))
  if (length(genes) < 5L) return(NULL)
  t2g <- t2g[t2g$gene %in% universe, , drop = FALSE]
  terms <- split(t2g$gene, t2g$term)
  n_uni <- length(universe)
  n_in <- length(genes)
  rows <- lapply(names(terms), function(tm) {
    gs <- unique(as.character(terms[[tm]]))
    k <- length(gs)
    if (k < min_gs || k > max_gs) return(NULL)
    overlap <- intersect(genes, gs)
    o <- length(overlap)
    if (o < 2L) return(NULL)
    p <- stats::phyper(o - 1L, k, n_uni - k, n_in, lower.tail = FALSE)
    data.frame(
      ID = tm,
      Description = tm,
      GeneRatio = paste0(o, "/", n_in),
      BgRatio = paste0(k, "/", n_uni),
      pvalue = p,
      Count = as.integer(o),
      geneID = paste(overlap, collapse = "/"),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  if (is.null(df) || !nrow(df)) return(NULL)
  df$p.adjust <- stats::p.adjust(df$pvalue, method = "BH")
  df$qvalue <- df$p.adjust
  df[order(df$p.adjust, -df$Count), , drop = FALSE]
}

pathway_ora_clusterprofiler <- function(genes, t2g, universe) {
  if (!requireNamespace("clusterProfiler", quietly = TRUE)) return(NULL)
  genes <- unique(intersect(as.character(genes), universe))
  if (length(genes) < 5L) return(NULL)
  t2g2 <- t2g[, c("term", "gene")]
  names(t2g2) <- c("term", "gene")
  res <- tryCatch(
    clusterProfiler::enricher(
      gene = genes,
      TERM2GENE = t2g2,
      universe = universe,
      pvalueCutoff = 1,
      qvalueCutoff = 1,
      minGSSize = 10,
      maxGSSize = 500,
      pAdjustMethod = "BH"
    ),
    error = function(e) {
      log_msg("enricher failed:", conditionMessage(e))
      NULL
    }
  )
  if (is.null(res)) return(NULL)
  df <- as.data.frame(res)
  if (!nrow(df)) return(NULL)
  df
}

pathway_run_ora <- function(genes, t2g, universe) {
  if (is.null(t2g) || !nrow(t2g)) return(NULL)
  df <- pathway_ora_clusterprofiler(genes, t2g, universe)
  if (is.null(df)) df <- pathway_ora_simple(genes, t2g, universe)
  df
}

pathway_run_gsea <- function(ranked, t2g) {
  if (is.null(t2g) || !nrow(t2g)) return(NULL)
  ranked <- ranked[is.finite(ranked) & nzchar(names(ranked))]
  ranked <- ranked[!duplicated(names(ranked))]
  ranked <- sort(ranked, decreasing = TRUE)
  if (length(ranked) < 50L) return(NULL)
  if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
    log_msg("GSEA skip — clusterProfiler not installed")
    return(NULL)
  }
  t2g2 <- unique(t2g[, c("term", "gene")])
  res <- tryCatch(
    clusterProfiler::GSEA(
      geneList = ranked,
      TERM2GENE = t2g2,
      pvalueCutoff = 1,
      minGSSize = 10,
      maxGSSize = 500,
      pAdjustMethod = "BH",
      verbose = FALSE,
      eps = 0
    ),
    error = function(e) {
      log_msg("GSEA failed:", conditionMessage(e))
      NULL
    }
  )
  if (is.null(res)) return(NULL)
  df <- as.data.frame(res)
  if (!nrow(df)) return(NULL)
  df
}

pathway_reactome_ora <- function(genes, universe_symbols, t2g_fallback) {
  if (requireNamespace("ReactomePA", quietly = TRUE) &&
      requireNamespace("clusterProfiler", quietly = TRUE) &&
      requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    map <- tryCatch(
      clusterProfiler::bitr(
        unique(genes),
        fromType = "SYMBOL",
        toType = "ENTREZID",
        OrgDb = org.Hs.eg.db::org.Hs.eg.db
      ),
      error = function(e) NULL
    )
    if (!is.null(map) && nrow(map)) {
      res <- tryCatch(
        ReactomePA::enrichPathway(
          gene = unique(map$ENTREZID),
          organism = "human",
          pvalueCutoff = 1,
          qvalueCutoff = 1,
          readable = TRUE,
          minGSSize = 10,
          maxGSSize = 500
        ),
        error = function(e) {
          log_msg("ReactomePA failed:", conditionMessage(e))
          NULL
        }
      )
      if (!is.null(res)) {
        df <- as.data.frame(res)
        if (nrow(df)) return(df)
      }
    }
  }
  pathway_run_ora(genes, t2g_fallback, universe_symbols)
}

pathway_plot_ora <- function(df, title, stem, padj) {
  if (is.null(df) || !nrow(df)) return(invisible(NULL))
  desc <- if ("Description" %in% names(df)) df$Description else df$ID
  padj_col <- if ("p.adjust" %in% names(df)) df$p.adjust else df$pvalue
  keep <- which(!is.na(padj_col) & padj_col <= max(padj, 0.2))
  if (!length(keep)) keep <- seq_len(min(15L, nrow(df)))
  d <- df[keep, , drop = FALSE]
  d$label <- as.character(desc[keep])
  d$neglog <- -log10(pmax(padj_col[keep], 1e-300))
  d <- d[order(d$neglog, decreasing = TRUE), , drop = FALSE]
  d <- utils::head(d, 15L)
  d$label <- factor(d$label, levels = rev(d$label))
  source_palette(getOption("scauto.project"))
  p <- ggplot2::ggplot(d, ggplot2::aes(neglog, label)) +
    ggplot2::geom_col(fill = "#1F3A5F", width = 0.72) +
    ggplot2::labs(title = title, x = expression(-log[10]~adjusted~P), y = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(colour = pal_ink, hjust = 0, size = 14),
      axis.text = ggplot2::element_text(colour = pal_ink),
      panel.grid.major.y = ggplot2::element_blank()
    )
  save_plot(p, stem, width = 9.5, height = max(4.5, 0.38 * nrow(d) + 2.2))
}

pathway_deg_pair <- function(deg) {
  i1 <- "focus"
  i2 <- "reference"
  if (!is.null(deg) && nrow(deg)) {
    if ("ident.1" %in% names(deg)) {
      v <- unique(as.character(deg$ident.1))
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v)) i1 <- v[[1]]
    }
    if ("ident.2" %in% names(deg)) {
      v <- unique(as.character(deg$ident.2))
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v)) i2 <- v[[1]]
    }
  }
  list(ident.1 = i1, ident.2 = i2)
}

gsea_group_fills <- function(ident.1, ident.2) {
  source_palette(getOption("scauto.project"))
  col1 <- if (identical(ident.1, "GroupA")) {
    "#1F3A5F"
  } else if (exists("pal_match_one", mode = "function")) {
    pal_match_one(ident.1)
  } else {
    NA_character_
  }
  col2 <- if (identical(ident.2, "GroupB")) {
    "#C45C26"
  } else if (exists("pal_match_one", mode = "function")) {
    pal_match_one(ident.2)
  } else {
    NA_character_
  }
  if (is.na(col1) || !nzchar(col1)) col1 <- "#1F3A5F"
  if (is.na(col2) || !nzchar(col2)) col2 <- "#C45C26"
  c(pos = unname(col1), neg = unname(col2))
}

pathway_plot_gsea <- function(df, title, stem, ident.1 = "focus", ident.2 = "reference") {
  if (is.null(df) || !nrow(df) || !"NES" %in% names(df)) return(invisible(NULL))
  padj_col <- if ("p.adjust" %in% names(df)) df$p.adjust else df$pvalue
  d <- df
  d$label <- as.character(if ("Description" %in% names(d)) d$Description else d$ID)
  d$sig <- !is.na(padj_col) & padj_col < 0.05
  d <- d[order(abs(d$NES), decreasing = TRUE), , drop = FALSE]
  d <- utils::head(d, 20L)
  d <- d[order(d$NES), , drop = FALSE]
  d$label <- factor(d$label, levels = d$label)
  ident.1 <- as.character(ident.1)[[1]]
  ident.2 <- as.character(ident.2)[[1]]
  lab_pos <- paste0("Higher in ", ident.1, " (NES > 0)")
  lab_neg <- paste0("Higher in ", ident.2, " (NES < 0)")
  d$nes_dir <- ifelse(d$NES > 0, lab_pos, lab_neg)
  d$nes_dir <- factor(d$nes_dir, levels = c(lab_pos, lab_neg))
  source_palette(getOption("scauto.project"))
  cols <- gsea_group_fills(ident.1, ident.2)
  fill_vals <- stats::setNames(c(cols[["pos"]], cols[["neg"]]), c(lab_pos, lab_neg))
  dummy <- d[c(1L, 1L), , drop = FALSE]
  dummy$NES <- 0
  dummy$nes_dir <- factor(c(lab_pos, lab_neg), levels = c(lab_pos, lab_neg))
  plot_df <- rbind(dummy, d)
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(NES, label, fill = nes_dir)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::scale_fill_manual(
      values = fill_vals,
      breaks = c(lab_pos, lab_neg),
      drop = FALSE,
      name = NULL
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::labs(title = title, x = "NES", y = NULL) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(colour = pal_ink, hjust = 0, size = 14),
      axis.text = ggplot2::element_text(colour = pal_ink),
      legend.position = "top",
      legend.justification = "left",
      legend.text = ggplot2::element_text(colour = pal_ink, size = 11),
      panel.grid.major.y = ggplot2::element_blank()
    )
  save_plot(p, stem, width = 11.2, height = max(5.2, 0.36 * nrow(d) + 2.6))
}

pathway_jobs_from_deg <- function(deg, min_deg, lfc, padj) {
  if (is.null(deg) || !nrow(deg)) return(list())
  if (!"significant" %in% names(deg)) {
    deg$significant <- !is.na(deg$p_val_adj) & deg$p_val_adj < padj &
      abs(deg$avg_log2FC) >= lfc
  }
  jobs <- list()
  add_job <- function(scope, ident, tab, direction) {
    if (identical(direction, "up")) {
      genes <- tab$gene[isTRUE(tab$significant) & tab$avg_log2FC > 0]
    } else if (identical(direction, "down")) {
      genes <- tab$gene[isTRUE(tab$significant) & tab$avg_log2FC < 0]
    } else {
      genes <- tab$gene[isTRUE(tab$significant)]
    }
    genes <- unique(as.character(genes[!is.na(genes) & nzchar(genes)]))
    if (length(genes) < min_deg) return(invisible(NULL))
    jobs[[length(jobs) + 1L]] <<- list(
      scope = scope, ident = ident, direction = direction, genes = genes
    )
    invisible(NULL)
  }
  scopes <- unique(as.character(deg$scope))
  for (sc in scopes) {
    idents <- unique(as.character(deg$ident[deg$scope == sc]))
    for (id in idents) {
      tab <- deg[deg$scope == sc & deg$ident == id, , drop = FALSE]
      add_job(sc, id, tab, "all")
      add_job(sc, id, tab, "up")
      add_job(sc, id, tab, "down")
    }
  }
  jobs
}

pathway_ranked_lists <- function(deg) {
  if (is.null(deg) || !nrow(deg) || !"avg_log2FC" %in% names(deg)) return(list())
  out <- list()
  for (sc in unique(as.character(deg$scope))) {
    idents <- unique(as.character(deg$ident[deg$scope == sc]))
    for (id in idents) {
      tab <- deg[deg$scope == sc & deg$ident == id, , drop = FALSE]
      v <- stats::setNames(as.numeric(tab$avg_log2FC), as.character(tab$gene))
      v <- v[is.finite(v) & nzchar(names(v))]
      if (length(v) < 50L) next
      # mean log2FC when a gene appears more than once
      if (any(duplicated(names(v)))) {
        v <- tapply(v, names(v), mean, na.rm = TRUE)
        v <- stats::setNames(as.numeric(v), names(v))
      }
      out[[length(out) + 1L]] <- list(scope = sc, ident = id, ranked = v)
    }
  }
  out
}

job_file_stem <- function(scope, ident, extra = "") {
  base <- if (identical(scope, "lineage")) "lineage" else paste0("subtype_", deg_file_stem(ident))
  if (nzchar(extra)) paste0(base, "_", extra) else base
}

pathway_run_gsva <- function(obj, assignments, t2g, dirs, level_name) {
  if (is.null(t2g) || !nrow(t2g)) {
    pathway_note(dirs, "00_skip.txt", "No gene sets available for GSVA.")
    return(invisible(NULL))
  }
  if (!requireNamespace("GSVA", quietly = TRUE)) {
    log_msg("GSVA skip — GSVA package not installed")
    pathway_note(dirs, "00_skip.txt", "GSVA package not installed. Install Bioconductor GSVA on a machine with internet, then re-run.")
    return(invisible(NULL))
  }
  ident_col <- if ("subtype_label" %in% names(assignments)) "subtype_label" else "subtype"
  asg <- assignments
  asg$subtype <- as.character(asg[[ident_col]])
  asg <- asg[!is.na(asg$subtype) & nzchar(asg$subtype), ]
  asg <- asg[!subtype_is_noise(asg$subtype), ]
  if (!nrow(asg)) return(invisible(NULL))
  cells <- intersect(asg$cell, colnames(obj))
  if (length(cells) < 20L) return(invisible(NULL))
  dat <- get_data(obj)
  genes <- intersect(rownames(dat), unique(t2g$gene))
  if (length(genes) < 30L) {
    log_msg("GSVA skip", level_name, "- too few overlapping genes")
    return(invisible(NULL))
  }
  subst <- asg$subtype[match(cells, asg$cell)]
  lev <- natural_cluster_order(unique(subst))
  means <- sapply(lev, function(st) {
    hit <- cells[subst == st]
    if (!length(hit)) return(rep(NA_real_, length(genes)))
    Matrix::rowMeans(dat[genes, hit, drop = FALSE])
  })
  if (is.null(dim(means))) means <- matrix(means, ncol = 1, dimnames = list(genes, lev))
  rownames(means) <- genes
  colnames(means) <- lev
  means <- means[, colSums(is.finite(means)) > 0, drop = FALSE]
  if (ncol(means) < 2L) {
    log_msg("GSVA skip", level_name, "- need at least 2 subtypes")
    return(invisible(NULL))
  }
  gs <- split(t2g$gene, t2g$term)
  gs <- lapply(gs, function(g) intersect(unique(g), genes))
  gs <- gs[vapply(gs, length, integer(1)) >= 8L & vapply(gs, length, integer(1)) <= 200L]
  if (length(gs) > 80L) {
    # prefer compact sets; keep Hallmark-sized or first 80 by size mid-range
    n <- vapply(gs, length, integer(1))
    gs <- gs[order(abs(n - 40))]
    gs <- gs[seq_len(80L)]
  }
  if (!length(gs)) return(invisible(NULL))
  log_msg("GSVA", level_name, "subtypes=", ncol(means), "sets=", length(gs))
  mat <- as.matrix(means)
  scores <- tryCatch({
    if (isTRUE(exists("gsvaParam", where = asNamespace("GSVA"), inherits = FALSE))) {
      param <- GSVA::gsvaParam(exprData = mat, geneSets = gs)
      GSVA::gsva(param, verbose = FALSE)
    } else {
      GSVA::gsva(mat, gs, method = "gsva", kcdf = "Gaussian", verbose = FALSE)
    }
  }, error = function(e) {
    log_msg("GSVA failed:", conditionMessage(e))
    NULL
  })
  if (is.null(scores)) return(invisible(NULL))
  scores <- as.matrix(scores)
  tab <- data.frame(pathway = rownames(scores), scores, check.names = FALSE, stringsAsFactors = FALSE)
  pathway_write(tab, "gsva_subtype_scores.csv", dirs)
  z <- t(scale(t(scores)))
  z[!is.finite(z)] <- 0
  z <- pmax(pmin(z, 2), -2)
  if (exists("plot_module_heatmap", mode = "function")) {
    tryCatch(
      plot_module_heatmap(t(z), paste0("GSVA · ", level_name), file.path(dirs[[1]], "heatmap_gsva_subtypes")),
      error = function(e) log_msg("GSVA heatmap failed:", conditionMessage(e))
    )
    if (length(dirs) > 1L) {
      png1 <- paste0(file.path(dirs[[1]], "heatmap_gsva_subtypes"), ".png")
      pdf1 <- paste0(file.path(dirs[[1]], "heatmap_gsva_subtypes"), ".pdf")
      if (file.exists(png1)) file.copy(png1, file.path(dirs[[2]], basename(png1)), overwrite = TRUE)
      if (file.exists(pdf1)) file.copy(pdf1, file.path(dirs[[2]], basename(pdf1)), overwrite = TRUE)
    }
  }
  invisible(scores)
}

run_pathway_enrichment <- function(obj, assignments, cfg, outdir, level_name) {
  if (!pathway_enabled(cfg)) {
    log_msg("Pathway enrichment disabled for", level_name)
    return(invisible(NULL))
  }
  log_msg("Pathway enrichment", level_name)
  pathway_ensure_pkgs()
  padj <- as.numeric(pathway_cfg_value(cfg, "padj_cutoff", 0.05))
  min_deg <- as.integer(pathway_cfg_value(cfg, "min_deg", 10L))
  lfc <- as.numeric(pathway_cfg_value(cfg, "ora_lfc", deg_cfg_value(cfg, "lfc_cutoff", 0.25)))
  sets <- pathway_load_sets()
  universe <- rownames(obj)
  deg <- pathway_read_deg(outdir, level_name)
  deg_pair <- pathway_deg_pair(deg)
  ora_jobs <- pathway_jobs_from_deg(deg, min_deg, lfc, padj)

  run_db_ora <- function(db, t2g, runner) {
    dirs <- pathway_out_dirs(outdir, level_name, db)
    if (is.null(t2g) || !nrow(t2g)) {
      pathway_note(dirs, "00_skip.txt", paste(db, "gene sets were not available (install msigdbr / Bioconductor packages)."))
      return(invisible(NULL))
    }
    if (!length(ora_jobs)) {
      pathway_note(dirs, "00_skip.txt", "No DEG table or too few significant genes for ORA.")
      return(invisible(NULL))
    }
    summaries <- list()
    for (job in ora_jobs) {
      df <- runner(job$genes, t2g, universe)
      stem <- job_file_stem(job$scope, job$ident, job$direction)
      if (is.null(df) || !nrow(df)) {
        summaries[[length(summaries) + 1L]] <- data.frame(
          scope = job$scope, ident = job$ident, direction = job$direction,
          n_genes = length(job$genes), n_terms = 0L, n_sig = 0L,
          stringsAsFactors = FALSE
        )
        next
      }
      padj_col <- if ("p.adjust" %in% names(df)) df$p.adjust else df$pvalue
      n_sig <- sum(!is.na(padj_col) & padj_col < padj)
      pathway_write(df, paste0(stem, "_all_terms.csv"), dirs)
      pathway_write(df[!is.na(padj_col) & padj_col < padj, , drop = FALSE], paste0(stem, "_significant.csv"), dirs)
      summaries[[length(summaries) + 1L]] <- data.frame(
        scope = job$scope, ident = job$ident, direction = job$direction,
        n_genes = length(job$genes), n_terms = nrow(df), n_sig = as.integer(n_sig),
        stringsAsFactors = FALSE
      )
      if (identical(job$scope, "lineage") && identical(job$direction, "all")) {
        pathway_plot_ora(
          df,
          paste0(db, " ORA · ", level_name),
          file.path(dirs[[1]], paste0("bar_", stem)),
          padj
        )
        if (length(dirs) > 1L) {
          for (ext in c(".png", ".pdf")) {
            src <- paste0(file.path(dirs[[1]], paste0("bar_", stem)), ext)
            if (file.exists(src)) file.copy(src, file.path(dirs[[2]], basename(src)), overwrite = TRUE)
          }
        }
      }
    }
    if (length(summaries)) pathway_write(do.call(rbind, summaries), "00_summary.csv", dirs)
    invisible(NULL)
  }

  if (pathway_db_on(cfg, "go")) {
    log_msg("Pathway GO", level_name)
    tryCatch(
      run_db_ora("GO", sets$GO, pathway_run_ora),
      error = function(e) log_msg("ERROR pathway GO", conditionMessage(e))
    )
  }
  if (pathway_db_on(cfg, "kegg")) {
    log_msg("Pathway KEGG", level_name)
    tryCatch(
      run_db_ora("KEGG", sets$KEGG, pathway_run_ora),
      error = function(e) log_msg("ERROR pathway KEGG", conditionMessage(e))
    )
  }
  if (pathway_db_on(cfg, "reactome")) {
    log_msg("Pathway Reactome", level_name)
    tryCatch(
      run_db_ora("Reactome", sets$Reactome, function(genes, t2g, uni) {
        pathway_reactome_ora(genes, uni, t2g)
      }),
      error = function(e) log_msg("ERROR pathway Reactome", conditionMessage(e))
    )
  }
  if (pathway_db_on(cfg, "gsea")) {
    log_msg("Pathway GSEA", level_name)
    dirs <- pathway_out_dirs(outdir, level_name, "GSEA")
    ranked_jobs <- pathway_ranked_lists(deg)
    # lineage-level first; subtypes only when the gene list is large
    ranked_jobs <- Filter(function(j) {
      identical(j$scope, "lineage") || length(j$ranked) >= 200L
    }, ranked_jobs)
    gsea_sets <- list(
      Hallmark = sets$Hallmark,
      GO = sets$GO,
      KEGG = sets$KEGG,
      Reactome = sets$Reactome
    )
    if (!length(ranked_jobs)) {
      pathway_note(dirs, "00_skip.txt", "No ranked DEG list for GSEA (need Wilcoxon/MAST all-genes table).")
    } else {
      summaries <- list()
      for (job in ranked_jobs) {
        for (sn in names(gsea_sets)) {
          df <- tryCatch(pathway_run_gsea(job$ranked, gsea_sets[[sn]]), error = function(e) {
            log_msg("GSEA", sn, "failed:", conditionMessage(e))
            NULL
          })
          stem <- paste0(job_file_stem(job$scope, job$ident), "_", sn)
          if (is.null(df) || !nrow(df)) {
            summaries[[length(summaries) + 1L]] <- data.frame(
              scope = job$scope, ident = job$ident, collection = sn,
              n_genes = length(job$ranked), n_terms = 0L, n_sig = 0L,
              stringsAsFactors = FALSE
            )
            next
          }
          padj_col <- if ("p.adjust" %in% names(df)) df$p.adjust else df$pvalue
          n_sig <- sum(!is.na(padj_col) & padj_col < padj)
          pathway_write(df, paste0(stem, "_all_terms.csv"), dirs)
          pathway_write(df[!is.na(padj_col) & padj_col < padj, , drop = FALSE], paste0(stem, "_significant.csv"), dirs)
          summaries[[length(summaries) + 1L]] <- data.frame(
            scope = job$scope, ident = job$ident, collection = sn,
            n_genes = length(job$ranked), n_terms = nrow(df), n_sig = as.integer(n_sig),
            stringsAsFactors = FALSE
          )
          if (identical(job$scope, "lineage")) {
            pathway_plot_gsea(
              df, paste0("GSEA ", sn, " · ", level_name),
              file.path(dirs[[1]], paste0("nes_", stem)),
              ident.1 = deg_pair$ident.1, ident.2 = deg_pair$ident.2
            )
            if (length(dirs) > 1L) {
              for (ext in c(".png", ".pdf")) {
                src <- paste0(file.path(dirs[[1]], paste0("nes_", stem)), ext)
                if (file.exists(src)) file.copy(src, file.path(dirs[[2]], basename(src)), overwrite = TRUE)
              }
            }
          }
        }
      }
      if (length(summaries)) pathway_write(do.call(rbind, summaries), "00_summary.csv", dirs)
    }
  }
  if (pathway_db_on(cfg, "gsva")) {
    log_msg("Pathway GSVA", level_name)
    dirs <- pathway_out_dirs(outdir, level_name, "GSVA")
    t2g <- sets$Hallmark
    if (is.null(t2g) || !nrow(t2g)) t2g <- sets$GO
    tryCatch(
      pathway_run_gsva(obj, assignments, t2g, dirs, level_name),
      error = function(e) log_msg("ERROR pathway GSVA", conditionMessage(e))
    )
  }
  invisible(NULL)
}
