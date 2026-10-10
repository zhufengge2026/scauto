# Functional module heatmaps, clinical proportion tests, markdown report.

LIT_DIR_MAP <- c(
  CD4_T = "cd4",
  CD8_T_unconv = "cd8_core_functional",
  NK = "nk",
  Mono_DC = "monocyte",
  B_plasma = "bcell"
)

load_bundled_modules <- function(scauto_root, super) {
  js <- jsonlite::fromJSON(
    file.path(scauto_root, "config", "functional_modules.json"),
    simplifyVector = TRUE
  )
  js[[super]]
}

load_project_modules <- function(project, super) {
  dir <- LIT_DIR_MAP[[super]]
  if (is.null(dir)) return(NULL)
  genes_csv <- file.path(project, "resources", "literature_modules", dir, "genes.csv")
  if (!file.exists(genes_csv)) return(NULL)
  g <- utils::read.csv(genes_csv, stringsAsFactors = FALSE)
  if (!"gene_symbol" %in% names(g) && "gene" %in% names(g)) g$gene_symbol <- g$gene
  split(g$gene_symbol, g$module_id)
}

empty_lit_catalog <- function() {
  data.frame(
    lineage = character(), module_id = character(), label = character(),
    source_type = character(), year = integer(), citation_short = character(),
    title = character(), journal_year = character(), doi = character(),
    pmid = character(), species = character(), tissue = character(),
    n_genes = integer(), gene_list = character(), used_in_heatmap = logical(),
    note = character(), ref_id = character(), stringsAsFactors = FALSE
  )
}

load_extra_literature_sets <- function(scauto_root) {
  p <- file.path(scauto_root, "config", "literature_extra_gene_sets.json")
  if (!file.exists(p)) return(list())
  js <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  if (is.null(js$sets)) list() else js$sets
}

collapse_gene_list <- function(genes) {
  genes <- unique(as.character(genes))
  genes <- genes[nzchar(genes) & !is.na(genes)]
  paste(genes, collapse = ";")
}

year_from_journal <- function(x) {
  y <- suppressWarnings(as.integer(sub(".*\\b(19|20)([0-9]{2})\\b.*", "\\1\\2", x)))
  y
}

build_literature_catalog <- function(project, scauto_root) {
  refs_path <- file.path(project, "resources", "literature_modules", "references.csv")
  refs <- if (file.exists(refs_path)) {
    utils::read.csv(refs_path, stringsAsFactors = FALSE)
  } else {
    data.frame(ref_id = character(), stringsAsFactors = FALSE)
  }
  rows <- list()
  genes_long <- list()
  for (lin in names(LIT_DIR_MAP)) {
    d <- file.path(project, "resources", "literature_modules", LIT_DIR_MAP[[lin]])
    catp <- file.path(d, "catalog.csv")
    genp <- file.path(d, "genes.csv")
    if (!file.exists(catp) || !file.exists(genp)) next
    catab <- utils::read.csv(catp, stringsAsFactors = FALSE)
    gtab <- utils::read.csv(genp, stringsAsFactors = FALSE)
    if (!"gene_symbol" %in% names(gtab) && "gene" %in% names(gtab)) gtab$gene_symbol <- gtab$gene
    if (!"module_id" %in% names(catab) || !"module_id" %in% names(gtab)) next
    for (i in seq_len(nrow(catab))) {
      mid <- as.character(catab$module_id[[i]])
      gs <- unique(as.character(gtab$gene_symbol[gtab$module_id == mid]))
      gs <- gs[nzchar(gs) & !is.na(gs)]
      rid <- if ("ref_id" %in% names(catab)) as.character(catab$ref_id[[i]]) else ""
      r <- if (nzchar(rid) && "ref_id" %in% names(refs) && rid %in% refs$ref_id) {
        refs[match(rid, refs$ref_id), , drop = FALSE]
      } else {
        NULL
      }
      st <- if ("source_type" %in% names(catab)) as.character(catab$source_type[[i]]) else ""
      n <- length(gs)
      heat <- grepl("curated", st, ignore.case = TRUE) && n >= 4L && n <= 30L
      cit <- if ("citation_short" %in% names(catab)) as.character(catab$citation_short[[i]]) else ""
      lab <- if ("label" %in% names(catab)) as.character(catab$label[[i]]) else mid
      title <- if (!is.null(r) && "title" %in% names(r)) r$title[[1]] else ""
      jy <- if (!is.null(r) && "journal_year" %in% names(r)) r$journal_year[[1]] else cit
      doi <- if (!is.null(r) && "doi" %in% names(r)) r$doi[[1]] else ""
      pmid <- if (!is.null(r) && "pmid" %in% names(r)) as.character(r$pmid[[1]]) else ""
      spc <- if (!is.null(r) && "species" %in% names(r)) r$species[[1]] else "human"
      tis <- if (!is.null(r) && "tissue" %in% names(r)) r$tissue[[1]] else ""
      note <- if (!is.null(r) && "note" %in% names(r)) r$note[[1]] else ""
      yr <- year_from_journal(jy)
      if (is.na(yr)) yr <- year_from_journal(cit)
      rows[[length(rows) + 1L]] <- data.frame(
        lineage = lin, module_id = mid, label = lab, source_type = st,
        year = yr, citation_short = cit, title = title, journal_year = jy,
        doi = doi, pmid = pmid, species = spc, tissue = tis, n_genes = n,
        gene_list = collapse_gene_list(gs), used_in_heatmap = heat,
        note = note, ref_id = rid, stringsAsFactors = FALSE
      )
      if (length(gs)) {
        genes_long[[length(genes_long) + 1L]] <- data.frame(
          lineage = lin, module_id = mid, gene_symbol = gs,
          citation_short = cit, doi = doi, stringsAsFactors = FALSE
        )
      }
    }
  }
  extras <- load_extra_literature_sets(scauto_root)
  for (ex in extras) {
    gs <- unique(as.character(unlist(ex$genes)))
    gs <- gs[nzchar(gs) & !is.na(gs)]
    mid <- as.character(ex$module_id)
    lin <- as.character(ex$lineage)
    dup <- FALSE
    if (length(rows)) {
      have <- do.call(rbind, rows)
      dup <- any(have$lineage == lin & have$module_id == mid)
    }
    if (dup) next
    rows[[length(rows) + 1L]] <- data.frame(
      lineage = lin, module_id = mid,
      label = as.character(ex$label),
      source_type = "curated_panel_2016_2026",
      year = as.integer(ex$year),
      citation_short = as.character(ex$citation_short),
      title = as.character(ex$title),
      journal_year = as.character(ex$journal_year),
      doi = as.character(ex$doi),
      pmid = as.character(ex$pmid),
      species = as.character(ex$species),
      tissue = as.character(ex$tissue),
      n_genes = length(gs),
      gene_list = collapse_gene_list(gs),
      used_in_heatmap = TRUE,
      note = as.character(ex$note),
      ref_id = as.character(ex$ref_id),
      stringsAsFactors = FALSE
    )
    if (length(gs)) {
      genes_long[[length(genes_long) + 1L]] <- data.frame(
        lineage = lin, module_id = mid, gene_symbol = gs,
        citation_short = as.character(ex$citation_short),
        doi = as.character(ex$doi), stringsAsFactors = FALSE
      )
    }
  }
  catalog <- if (length(rows)) do.call(rbind, rows) else empty_lit_catalog()
  genes <- if (length(genes_long)) do.call(rbind, genes_long) else {
    data.frame(
      lineage = character(), module_id = character(), gene_symbol = character(),
      citation_short = character(), doi = character(), stringsAsFactors = FALSE
    )
  }
  catalog <- catalog[order(catalog$lineage, catalog$year, catalog$module_id, na.last = TRUE), ]
  rownames(catalog) <- NULL
  rownames(genes) <- NULL
  log_msg(
    "Literature catalog:", nrow(catalog), "gene sets;",
    sum(catalog$used_in_heatmap), "compact panels for heatmaps;",
    nrow(genes), "gene rows"
  )
  list(catalog = catalog, genes = genes)
}

heatmap_modules_from_catalog <- function(lit, super) {
  if (is.null(lit) || is.null(lit$catalog) || !nrow(lit$catalog)) return(NULL)
  catab <- lit$catalog
  catab <- catab[catab$lineage == super & catab$used_in_heatmap %in% TRUE, , drop = FALSE]
  if (!nrow(catab)) return(NULL)
  g <- lit$genes
  g <- g[g$lineage == super & g$module_id %in% catab$module_id, , drop = FALSE]
  if (!nrow(g)) return(NULL)
  mods <- split(as.character(g$gene_symbol), g$module_id)
  ord <- catab$module_id
  mods <- mods[intersect(ord, names(mods))]
  labs <- catab$label[match(names(mods), catab$module_id)]
  labs[is.na(labs) | !nzchar(labs)] <- names(mods)[is.na(labs) | !nzchar(labs)]
  names(mods) <- make.unique(as.character(labs), sep = " / ")
  mods
}

bundled_modules_as_table <- function(scauto_root) {
  js <- jsonlite::fromJSON(
    file.path(scauto_root, "config", "functional_modules.json"),
    simplifyVector = FALSE
  )
  rows <- list()
  for (lin in setdiff(names(js), "description")) {
    block <- js[[lin]]
    if (!is.list(block)) next
    for (mid in names(block)) {
      gs <- unique(as.character(unlist(block[[mid]])))
      gs <- gs[nzchar(gs)]
      rows[[length(rows) + 1L]] <- data.frame(
        lineage = lin, module_id = mid, label = mid,
        source_type = "bundled_fallback",
        n_genes = length(gs),
        gene_list = collapse_gene_list(gs),
        note = "Used only if resources/literature_modules is missing",
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(data.frame(
      lineage = character(), module_id = character(), label = character(),
      source_type = character(), n_genes = integer(), gene_list = character(),
      note = character(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

literature_export_readme <- function() {
  c(
    "# Functional modules: where they come from",
    "",
    "Auto-Pilot does **not** query PubMed or GO/KEGG while a run is in progress.",
    "Gene lists are fixed files in this project.",
    "",
    "## What the subtype heatmap uses",
    "",
    "Compact panels (typically 4–16 genes) **curated from human blood / PBMC scRNA-seq papers**",
    "(2016–2026), for example Hao/Azimuth 2021, Kotliar TCAT 2025, Crinier 2018 NK atlas,",
    "Villani 2017 DC atlas, Rebuffet 2024 NK, Terekhova 2023, Garner 2023 MAIT, Gray 2024 γδT.",
    "These rows have `source_type` containing `curated` and `used_in_heatmap = TRUE`.",
    "Citations, DOI and PMID are in `literature_gene_sets_catalog.csv`.",
    "",
    "Files on disk:",
    "",
    "- `resources/literature_modules/<lineage>/catalog.csv` + `genes.csv`",
    "- extra panels: `tools/scauto/config/literature_extra_gene_sets.json`",
    "- references: `resources/literature_modules/references.csv`",
    "",
    "## What is stored but **not** plotted on that heatmap",
    "",
    "GO Biological Process, MSigDB Hallmark, KEGG, and some MSigDB C7 immunologic signatures.",
    "`source_type = msigdb`. These are **snapshots** copied into the same folders (not a live",
    "database pull). They have tens–hundreds of genes, so the compact heatmap skips them.",
    "They are still exported here so the full set is inspectable.",
    "",
    "## Fallback",
    "",
    "If `resources/literature_modules` is missing (a copy of the helper without this project),",
    "heatmaps use `tools/scauto/config/functional_modules.json` (`bundled_fallback_panels.csv`).",
    "",
    "## Columns",
    "",
    "- `used_in_heatmap`: TRUE = compact curated panel drawn on the subtype heatmap",
    "- `source_type`: `curated_panel` / `curated_panel_2016_2026` = literature; `msigdb` = GO/Hallmark/KEGG/C7",
    "- `gene_list`: semicolon-separated symbols",
    ""
  )
}

write_lineage_module_tables <- function(lit, super, dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.null(lit) || is.null(lit$catalog) || !nrow(lit$catalog)) return(invisible(NULL))
  catab <- lit$catalog[lit$catalog$lineage == super, , drop = FALSE]
  genes <- lit$genes[lit$genes$lineage == super, , drop = FALSE]
  if (nrow(catab)) {
    write_csv(catab, file.path(dest_dir, "functional_modules_catalog.csv"))
    heat <- catab[catab$used_in_heatmap %in% TRUE, , drop = FALSE]
    write_csv(heat, file.path(dest_dir, "functional_modules_heatmap.csv"))
  }
  if (nrow(genes)) {
    write_csv(genes, file.path(dest_dir, "functional_modules_genes.csv"))
  }
  invisible(catab)
}

write_literature_exports <- function(lit, project, scauto_root, dest_tables, dest_lineage_root = NULL) {
  dir.create(dest_tables, recursive = TRUE, showWarnings = FALSE)
  write_csv(lit$catalog, file.path(dest_tables, "literature_gene_sets_catalog.csv"))
  write_csv(lit$genes, file.path(dest_tables, "literature_gene_sets_genes.csv"))
  write_csv(bundled_modules_as_table(scauto_root), file.path(dest_tables, "bundled_fallback_panels.csv"))
  refs <- file.path(project, "resources", "literature_modules", "references.csv")
  if (file.exists(refs)) {
    write_csv(
      utils::read.csv(refs, stringsAsFactors = FALSE),
      file.path(dest_tables, "literature_gene_sets_references.csv")
    )
  }
  writeLines(literature_export_readme(), file.path(dest_tables, "literature_gene_sets_README.md"))
  log_msg("Wrote", file.path(dest_tables, "literature_gene_sets_README.md"))
  if (!is.null(dest_lineage_root) && nrow(lit$catalog)) {
    for (lin in unique(as.character(lit$catalog$lineage))) {
      write_lineage_module_tables(lit, lin, file.path(dest_lineage_root, lin))
    }
  }
  invisible(dest_tables)
}

score_modules_by_subtype <- function(dat, subtype, modules, max_genes = 12L) {
  keep <- names(modules)[vapply(modules, function(g) {
    length(intersect(g, rownames(dat))) >= 3L
  }, logical(1))]
  if (!length(keep)) return(NULL)
  levs <- natural_cluster_order(unique(as.character(subtype)))
  raw <- matrix(NA_real_, nrow = length(levs), ncol = length(keep),
                dimnames = list(levs, keep))
  st <- as.character(subtype)
  for (nm in keep) {
    genes <- head(intersect(modules[[nm]], rownames(dat)), max_genes)
    sc <- Matrix::colMeans(dat[genes, , drop = FALSE])
    raw[, nm] <- vapply(levs, function(lv) mean(sc[st == lv], na.rm = TRUE), numeric(1))
  }
  z <- raw
  if (nrow(raw) >= 2L && ncol(raw) >= 1L) {
    z <- scale(raw)
    z[z > 2] <- 2
    z[z < -2] <- -2
    z[is.na(z)] <- 0
  } else {
    z[] <- 0
  }
  list(mean = raw, z = z)
}

# Reorder modules (columns) so each module's peak z sits along the
# top-left → bottom-right diagonal. Cell types (rows / x-axis) stay as given.
order_modules_along_diagonal <- function(z) {
  z <- as.matrix(z)
  if (ncol(z) < 2L || nrow(z) < 1L) return(z)
  peak_idx <- apply(z, 2L, function(v) {
    v[!is.finite(v)] <- -Inf
    which.max(v)
  })
  peak_val <- apply(z, 2L, function(v) {
    v[!is.finite(v)] <- -Inf
    max(v)
  })
  cn <- colnames(z)
  if (is.null(cn)) cn <- seq_len(ncol(z))
  z[, order(peak_idx, -peak_val, cn), drop = FALSE]
}

plot_module_heatmap <- function(z, title, stem) {
  source_palette(getOption("scauto.project"))
  z <- order_modules_along_diagonal(z)
  df <- as.data.frame(as.table(z), stringsAsFactors = FALSE)
  names(df) <- c("subtype", "module", "z")
  sub_lev <- rownames(z)
  mod_lev <- colnames(z)
  df$subtype <- factor(df$subtype, levels = sub_lev)
  df$module <- factor(df$module, levels = rev(mod_lev))
  n_sub <- max(1L, length(sub_lev))
  n_mod <- max(1L, length(mod_lev))
  max_x <- max(8L, nchar(as.character(sub_lev)), na.rm = TRUE)
  max_y <- max(10L, nchar(as.character(mod_lev)), na.rm = TRUE)
  # Left gutter for y labels; each x column wide enough for the full 45° subtype name.
  left_in <- min(7.5, max(2.2, 0.082 * max_y))
  col_in <- max(0.78, 0.072 * max_x)
  width <- left_in + n_sub * col_in + 1.7
  bottom_mm <- max(32, 2.6 * max_x)
  height <- max(5.4, 0.24 * n_mod + 0.09 * max_x + 1.5)
  p <- ggplot2::ggplot(df, ggplot2::aes(subtype, module, fill = z)) +
    ggplot2::geom_tile(colour = "black", linewidth = 0.22) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 0.15)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::scale_fill_gradientn(
      colours = pal_diverging,
      limits = c(-2, 2),
      name = "z"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::ggtitle(title) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45, hjust = 1, vjust = 1, colour = pal_ink, size = 12
      ),
      axis.text.y = ggplot2::element_text(colour = pal_ink, size = 12),
      legend.text = ggplot2::element_text(size = 13, colour = pal_ink),
      legend.title = ggplot2::element_text(size = 14, colour = pal_ink),
      plot.title = ggplot2::element_text(colour = pal_ink, hjust = 0, size = 17),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = pal_paper, colour = NA),
      plot.margin = ggplot2::margin(8, 20, bottom_mm, 8)
    ) +
    ggplot2::labs(x = NULL, y = NULL)
  save_plot(p, stem, width = width, height = height)
}

run_functional_modules <- function(obj, asg, super, scauto_root, project, outdir) {
  obj <- join_rna_if_needed(obj, Inf)
  cells <- asg$cell[asg$superlineage == super]
  cells <- intersect(cells, colnames(obj))
  if (length(cells) < 30L) return(invisible(NULL))
  dat <- get_data(obj)[, cells, drop = FALSE]
  subtype <- asg$subtype[match(cells, asg$cell)]
  if ("subtype_label" %in% names(asg)) {
    subtype <- asg$subtype_label[match(cells, asg$cell)]
  }
  lit <- getOption("scauto.literature", NULL)
  if (is.null(lit)) {
    lit <- build_literature_catalog(project, scauto_root)
  }
  mods <- heatmap_modules_from_catalog(lit, super)
  src <- "literature scRNA-seq gene sets (2016–2026)"
  if (is.null(mods) || !length(mods)) {
    mods <- load_bundled_modules(scauto_root, super)
    src <- "bundled compact panels"
  }
  mods <- mods[vapply(mods, function(g) length(unique(g)) >= 3L, logical(1))]
  if (!length(mods)) return(invisible(NULL))
  lin_dir <- file.path(outdir, "lineage", super)
  dir.create(lin_dir, recursive = TRUE, showWarnings = FALSE)
  write_lineage_module_tables(lit, super, lin_dir)
  used <- do.call(rbind, lapply(names(mods), function(nm) {
    defined <- unique(as.character(mods[[nm]]))
    present <- intersect(defined, rownames(dat))
    scored_g <- head(present, 16L)
    data.frame(
      lineage = super,
      module_label = nm,
      source = src,
      n_defined = length(defined),
      n_in_object = length(present),
      n_scored = length(scored_g),
      genes_defined = collapse_gene_list(defined),
      genes_scored = collapse_gene_list(scored_g),
      stringsAsFactors = FALSE
    )
  }))
  write_csv(used, file.path(lin_dir, "functional_modules_used.csv"))
  scored <- score_modules_by_subtype(dat, subtype, mods, max_genes = 16L)
  if (is.null(scored)) return(invisible(NULL))
  write_csv(
    data.frame(subtype = rownames(scored$mean), scored$mean, check.names = FALSE),
    file.path(lin_dir, "functional_module_means.csv")
  )
  plot_module_heatmap(
    scored$z,
    paste0(super, " functional modules x subtype", annotation_suffix(asg),
           " (", src, "; modules diagonal-aligned)"),
    file.path(lin_dir, "heatmap_functional_modules")
  )
}

prop_by_sample <- function(asg, group_col = "group", ident_col = NULL) {
  if (is.null(ident_col)) {
    ident_col <- if ("subtype_label" %in% names(asg)) "subtype_label" else "subtype"
  }
  if (!group_col %in% names(asg) && !"sample" %in% names(asg)) return(NULL)
  asg$subtype <- as.character(asg[[ident_col]])
  asg <- asg[!is.na(asg$subtype) & nzchar(asg$subtype), ]
  asg <- asg[!subtype_is_noise(asg$subtype), ]
  if (!nrow(asg)) return(NULL)
  if (!group_col %in% names(asg)) asg[[group_col]] <- "all"
  tot <- as.data.frame(table(sample = asg$sample), stringsAsFactors = FALSE)
  names(tot)[2] <- "n_total"
  cnt <- as.data.frame(table(sample = asg$sample, subtype = asg$subtype), stringsAsFactors = FALSE)
  names(cnt)[3] <- "n"
  sm <- unique(asg[, c("sample", group_col), drop = FALSE])
  out <- merge(cnt, tot, by = "sample")
  out <- merge(out, sm, by = "sample")
  out$prop <- out$n / out$n_total
  out
}

lineage_count_order <- function(x) {
  x <- unique(as.character(x))
  pref <- c(
    "CD4_T", "CD8_T", "CD8_T_unconv", "MAIT", "gdT", "NK",
    "B", "Plasma", "B_plasma",
    "CD14_Mono", "CD16_Mono", "cDC", "pDC", "Mono_DC",
    "Platelet", "RBC", "Prolif", "Other"
  )
  c(intersect(pref, x), natural_cluster_order(setdiff(x, pref)))
}

counts_to_wide <- function(long, sample_cols, key_col, value_col, key_levels) {
  if (is.null(long) || !nrow(long)) return(long)
  key_levels <- as.character(key_levels)
  key_levels <- key_levels[key_levels %in% as.character(long[[key_col]])]
  sm <- unique(long[, intersect(sample_cols, names(long)), drop = FALSE])
  sm <- sm[!duplicated(sm$sample), , drop = FALSE]
  for (k in key_levels) {
    hit <- long[as.character(long[[key_col]]) == k, , drop = FALSE]
    sm[[k]] <- as.integer(hit[[value_col]][match(sm$sample, hit$sample)])
    sm[[k]][is.na(sm[[k]])] <- 0L
  }
  num <- sm[, key_levels, drop = FALSE]
  sm$Total <- as.integer(rowSums(num, na.rm = TRUE))
  sm
}

complete_count_grid <- function(samples, keys, counts) {
  grid <- expand.grid(sample = as.character(samples), key = as.character(keys), stringsAsFactors = FALSE)
  if (is.null(counts) || !nrow(counts)) {
    grid$n <- 0L
    return(grid)
  }
  counts$sample <- as.character(counts$sample)
  counts$key <- as.character(counts$key)
  i <- match(paste(grid$sample, grid$key, sep = "\t"), paste(counts$sample, counts$key, sep = "\t"))
  grid$n <- ifelse(is.na(i), 0L, as.integer(counts$n[i]))
  grid
}

attach_sample_group <- function(df, sample_meta) {
  if (is.null(df) || !nrow(df)) return(df)
  if (!is.null(sample_meta) && "group" %in% names(sample_meta)) {
    df$group <- as.character(sample_meta$group[match(df$sample, as.character(sample_meta$sample))])
  } else if (!"group" %in% names(df)) {
    df$group <- NA_character_
  }
  df
}

write_sample_cell_count_tables <- function(atlas_asg, subtype_asg, sample_meta, outdir) {
  tab_dir <- file.path(outdir, "tables")
  dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
  samples <- character()
  if (!is.null(sample_meta) && "sample" %in% names(sample_meta)) {
    samples <- unique(as.character(sample_meta$sample))
  }
  if (!is.null(atlas_asg) && nrow(atlas_asg) && "sample" %in% names(atlas_asg)) {
    samples <- unique(c(samples, as.character(atlas_asg$sample)))
  }
  if (!is.null(subtype_asg) && nrow(subtype_asg) && "sample" %in% names(subtype_asg)) {
    samples <- unique(c(samples, as.character(subtype_asg$sample)))
  }
  samples <- samples[nzchar(samples)]
  if (!length(samples)) {
    log_msg("No samples for cell-count tables")
    return(invisible(NULL))
  }

  n_sample <- integer()
  if (!is.null(atlas_asg) && nrow(atlas_asg)) {
    n_sample <- as.integer(table(factor(as.character(atlas_asg$sample), levels = samples)))
    names(n_sample) <- samples
  } else if (!is.null(subtype_asg) && nrow(subtype_asg)) {
    n_sample <- as.integer(table(factor(as.character(subtype_asg$sample), levels = samples)))
    names(n_sample) <- samples
  }

  lin_long <- NULL
  if (!is.null(atlas_asg) && nrow(atlas_asg) && "superlineage" %in% names(atlas_asg)) {
    lin_lev <- lineage_count_order(atlas_asg$superlineage)
    raw <- as.data.frame(
      table(sample = as.character(atlas_asg$sample), key = as.character(atlas_asg$superlineage)),
      stringsAsFactors = FALSE
    )
    names(raw)[3] <- "n"
    grid <- complete_count_grid(samples, lin_lev, raw)
    lin_long <- data.frame(
      sample = grid$sample,
      superlineage = factor(grid$key, levels = lin_lev),
      n = grid$n,
      n_sample = unname(n_sample[grid$sample]),
      stringsAsFactors = FALSE
    )
    lin_long$prop_among_sample <- ifelse(lin_long$n_sample > 0, lin_long$n / lin_long$n_sample, NA_real_)
    lin_long <- attach_sample_group(lin_long, sample_meta)
    lin_long <- lin_long[order(lin_long$sample, lin_long$superlineage), ]
    lin_long$superlineage <- as.character(lin_long$superlineage)
    write_csv(lin_long, file.path(tab_dir, "sample_lineage_cell_counts.csv"))
    wide <- counts_to_wide(
      lin_long, c("sample", "group"), "superlineage", "n", lin_lev
    )
    write_csv(wide, file.path(tab_dir, "sample_lineage_cell_counts_wide.csv"))
  }

  if (!is.null(atlas_asg) && nrow(atlas_asg) && "lineage" %in% names(atlas_asg)) {
    al_lev <- lineage_count_order(atlas_asg$lineage)
    raw <- as.data.frame(
      table(sample = as.character(atlas_asg$sample), key = as.character(atlas_asg$lineage)),
      stringsAsFactors = FALSE
    )
    names(raw)[3] <- "n"
    grid <- complete_count_grid(samples, al_lev, raw)
    al_long <- data.frame(
      sample = grid$sample,
      lineage = factor(grid$key, levels = al_lev),
      n = grid$n,
      n_sample = unname(n_sample[grid$sample]),
      stringsAsFactors = FALSE
    )
    al_long$prop_among_sample <- ifelse(al_long$n_sample > 0, al_long$n / al_long$n_sample, NA_real_)
    al_long <- attach_sample_group(al_long, sample_meta)
    al_long <- al_long[order(al_long$sample, al_long$lineage), ]
    al_long$lineage <- as.character(al_long$lineage)
    write_csv(al_long, file.path(tab_dir, "sample_atlas_lineage_cell_counts.csv"))
    write_csv(
      counts_to_wide(al_long, c("sample", "group"), "lineage", "n", al_lev),
      file.path(tab_dir, "sample_atlas_lineage_cell_counts_wide.csv")
    )
  }

  if (is.null(subtype_asg) || !nrow(subtype_asg)) {
    log_msg("Wrote sample lineage cell-count tables")
    return(invisible(lin_long))
  }
  lab_col <- if ("subtype_label" %in% names(subtype_asg)) "subtype_label" else "subtype"
  st_asg <- subtype_asg
  st_asg$sample <- as.character(st_asg$sample)
  st_asg$superlineage <- as.character(st_asg$superlineage)
  st_asg$subtype_label <- as.character(st_asg[[lab_col]])
  if ("subtype_cluster" %in% names(st_asg)) {
    st_asg$subtype_cluster <- as.character(st_asg$subtype_cluster)
  } else {
    st_asg$subtype_cluster <- st_asg$subtype_label
  }
  st_asg$subtype <- if ("subtype" %in% names(st_asg)) as.character(st_asg$subtype) else st_asg$subtype_label
  map <- unique(st_asg[, c("superlineage", "subtype_cluster", "subtype", "subtype_label"), drop = FALSE])
  lin_lev <- lineage_count_order(map$superlineage)
  map <- map[order(match(map$superlineage, lin_lev), cluster_tail_number(map$subtype_cluster), map$subtype_label), ]
  map$key <- paste(map$superlineage, map$subtype_label, sep = " | ")
  raw <- as.data.frame(
    table(
      sample = st_asg$sample,
      key = paste(st_asg$superlineage, st_asg$subtype_label, sep = " | ")
    ),
    stringsAsFactors = FALSE
  )
  names(raw)[3] <- "n"
  grid <- complete_count_grid(samples, map$key, raw)
  i <- match(grid$key, map$key)
  n_lin <- if (!is.null(lin_long)) {
    lin_long$n[match(paste(grid$sample, map$superlineage[i], sep = "\t"),
                     paste(lin_long$sample, lin_long$superlineage, sep = "\t"))]
  } else {
    NA_integer_
  }
  st_long <- data.frame(
    sample = grid$sample,
    superlineage = map$superlineage[i],
    subtype_cluster = map$subtype_cluster[i],
    subtype = map$subtype[i],
    subtype_label = map$subtype_label[i],
    n = grid$n,
    n_lineage = as.integer(n_lin),
    n_sample = unname(n_sample[grid$sample]),
    stringsAsFactors = FALSE
  )
  st_long$prop_within_lineage <- ifelse(st_long$n_lineage > 0, st_long$n / st_long$n_lineage, NA_real_)
  st_long$prop_among_sample <- ifelse(st_long$n_sample > 0, st_long$n / st_long$n_sample, NA_real_)
  st_long <- attach_sample_group(st_long, sample_meta)
  st_long$superlineage <- factor(st_long$superlineage, levels = lin_lev)
  st_long <- st_long[order(st_long$sample, st_long$superlineage, cluster_tail_number(st_long$subtype_cluster)), ]
  st_long$superlineage <- as.character(st_long$superlineage)
  write_csv(st_long, file.path(tab_dir, "sample_subtype_cell_counts.csv"))
  st_long$key <- paste(st_long$superlineage, st_long$subtype_label, sep = " | ")
  write_csv(
    counts_to_wide(st_long, c("sample", "group"), "key", "n", map$key),
    file.path(tab_dir, "sample_subtype_cell_counts_wide.csv")
  )
  st_long$key <- NULL
  for (lv in unique(st_long$superlineage)) {
    lin_dir <- file.path(outdir, "lineage", lv)
    dir.create(lin_dir, recursive = TRUE, showWarnings = FALSE)
    sub <- st_long[st_long$superlineage == lv, , drop = FALSE]
    write_csv(sub, file.path(lin_dir, "sample_subtype_cell_counts.csv"))
    write_csv(
      counts_to_wide(sub, c("sample", "group"), "subtype_label", "n", unique(sub$subtype_label)),
      file.path(lin_dir, "sample_subtype_cell_counts_wide.csv")
    )
  }
  log_msg("Wrote sample lineage and subtype cell-count tables")
  invisible(st_long)
}

sig_stars <- function(p) {
  p <- as.numeric(p)
  out <- rep("", length(p))
  out[!is.na(p) & p < 0.05] <- "*"
  out[!is.na(p) & p < 0.01] <- "**"
  out[!is.na(p) & p < 0.001] <- "***"
  out
}

order_clinical_groups <- function(groups) {
  groups <- unique(as.character(groups))
  groups <- groups[nzchar(groups) & !is.na(groups)]
  pref <- c("GroupB", "GroupA", "Baseline", "Followup")
  c(intersect(pref, groups), sort(setdiff(groups, pref)))
}

annotate_prop_stats <- function(tab) {
  if (is.null(tab) || !nrow(tab)) return(tab)
  if (!"p_value" %in% names(tab) && "p_wilcox" %in% names(tab)) {
    tab$p_value <- tab$p_wilcox
  }
  if (!"p_adj_BH" %in% names(tab)) {
    tab$p_adj_BH <- stats::p.adjust(tab$p_value, method = "BH")
  }
  tab$sig_raw <- sig_stars(tab$p_value)
  tab$sig_adj <- sig_stars(tab$p_adj_BH)
  tab$significant_nominal_0.05 <- !is.na(tab$p_value) & tab$p_value < 0.05
  tab$significant_FDR_0.05 <- !is.na(tab$p_adj_BH) & tab$p_adj_BH < 0.05
  tab
}

se_safe <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(0)
  stats::sd(x) / sqrt(length(x))
}

wilcox_props <- function(pr, group_col, min_n) {
  groups <- order_clinical_groups(pr[[group_col]])
  if (length(groups) != 2L) return(NULL)
  res <- lapply(sort(unique(as.character(pr$subtype))), function(st) {
    d <- pr[pr$subtype == st, ]
    g1 <- d$prop[d[[group_col]] == groups[1]]
    g2 <- d$prop[d[[group_col]] == groups[2]]
    row <- data.frame(
      subtype = st, test = "wilcoxon_sample_proportions",
      group1 = groups[1], group2 = groups[2],
      n1 = length(g1), n2 = length(g2),
      mean1 = mean(g1), mean2 = mean(g2),
      se1 = se_safe(g1), se2 = se_safe(g2),
      delta_mean = mean(g2) - mean(g1),
      p_wilcox = NA_real_, stringsAsFactors = FALSE
    )
    if (length(g1) >= min_n && length(g2) >= min_n) {
      row$p_wilcox <- tryCatch(
        stats::wilcox.test(g1, g2, exact = FALSE)$p.value,
        error = function(e) NA_real_
      )
    }
    row
  })
  tab <- do.call(rbind, res)
  tab$p_value <- tab$p_wilcox
  tab$p_adj_BH <- stats::p.adjust(tab$p_wilcox, method = "BH")
  annotate_prop_stats(tab)
}

kruskal_props <- function(pr, group_col, min_n) {
  groups <- order_clinical_groups(pr[[group_col]])
  if (length(groups) < 3L) return(NULL)
  res <- lapply(sort(unique(as.character(pr$subtype))), function(st) {
    d <- pr[pr$subtype == st, ]
    n_ok <- vapply(groups, function(g) sum(d[[group_col]] == g), integer(1))
    p <- NA_real_
    if (all(n_ok >= min_n)) {
      p <- tryCatch(
        stats::kruskal.test(d$prop ~ factor(d[[group_col]], levels = groups))$p.value,
        error = function(e) NA_real_
      )
    }
    data.frame(
      subtype = st, test = "kruskal_wallis_sample_proportions",
      groups = paste(groups, collapse = ";"),
      n_groups = length(groups),
      p_value = p, stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, res)
  tab$p_adj_BH <- stats::p.adjust(tab$p_value, method = "BH")
  annotate_prop_stats(tab)
}

prop_group_stats <- function(pr, group_col, min_n) {
  n_grp <- length(order_clinical_groups(pr[[group_col]]))
  if (n_grp == 2L) wilcox_props(pr, group_col, min_n) else kruskal_props(pr, group_col, min_n)
}

merge_prop_stats <- function(pr, wt) {
  if (is.null(wt) || !nrow(wt)) return(pr)
  keep <- intersect(
    c(
      "subtype", "test", "group1", "group2", "mean1", "mean2", "delta_mean",
      "p_wilcox", "p_value", "p_adj_BH", "sig_raw", "sig_adj",
      "significant_nominal_0.05", "significant_FDR_0.05"
    ),
    names(wt)
  )
  merge(pr, wt[, keep, drop = FALSE], by = "subtype", all.x = TRUE, sort = FALSE)
}

fisher_or <- function(asg, group_col, ref = NULL) {
  groups <- unique(as.character(asg[[group_col]]))
  if (length(groups) < 2L) return(NULL)
  if (is.null(ref) || !nzchar(ref) || !ref %in% groups) ref <- groups[1]
  alt <- setdiff(groups, ref)
  if (length(alt) != 1L) {
    # pairwise vs reference
  }
  focus <- if (length(alt) == 1L) alt else alt[1]
  asg <- asg[!is.na(asg$subtype), ]
  asg$Outcome_group <- factor(asg[[group_col]], levels = c(ref, focus))
  rows <- lapply(sort(unique(asg$subtype)), function(st) {
    is_st <- factor(asg$subtype == st, levels = c(FALSE, TRUE))
    ct <- table(asg$Outcome_group, is_st)
    if (nrow(ct) < 2 || ncol(ct) < 2) return(NULL)
    ft <- tryCatch(stats::fisher.test(ct), error = function(e) NULL)
    if (is.null(ft)) return(NULL)
    data.frame(
      subtype = st,
      reference = ref,
      focus = focus,
      Yes_focus = as.integer(ct[focus, "TRUE"]),
      No_focus = as.integer(ct[focus, "FALSE"]),
      Yes_ref = as.integer(ct[ref, "TRUE"]),
      No_ref = as.integer(ct[ref, "FALSE"]),
      OR = unname(ft$estimate),
      Lower_CI = ft$conf.int[1],
      Upper_CI = ft$conf.int[2],
      P_value = ft$p.value,
      stringsAsFactors = FALSE
    )
  })
  tab <- do.call(rbind, rows)
  if (is.null(tab) || !nrow(tab)) return(NULL)
  tab$P_adj_BH <- stats::p.adjust(tab$P_value, method = "BH")
  tab$sig_raw <- sig_stars(tab$P_value)
  tab$sig_adj <- sig_stars(tab$P_adj_BH)
  tab$significant_nominal_0.05 <- !is.na(tab$P_value) & tab$P_value < 0.05
  tab$significant_FDR_0.05 <- !is.na(tab$P_adj_BH) & tab$P_adj_BH < 0.05
  tab
}

plot_prop_bars <- function(pr, group_col, stem, title, stats = NULL) {
  source_palette(getOption("scauto.project"))
  pr$grp <- as.character(pr[[group_col]])
  pal <- pal_for(unique(pr$grp))
  percent_lab <- function(x) paste0(round(100 * x), "%")
  xlev <- natural_cluster_order(unique(as.character(pr$subtype)))
  pr$subtype <- factor(as.character(pr$subtype), levels = xlev)
  y_max <- max(pr$prop, na.rm = TRUE)
  if (!is.finite(y_max) || y_max <= 0) y_max <- 0.05
  pad <- max(0.04, y_max * 0.12)
  p <- ggplot2::ggplot(pr, ggplot2::aes(subtype, prop, fill = grp)) +
    ggplot2::stat_summary(fun = mean, geom = "col", position = ggplot2::position_dodge(0.7), width = 0.65, colour = NA) +
    ggplot2::geom_point(
      ggplot2::aes(colour = grp),
      position = ggplot2::position_jitterdodge(jitter.width = 0.08, dodge.width = 0.7),
      size = 1.6, alpha = 0.9
    ) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::scale_colour_manual(values = pal, guide = "none") +
    ggplot2::scale_y_continuous(labels = percent_lab, expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::ggtitle(title) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = pal_ink),
      plot.title = ggplot2::element_text(colour = pal_ink, hjust = 0),
      plot.caption = ggplot2::element_text(colour = pal_ink, hjust = 0, size = 8),
      plot.background = ggplot2::element_rect(fill = pal_paper, colour = NA)
    ) +
    ggplot2::labs(x = NULL, y = "Proportion of lineage (per sample)", fill = NULL)
  if (!is.null(stats) && nrow(stats)) {
    p <- p + ggplot2::labs(
      caption = "Wilcoxon (or Kruskal–Wallis) on per-sample proportions. * p<0.05, ** p<0.01, *** p<0.001 (unadjusted). BH-FDR in the CSV."
    )
    pcol <- if ("p_value" %in% names(stats)) stats$p_value else stats$p_wilcox
    stars <- if ("sig_raw" %in% names(stats)) as.character(stats$sig_raw) else sig_stars(pcol)
    ymax_st <- vapply(xlev, function(st) {
      m <- max(pr$prop[as.character(pr$subtype) == st], na.rm = TRUE)
      if (!is.finite(m)) 0 else m
    }, numeric(1))
    names(ymax_st) <- xlev
    ann <- data.frame(
      subtype = factor(as.character(stats$subtype), levels = xlev),
      y_br = unname(ymax_st[as.character(stats$subtype)]) + pad * 0.45,
      y_tx = unname(ymax_st[as.character(stats$subtype)]) + pad * 1.15,
      lab = stars,
      show = !is.na(pcol) & pcol < 0.05 & nzchar(stars),
      stringsAsFactors = FALSE
    )
    ann <- ann[ann$show %in% TRUE, , drop = FALSE]
    if (nrow(ann)) {
      p <- p +
        ggplot2::geom_segment(
          data = ann,
          ggplot2::aes(x = as.numeric(subtype) - 0.22, xend = as.numeric(subtype) + 0.22,
                       y = y_br, yend = y_br),
          inherit.aes = FALSE, colour = pal_ink, linewidth = 0.45
        ) +
        ggplot2::geom_text(
          data = ann,
          ggplot2::aes(x = subtype, y = y_tx, label = lab),
          inherit.aes = FALSE, colour = pal_ink, size = 5, fontface = "bold", vjust = 0
        ) +
        ggplot2::coord_cartesian(
          ylim = c(0, max(ann$y_tx, y_max, na.rm = TRUE) + pad * 0.4),
          clip = "off"
        )
    }
  }
  save_plot(p, stem, width = max(8, 0.55 * length(xlev) + 3), height = 6.4)
}

plot_or_forest <- function(or_tab, stem, title) {
  source_palette(getOption("scauto.project"))
  d <- or_tab
  if (is.null(d) || !nrow(d)) return(invisible(NULL))
  cap_lo <- 0.05
  cap_hi <- 20
  star <- if ("sig_adj" %in% names(d)) as.character(d$sig_adj) else {
    if ("sig_raw" %in% names(d)) as.character(d$sig_raw) else rep("", nrow(d))
  }
  star[is.na(star)] <- ""
  d$label <- ifelse(nzchar(star), paste0(as.character(d$subtype), " ", star), as.character(d$subtype))
  or_num <- as.numeric(d$OR)
  lo <- as.numeric(d$Lower_CI)
  hi <- as.numeric(d$Upper_CI)
  or_plot <- or_num
  or_plot[is.infinite(or_num) & or_num > 0] <- cap_hi
  or_plot[!is.finite(or_plot) | or_plot <= 0] <- cap_lo
  d$OR_plot <- pmin(pmax(or_plot, cap_lo), cap_hi)
  lo[!is.finite(lo) | lo <= 0] <- cap_lo
  hi[!is.finite(hi)] <- cap_hi
  d$Lower_CI <- pmax(pmin(lo, d$OR_plot), cap_lo)
  d$Upper_CI <- pmin(pmax(hi, d$OR_plot), cap_hi)
  # Left → right: increasing OR (weaker to stronger association with focus).
  ord <- order(d$OR_plot, d$label)
  xlev <- unique(as.character(d$label[ord]))
  d$label <- factor(d$label, levels = xlev)
  focus <- as.character(d$focus[[1]])
  ref <- as.character(d$reference[[1]])
  col_focus <- if (focus %in% names(pal_clinical)) unname(pal_clinical[[focus]]) else "#1F3A5F"
  col_ref <- if (ref %in% names(pal_clinical)) unname(pal_clinical[[ref]]) else "#C45C26"
  d$enrich <- ifelse(d$OR_plot > 1, paste0(focus, " enriched"), paste0(ref, " enriched"))
  d$enrich <- factor(
    d$enrich,
    levels = c(paste0(focus, " enriched"), paste0(ref, " enriched"))
  )
  cols <- stats::setNames(
    c(col_focus, col_ref),
    c(paste0(focus, " enriched"), paste0(ref, " enriched"))
  )
  n_cl <- length(xlev)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = label, y = OR_plot, colour = enrich)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", colour = pal_ink, linewidth = 0.45) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = Lower_CI, ymax = Upper_CI),
      width = if (n_cl <= 3) 0.12 else 0.18,
      linewidth = 0.6
    ) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_colour_manual(values = cols, drop = FALSE) +
    ggplot2::scale_y_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10, 20)) +
    ggplot2::coord_cartesian(ylim = c(cap_lo, cap_hi), clip = "off") +
    ggplot2::ggtitle(title) +
    ggplot2::labs(
      x = NULL,
      y = paste0("Odds ratio (", focus, " vs ", ref, "; log scale)"),
      colour = NULL,
      caption = "Stars on labels: BH-FDR < 0.05 (Fisher exact, cells pooled)."
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, colour = pal_ink),
      plot.title = ggplot2::element_text(colour = pal_ink, hjust = 0),
      plot.caption = ggplot2::element_text(colour = pal_ink, hjust = 0, size = 8),
      plot.background = ggplot2::element_rect(fill = pal_paper, colour = NA),
      legend.position = "top",
      legend.justification = "left"
    )
  save_plot(p, stem, width = max(7, 0.72 * n_cl + 3.4), height = 5.8)
}

annotation_suffix <- function(asg) {
  m <- unique(na.omit(as.character(asg$annotation_method)))
  if (length(m) == 1L && identical(m, "cassia")) " (CASSIA)" else ""
}

run_clinical <- function(obj, assignments, cfg, outdir, level_name) {
  spec <- scauto_group_contrasts(cfg, obj)
  for (gc in spec$all) {
    is_primary <- identical(gc, spec$primary)
    clin_dir <- file.path(outdir, "clinical", level_name)
    if (!is_primary) clin_dir <- file.path(clin_dir, paste0("by_", gc))
    tryCatch(
      run_clinical_one(
        obj, assignments, cfg, outdir, level_name,
        group_col = gc, clin_dir = clin_dir, copy_to_lineage = is_primary
      ),
      error = function(e) log_msg("ERROR clinical", level_name, gc, conditionMessage(e))
    )
  }
  invisible(NULL)
}

run_clinical_one <- function(obj, assignments, cfg, outdir, level_name,
                             group_col, clin_dir, copy_to_lineage = TRUE) {
  asg <- assignments
  ident_col <- if ("subtype_label" %in% names(asg)) "subtype_label" else "subtype"
  asg$subtype <- as.character(asg[[ident_col]])
  if (group_col %in% colnames(obj[[]])) {
    asg[[group_col]] <- obj[[group_col]][match(asg$cell, colnames(obj)), 1]
  } else {
    asg[[group_col]] <- "all"
  }
  asg$sample <- obj$sample[match(asg$cell, colnames(obj))]
  dir.create(clin_dir, recursive = TRUE, showWarnings = FALSE)
  pr <- prop_by_sample(asg, group_col, ident_col = "subtype")
  if (is.null(pr) || !nrow(pr)) return(invisible(NULL))
  bar_title <- paste0(level_name, " secondary-cluster proportions", annotation_suffix(asg))
  if (!identical(unique(as.character(pr[[group_col]])), "all")) {
    bar_title <- paste0(bar_title, " by ", group_col)
  }
  min_n <- as.integer(cfg$clinical$min_samples_per_group)
  if (!is.finite(min_n) || min_n < 2L) min_n <- 2L
  n_grp <- length(order_clinical_groups(pr[[group_col]]))
  wt <- NULL
  if (n_grp >= 2L) {
    wt <- prop_group_stats(pr, group_col, min_n)
  }
  pr_out <- merge_prop_stats(pr, wt)
  write_csv(pr_out, file.path(clin_dir, "sample_subtype_proportions.csv"))
  plot_prop_bars(pr, group_col, file.path(clin_dir, "proportion_bars"), bar_title, stats = wt)
  if (isTRUE(copy_to_lineage) && !identical(level_name, "atlas_lineage")) {
    lin_dir <- file.path(outdir, "lineage", level_name)
    dir.create(lin_dir, recursive = TRUE, showWarnings = FALSE)
    write_csv(pr_out, file.path(lin_dir, "sample_subtype_proportions.csv"))
    plot_prop_bars(pr, group_col, file.path(lin_dir, "proportion_bars"), bar_title, stats = wt)
  }
  if (n_grp >= 2L) {
    if (!is.null(wt)) {
      write_csv(wt, file.path(clin_dir, "wilcoxon_sample_proportions.csv"))
      if (isTRUE(copy_to_lineage) && !identical(level_name, "atlas_lineage")) {
        write_csv(wt, file.path(outdir, "lineage", level_name, "sample_subtype_proportion_stats.csv"))
      }
    }
    or_tab <- fisher_or(asg, group_col, cfg$clinical$reference_group)
    if (!is.null(or_tab)) {
      write_csv(or_tab, file.path(clin_dir, "fisher_odds_ratios.csv"))
      n_samp <- length(unique(asg$sample))
      note <- if (n_samp < 4L) " (exploratory; few samples — cells are not biological replicates)" else ""
      plot_or_forest(
        or_tab,
        file.path(clin_dir, "odds_ratio_forest"),
        paste0(level_name, " Fisher OR", annotation_suffix(asg), note)
      )
    }
  } else {
    log_msg("One clinical group; Wilcoxon/Fisher skipped for", level_name, group_col)
  }
}

write_report <- function(outdir, manifest, qc_tbl, lineage_tab) {
  lines <- c(
    paste0("# PBMC Auto-Pilot report: ", manifest$project_name),
    "",
    paste0("Generated: ", manifest$finished),
    paste0("Compute mode: **", manifest$compute_mode, "** — ", manifest$compute_note),
    paste0("Cells after QC: ", manifest$n_qc, " / raw ", manifest$n_raw),
    paste0("RSS peak (last logged): ", manifest$rss_gb_end, " GB"),
    "",
    "## Inputs",
    paste0("- 10x root: `", manifest$input, "`"),
    paste0("- samples: ", paste(manifest$samples, collapse = ", ")),
    paste0("- metadata: `", manifest$meta %||% "none", "`"),
    "",
    "## QC by sample",
    "",
    paste0("See `tables/qc_by_sample.csv` and `figures/qc_violins_by_sample.png`."),
    "",
    "## Atlas clustering",
    "",
    paste0("- HVG ", manifest$n_hvg, "; PCA ", manifest$npcs, "; Leiden res=", manifest$leiden_res),
    paste0("- clusters: ", manifest$n_clusters),
    "- full biomarkers: `tables/atlas_findallmarkers.csv`",
    "- lineage calls: `tables/atlas_cluster_lineage.csv`",
    "",
    "## Lineage (deliverable 2)",
    ""
  )
  if (!is.null(lineage_tab) && nrow(lineage_tab)) {
    lines <- c(
      lines,
      paste0("- superlineages: ", paste(unique(lineage_tab$superlineage), collapse = ", ")),
      "- cell counts by sample × lineage and sample × subtype: `tables/sample_lineage_cell_counts.csv`, `tables/sample_subtype_cell_counts.csv` (wide Excel copies: `*_wide.csv`; per lineage: `lineage/<name>/sample_subtype_cell_counts.csv`)"
    )
  }
  lines <- c(
    lines,
    "",
    "## Secondary clustering (deliverable 3–5)",
    "",
    "Per-lineage folders under `lineage/`: secondary clusters (`c0`, `c1`, …), subtype labels (CASSIA when the `cassia` CLI and an agent backend are available; otherwise compact marker panels), diagonal DotPlot (black point outlines; MT/RB excluded), functional module heatmap (cell types on x, modules on y ordered so peak tiles follow the diagonal; compact tiles; literature scRNA-seq gene sets), and subtype proportion bars. CASSIA CSV/HTML live in `lineage/<name>/cassia/`. Proportions and DEGs use the CASSIA names and drop doublet/contaminant/leak clusters.",
    "",
    "Subtype biomarkers (FindAllMarkers within each lineage): every tested gene per subtype in `lineage/<name>/markers/subtype_*_all.csv`, plus the top 100 positive markers (`avg_log2FC > 0`, ranked by adjusted P then log2FC) in `subtype_*_top100.csv`. Combined tables: `lineage/<name>/markers/00_all_genes.csv`, `00_top100.csv`, `tables/all_lineage_subtype_markers_all.csv`, and `tables/all_lineage_subtype_markers_top100.csv`.",
    "",
    "Literature gene-set catalog (all lineages): `tables/literature_gene_sets_catalog.csv` (citation, DOI, PMID, genes, heatmap flag). Long gene table: `tables/literature_gene_sets_genes.csv`. How modules were chosen: `tables/literature_gene_sets_README.md`. Per lineage: `lineage/<name>/functional_modules_catalog.csv`, `functional_modules_heatmap.csv` (what the figure uses), `functional_modules_genes.csv`, `functional_modules_used.csv` (genes actually scored).",
    "",
    "## Clinical groups (deliverable 6)",
    "",
    "Sample-level proportions, Wilcoxon (if ≥2 samples/group; Kruskal–Wallis if >2 groups) with BH-FDR and * / ** / *** on proportion bars when unadjusted p<0.05, plus Fisher OR forest (cell types on the x-axis, FDR-significant subtypes starred) under `clinical/` and `lineage/<name>/`. Extra clinical group columns from the Groups page are under `clinical/<lineage>/by_<column>/`.",
    "",
    "If Advanced Settings imported sample numeric columns (Age, LabValue1, …), Spearman correlation of each subtype’s within-lineage proportion vs those numbers is under `clinical/<lineage>/clinical_numeric/` (heatmap: rho in tiles, uncorrected stars, BH-FDR in the CSV).",
    "",
    "DEGs between the two clinical groups: Wilcoxon rank-sum and MAST hurdle, each saved once under `clinical/<lineage>/deg_wilcoxon/` and `deg_MAST/` (whole lineage and every subtype), with volcano plots (positive log2FC = higher in the focus group, e.g. GroupA).",
    "",
    "## Pathway enrichment",
    "",
    "GO, KEGG, GSEA, GSVA, and Reactome are each written once under `clinical/<lineage>/pathway/<DB>/`. ORA and GSEA use clinical DEGs (Wilcoxon when present). GSVA scores Hallmark (or GO) sets on subtype-mean expression. A missing Bioconductor package skips that database only.",
    "",
    "## Trajectory (Advanced Settings)",
    "",
    "CytoTRACE2 and Monocle3 run only when Advanced Settings enables them and at least one lineage is ticked. Results: `lineage/<name>/trajectory/`.",
    "",
    "## How to open",
    "",
    "CSV in Excel; PNG/PDF in any image viewer. No internet and no extra software required after this folder is copied.",
    ""
  )
  p <- file.path(outdir, "REPORT.md")
  writeLines(lines, p)
  log_msg("Wrote", p)
}

`%||%` <- function(a, b) if (is.null(a) || !nzchar(a)) b else a
