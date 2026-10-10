# Editorial Ink (scheme D) — PBMC analyses
# Source of truth: viz/editorial_ink.json
# Optional light variant: SCAUTO_PALETTE=light → viz/editorial_ink_light.json
# Or SCAUTO_PALETTE_FILE=/path/to.json

.editorial_ink_json <- function() {
  override <- Sys.getenv("SCAUTO_PALETTE_FILE", "")
  if (nzchar(override) && file.exists(override)) return(override)
  variant <- tolower(Sys.getenv("SCAUTO_PALETTE", "default"))
  fname <- if (variant %in% c("light", "d-light", "editorial_ink_light")) {
    "editorial_ink_light.json"
  } else {
    "editorial_ink.json"
  }
  candidates <- c(
    file.path(Sys.getenv("PROJECT_DIR", ""), "viz", fname),
    file.path(Sys.getenv("LOCAL_PROJECT_DIR", ""), "viz", fname),
    file.path(getwd(), "viz", fname)
  )
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) {
    # fall back to default editorial ink
    candidates2 <- gsub("editorial_ink_light", "editorial_ink", candidates)
    hit <- candidates2[file.exists(candidates2)][1]
  }
  hit
}

.load_palette_json <- function() {
  path <- .editorial_ink_json()
  if (is.na(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE), error = function(e) NULL)
}

.pal_cfg <- .load_palette_json()

pal_ink <- if (!is.null(.pal_cfg$ink)) .pal_cfg$ink else "#2B2B2B"
pal_paper <- if (!is.null(.pal_cfg$paper)) .pal_cfg$paper else "#FFFFFF"

pal_clinical <- if (!is.null(.pal_cfg$clinical)) {
  unlist(.pal_cfg$clinical)
} else {
  c(
    GroupA = "#1F3A5F", GroupB = "#C45C26",
    Baseline = "#4F7A62", Followup = "#C4A36A"
  )
}

pal_lineage <- if (!is.null(.pal_cfg$lineage)) {
  x <- unlist(.pal_cfg$lineage)
  # aliases
  c(x,
    `CD4 T` = unname(x["CD4_T"]),
    `CD8 T` = unname(x["CD8_T"]),
    `CD14 Mono` = unname(x["CD14_Mono"]),
    `CD16 Mono` = unname(x["CD16_Mono"]),
    `MAIT / gdT` = unname(x["MAIT"])
  )
} else {
  c(
    CD4_T = "#1F3A5F", `CD4 T` = "#1F3A5F",
    CD8_T = "#4F7CAC", `CD8 T` = "#4F7CAC",
    NK = "#2F5A44",
    B = "#C45C26",
    Plasma = "#7A3E4A",
    CD14_Mono = "#C4A36A", `CD14 Mono` = "#C4A36A",
    CD16_Mono = "#E0C8A8", `CD16 Mono` = "#E0C8A8",
    cDC = "#D4B896", pDC = "#8A6E4F",
    MAIT = "#6B5B95", gdT = "#8B7BB8", `MAIT / gdT` = "#6B5B95",
    Platelet = "#B8B2A7", RBC = "#A33B3B", Prolif = "#9A3B12"
  )
}

pal_cluster26 <- if (!is.null(.pal_cfg$cluster26)) {
  as.character(.pal_cfg$cluster26)
} else {
  c(
    "#1F3A5F", "#4F7CAC", "#A7C4D8", "#C45C26", "#E09A6A", "#7A3E4A",
    "#4F7A62", "#5E8C6A", "#A3C9A8", "#D4B896", "#8A6E4F", "#6B5B95",
    "#8B7BB8", "#B8B2A7", "#9A3B12", "#C4A36A", "#2B2B2B", "#5C6B73",
    "#3D5A80", "#B44A1C", "#8F5A3A", "#2F5A44", "#7A9E82", "#C9A882",
    "#5A4A7A", "#A33B3B"
  )
}

pal_sequential <- if (!is.null(.pal_cfg$sequential)) {
  as.character(.pal_cfg$sequential)
} else {
  c("#F5EDE4", "#E0C4A8", "#C45C26", "#8A3A14", "#4A1C08")
}
pal_diverging <- if (!is.null(.pal_cfg$diverging)) {
  as.character(.pal_cfg$diverging)
} else {
  c("#1F3A5F", "#8FA4BE", "#F5F2EB", "#E09A6A", "#C45C26")
}

.pal_lineage_rules <- list(
  Plasma = "plasma|plasmablast",
  B = "\\bb cell|\\bb-|exhausted b|^b$|b naive|b memory|b intermediate|ms4a1",
  CD4_T = "cd4|t helper|treg|tcm|tem|t naive",
  CD8_T = "cd8|cytotoxic",
  NK = "\\bnk\\b|natural killer",
  CD16_Mono = "cd16|non-classical|nonclassical|fcgr3",
  CD14_Mono = "cd14|classical mono|monocyte",
  pDC = "pdc|plasmacytoid",
  cDC = "cdc|myeloid dendritic|\\bdc\\b|dendritic",
  MAIT = "mait",
  gdT = "gd t|gdt|gamma.?delta|vd2",
  Platelet = "platelet|megakary",
  RBC = "eryth|rbc|\\bhb[ab]",
  Prolif = "prolif|cycling|g2m|mki67"
)

# Secondary-cluster labels must recycle cluster26 — never collapse via grepl
# lineage rules (e.g. "c0 · CD4_TCM" contains "cd4"; "NK_CD16dim" contains "cd16").
is_cluster26_label <- function(x) {
  x <- as.character(x)
  if (is.na(x) || !nzchar(x)) return(FALSE)
  grepl("^[0-9]+$", x) ||
    grepl("_c[0-9]+", x) ||
    grepl("(^|[ ·])c[0-9]+(\\b| ·|$)", x) ||
    grepl(" · ", x, fixed = TRUE)
}

pal_match_one <- function(label) {
  x <- as.character(label)
  if (is.na(x) || !nzchar(x)) return(NA_character_)
  if (x %in% names(pal_clinical)) return(unname(pal_clinical[[x]]))
  if (x %in% names(pal_lineage)) return(unname(pal_lineage[[x]]))
  if (is_cluster26_label(x)) return(NA_character_)
  xl <- tolower(x)
  xl <- gsub("_lowconf$", "", xl)
  for (nm in names(.pal_lineage_rules)) {
    if (grepl(.pal_lineage_rules[[nm]], xl, perl = TRUE)) {
      return(unname(pal_lineage[[nm]]))
    }
  }
  NA_character_
}

#' Named colours for a vector of labels (clinical / lineage / else cluster26).
pal_for <- function(x) {
  x <- as.character(x)
  levs <- unique(x[!is.na(x) & nzchar(x)])
  cols <- vapply(levs, pal_match_one, character(1))
  leftover <- levs[is.na(cols)]
  if (length(leftover)) {
    # stable order: numeric clusters first
    num <- suppressWarnings(as.numeric(leftover))
    ord <- order(is.na(num), num, leftover)
    leftover <- leftover[ord]
    cols[match(leftover, levs)] <- pal_cluster26[
      ((seq_along(leftover) - 1L) %% length(pal_cluster26)) + 1L
    ]
  }
  names(cols) <- levs
  cols
}

pal_cols_for_idents <- function(vec) {
  vec <- as.character(vec)
  mp <- pal_for(vec)
  unname(mp[unique(vec[!is.na(vec)])])
}

theme_scauto_umap <- function() {
  ggplot2::theme_void(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(colour = pal_ink, size = 12, hjust = 0),
      legend.text = ggplot2::element_text(
        colour = pal_ink, size = 12, lineheight = 0.85,
        margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 4)
      ),
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.key.size = grid::unit(18, "pt"),
      legend.key.height = grid::unit(12, "pt"),
      legend.key.width = grid::unit(18, "pt"),
      legend.key.spacing.y = grid::unit(-1, "pt"),
      legend.key.spacing.x = grid::unit(2, "pt"),
      legend.spacing.y = grid::unit(0, "pt"),
      legend.spacing.x = grid::unit(2, "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(2, "pt"),
      plot.background = ggplot2::element_rect(fill = pal_paper, colour = NA),
      panel.background = ggplot2::element_rect(fill = pal_paper, colour = NA),
      legend.background = ggplot2::element_rect(fill = pal_paper, colour = NA)
    )
}

#' Legend / factor order: c0, c1, c2, … (or 0, 1, 2, …). Labels without a
#' cluster id stay last, sorted by name.
ident_level_order <- function(ids) {
  ids <- unique(as.character(ids))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!length(ids)) return(ids)
  num <- vapply(ids, function(s) {
    if (grepl("^[0-9]+$", s)) return(as.numeric(s))
    m <- regexpr("_c([0-9]+)$|_c([0-9]+)(\\b| ·)", s, perl = TRUE)
    if (m[[1]] > 0L) {
      hit <- regmatches(s, m)
      return(as.numeric(gsub("[^0-9]", "", hit)))
    }
    m2 <- regexpr("(^|[ ·])c([0-9]+)(\\b| ·|$)", s, perl = TRUE)
    if (m2[[1]] > 0L) {
      hit <- regmatches(s, m2)
      return(as.numeric(gsub("[^0-9]", "", hit)))
    }
    NA_real_
  }, numeric(1), USE.NAMES = FALSE)
  ids[order(is.na(num), num, ids)]
}

umap_cluster_id_label <- function(labs) {
  labs <- as.character(labs)
  out <- labs
  hit <- grepl("c[0-9]+", labs, perl = TRUE)
  if (any(hit)) {
    out[hit] <- gsub(".*?(c[0-9]+).*", "\\1", labs[hit], perl = TRUE)
  }
  out
}

.umap_label_layer_col <- function(d) {
  skip_cols <- c("umap_1", "umap_2", "UMAP_1", "UMAP_2", "x", "y",
                 "color", "colour", "fill")
  id_col <- setdiff(names(d), skip_cols)
  if (!length(id_col)) return(NULL)
  id_col[[1]]
}

#' Replace on-plot DimPlot labels with cluster ids (c0, c1, …); legend unchanged.
.shorten_umap_cluster_labels <- function(p) {
  label_geoms <- c("GeomTextRepel", "GeomText", "GeomLabelRepel", "GeomLabel")
  for (i in seq_along(p$layers)) {
    lyr <- p$layers[[i]]
    if (!any(class(lyr$geom) %in% label_geoms)) next
    d <- lyr$data
    if (is.null(d) || !nrow(d)) next
    lab_col <- .umap_label_layer_col(d)
    if (is.null(lab_col)) next
    d[[lab_col]] <- umap_cluster_id_label(d[[lab_col]])
    lyr$data <- d
    p$layers[[i]] <- lyr
  }
  p
}

#' Recolour Seurat DimPlot labels: only identity "1" is white; all others black/ink.
.apply_umap_label_contrast <- function(p) {
  label_geoms <- c("GeomTextRepel", "GeomText", "GeomLabelRepel", "GeomLabel")
  for (i in seq_along(p$layers)) {
    lyr <- p$layers[[i]]
    if (!any(class(lyr$geom) %in% label_geoms)) next
    d <- lyr$data
    if (is.null(d) || !nrow(d)) next
    lab_col <- .umap_label_layer_col(d)
    if (is.null(lab_col)) next
    labs <- as.character(d[[lab_col]])
    fg <- ifelse(labs %in% c("1", "01"), "#FFFFFF", pal_ink)
    lyr$aes_params$colour <- NULL
    lyr$aes_params$color <- NULL
    lyr$aes_params$colour <- fg
    p$layers[[i]] <- lyr
  }
  p
}

scauto_dimplot <- function(obj, group, title, label = TRUE,
                            pt.size = 0.45, raster = FALSE,
                            raster.dpi = 1024,
                            label.size = 10,
                            legend.point.size = 5,
                            cells = NULL,
                            label_as = c("value", "cluster_id")) {
  label_as <- match.arg(label_as)
  vec_all <- as.character(obj[[group, drop = TRUE]])
  blank <- is.na(vec_all) | !nzchar(trimws(vec_all))
  # DimPlot drops NA/blank cells then cbind fails (n cells vs 0 rows).
  if (length(vec_all) && all(blank)) {
    return(
      ggplot2::ggplot() +
        ggplot2::ggtitle(paste0(title, " (no values)")) +
        theme_scauto_umap()
    )
  }
  if (any(blank)) {
    vec_all[blank] <- "unassigned"
    obj[[group]] <- vec_all
  }
  if (!is.null(cells)) {
    vec <- as.character(obj[[group]][cells, 1])
  } else {
    vec <- vec_all
  }
  # Colour map from full object levels when possible, so splits share a palette.
  cols <- pal_for(vec_all)
  levs <- ident_level_order(names(cols))
  cols <- cols[levs]
  # raster=TRUE (scattermore) ignores legend size overrides → tiny legend dots.
  # Use vector points for publication UMAPs so legend keys stay large & solid.
  args <- list(
    object = obj,
    reduction = "umap",
    group.by = group,
    label = label,
    label.size = label.size,
    repel = TRUE,
    pt.size = pt.size,
    raster = raster,
    cols = unname(cols[levs])
  )
  if (!is.null(cells)) {
    args$cells <- cells
  }
  if (isTRUE(raster)) {
    args$raster.dpi <- c(raster.dpi, raster.dpi)
  }
  if ("alpha" %in% names(formals(Seurat::DimPlot))) {
    args$alpha <- 1
  }
  p <- do.call(Seurat::DimPlot, args)
  p <- suppressMessages(
    p +
      ggplot2::scale_colour_manual(
        name = NULL,
        values = cols,
        breaks = levs,
        drop = FALSE,
        guide = ggplot2::guide_legend(
          override.aes = list(
            size = legend.point.size,
            alpha = 1,
            stroke = 0,
            shape = 16
          ),
          title = NULL,
          label.position = "right",
          byrow = TRUE
        )
      ) +
      ggplot2::ggtitle(title) +
      theme_scauto_umap()
  )
  if (isTRUE(label)) {
    if (identical(label_as, "cluster_id")) {
      p <- .shorten_umap_cluster_labels(p)
    }
    p <- .apply_umap_label_contrast(p)
  }
  p
}
