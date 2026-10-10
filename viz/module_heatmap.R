# Stitched module × subtype heatmap with guaranteed square cells.
# ggplot/ComplexHeatmap shrink the body when large y-labels exceed the
# reserved width; this helper draws body/labels/legend separately and pastes.
#
# Usage:
#   source("viz/module_heatmap.R")
#   save_module_heatmap_stitched(mat, row_lab, title, out_stem, ...)

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
  library(grid)
  library(cowplot)
  library(png)
})

.module_hm_sanitize_lab <- function(labs) {
  # Arial/ragg often drop Greek glyphs → looks like truncated names (e.g. "γδT" → "T").
  labs <- as.character(labs)
  labs <- gsub("γδ", "gd", labs, fixed = TRUE)
  labs <- gsub("γ", "g", labs, fixed = TRUE)
  labs <- gsub("δ", "d", labs, fixed = TRUE)
  labs <- gsub("–", "-", labs, fixed = TRUE)   # en-dash
  labs <- gsub("—", "-", labs, fixed = TRUE)   # em-dash
  labs <- gsub("−", "-", labs, fixed = TRUE)   # minus
  labs <- gsub("×", "x", labs, fixed = TRUE)
  labs
}

.module_hm_measure_label_in <- function(labs, fontsize_pt, family) {
  # Render labels the same way as the final panel, then measure ink bbox in pixels.
  # grobWidth/strwidth can disagree with ragg and leave names clipped.
  labs <- as.character(labs)
  n <- length(labs)
  dpi <- 150
  trial_w <- 28
  trial_h <- max(1.2, n * (fontsize_pt / 72) * 1.4)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ragg::agg_png(
    tmp, width = trial_w, height = trial_h,
    units = "in", res = dpi, background = "white"
  )
  grid::grid.newpage()
  gp <- grid::gpar(fontsize = fontsize_pt, fontfamily = family, col = "black")
  inset <- unit(1.2, "mm")
  for (i in seq_len(n)) {
    grid::grid.text(
      labs[[i]],
      x = unit(1, "npc") - inset,
      y = unit((n - i + 0.5) / n, "npc"),
      just = c("right", "centre"),
      gp = gp
    )
  }
  grDevices::dev.off()
  img <- png::readPNG(tmp)
  g <- img[, , 1]
  xs <- which(colMeans(g < 0.78) > 1e-4)
  if (!length(xs)) return(6)
  ink_in <- (max(xs) - min(xs) + 1) / dpi
  # left breathing room + right inset already present in the render
  ink_in + 0.35
}

.module_hm_as_rgba <- function(w_px, h_px) array(1, dim = c(h_px, w_px, 4))

.module_hm_read_png <- function(path) {
  a <- png::readPNG(path)
  if (length(dim(a)) == 2) {
    b <- array(1, dim = c(dim(a)[1], dim(a)[2], 4))
    b[, , 1:3] <- a
    a <- b
  } else if (dim(a)[3] == 3) {
    b <- array(1, dim = c(dim(a)[1], dim(a)[2], 4))
    b[, , 1:3] <- a
    a <- b
  }
  a
}

.module_hm_paste_h <- function(imgs) {
  ws <- vapply(imgs, function(a) dim(a)[2], 1L)
  h <- max(vapply(imgs, function(a) dim(a)[1], 1L))
  out <- .module_hm_as_rgba(sum(ws), h)
  x <- 1L
  for (a in imgs) {
    out[seq_len(dim(a)[1]), x:(x + dim(a)[2] - 1L), ] <- a
    x <- x + dim(a)[2]
  }
  out
}

.module_hm_paste_v <- function(imgs) {
  hs <- vapply(imgs, function(a) dim(a)[1], 1L)
  w <- max(vapply(imgs, function(a) dim(a)[2], 1L))
  out <- .module_hm_as_rgba(w, sum(hs))
  y <- 1L
  for (a in imgs) {
    out[y:(y + dim(a)[1] - 1L), seq_len(dim(a)[2]), ] <- a
    y <- y + dim(a)[1]
  }
  out
}

#' Save a module z-score heatmap with square cells (stitched layout).
#'
#' @param mat numeric matrix (modules × clusters), already ordered
#' @param row_lab character vector of row labels (same order as rownames(mat))
#' @param title plot title
#' @param out_stem output path without extension
#' @param cell_mm square cell size in millimetres
#' @param y_fs / x_fs / title_fs font sizes in pt
#' @param font_family font family
#' @param div_cols diverging palette (length >= 3)
#' @param title_colour title colour
#' @param dpi raster resolution
save_module_heatmap_stitched <- function(
  mat,
  row_lab,
  title,
  out_stem,
  cell_mm = 18,
  y_fs = 40,
  x_fs = 24,
  title_fs = 28,
  font_family = "Arial",
  div_cols = c("#1F3A5F", "#F5F2EB", "#C45C26"),
  title_colour = "#1F3A5F",
  dpi = 300,
  legend_w_in = 1.4,
  xlab_h_in = 1.2,
  title_h_in = 0.6,
  xlab_pad_in = 0.55,
  legend_title_fs = 15,
  legend_text_fs = 13,
  legend_barwidth_cm = 0.55,
  legend_barheight_cm = NULL,
  ylab_inset_mm = 1.2,
  legend_name = "Module\nz-score",
  row_ann = NULL,
  row_ann_cols = NULL,
  row_ann_labels = NULL,
  row_ann_title = "Upregulated in",
  row_ann_w_mm = 8.5,
  row_ann_gap_mm = 1.4
) {
  mat <- as.matrix(mat)
  storage.mode(mat) <- "numeric"
  clusters <- colnames(mat)
  n_cl <- ncol(mat)
  n_mod <- nrow(mat)
  row_lab <- .module_hm_sanitize_lab(row_lab)
  if (length(row_lab) != n_mod) {
    stop("row_lab length (", length(row_lab), ") != nrow(mat) (", n_mod, ")")
  }
  if (length(div_cols) < 3) {
    div_cols <- c("#1F3A5F", "#F5F2EB", "#C45C26")
  }
  has_ann <- !is.null(row_ann)
  if (has_ann) {
    row_ann <- as.character(row_ann)
    if (length(row_ann) != n_mod) {
      stop("row_ann length (", length(row_ann), ") != nrow(mat) (", n_mod, ")")
    }
    if (is.null(row_ann_cols) || !length(row_ann_cols)) {
      u <- unique(row_ann[!is.na(row_ann)])
      row_ann_cols <- setNames(rep("#888888", length(u)), u)
    }
    if (is.null(row_ann_labels)) {
      row_ann_labels <- setNames(names(row_ann_cols), names(row_ann_cols))
      miss <- setdiff(names(row_ann_cols), names(row_ann_labels))
      if (length(miss)) row_ann_labels[miss] <- miss
    }
    gap_in <- row_ann_gap_mm / 25.4
    ann_w_in <- row_ann_w_mm / 25.4
    ann_block_w <- gap_in + ann_w_in + gap_in
    legend_w_in <- max(legend_w_in, 2.05)
  } else {
    ann_block_w <- 0
  }

  cell_in <- cell_mm / 25.4
  heat_w <- cell_in * n_cl
  heat_h <- cell_in * n_mod
  lab_w <- .module_hm_measure_label_in(row_lab, y_fs, font_family)

  hm <- expand.grid(
    xi = seq_len(n_cl), yi = seq_len(n_mod),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  hm$score <- as.numeric(mat[cbind(n_mod + 1L - hm$yi, hm$xi)])

  p_body <- ggplot(hm, aes(xi, yi, fill = score)) +
    geom_tile(colour = "white", linewidth = 0.65, width = 1, height = 1) +
    scale_fill_gradientn(
      colours = div_cols, limits = c(-2, 2), oob = squish, guide = "none"
    ) +
    scale_x_continuous(expand = c(0, 0), limits = c(0.5, n_cl + 0.5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0.5, n_mod + 0.5)) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))

  body_png <- tempfile(fileext = ".png")
  ragg::agg_png(
    body_png, width = heat_w, height = heat_h,
    units = "in", res = dpi, background = "white"
  )
  print(p_body)
  grDevices::dev.off()

  # X labels: full width under (row names + optional direction bar + heatmap).
  xlab_w <- lab_w + ann_block_w + heat_w
  xlab_png <- tempfile(fileext = ".png")
  ragg::agg_png(
    xlab_png, width = xlab_w, height = xlab_h_in,
    units = "in", res = dpi, background = "white"
  )
  grid::grid.newpage()
  gp_x <- grid::gpar(fontsize = x_fs, fontfamily = font_family, col = "black")
  for (i in seq_len(n_cl)) {
    x_in <- lab_w + ann_block_w + (i - 0.5) / n_cl * heat_w
    grid::grid.text(
      clusters[[i]],
      x = unit(x_in, "in"),
      y = unit(1, "npc") - unit(1.5, "mm"),
      rot = 45,
      just = c("right", "top"),
      gp = gp_x
    )
  }
  grDevices::dev.off()

  # Draw row labels with grid (no ggplot panel clipping at x=0/1).
  # Right-align close to the heatmap (small mm inset only).
  ylab_png <- tempfile(fileext = ".png")
  ragg::agg_png(
    ylab_png, width = lab_w, height = heat_h,
    units = "in", res = dpi, background = "white"
  )
  grid::grid.newpage()
  labs_draw <- rev(row_lab)
  gp_lab <- grid::gpar(fontsize = y_fs, fontfamily = font_family, col = "black")
  for (i in seq_len(n_mod)) {
    y_npc <- (i - 0.5) / n_mod
    grid::grid.text(
      labs_draw[[i]],
      x = unit(1, "npc") - unit(ylab_inset_mm, "mm"),
      y = unit(y_npc, "npc"),
      just = c("right", "centre"),
      gp = gp_lab
    )
  }
  grDevices::dev.off()

  ann_png <- NULL
  gap_png <- NULL
  if (has_ann) {
    ann_df <- data.frame(
      xi = 1,
      yi = seq_len(n_mod),
      grp = factor(
        row_ann[n_mod + 1L - seq_len(n_mod)],
        levels = names(row_ann_cols)
      ),
      stringsAsFactors = FALSE
    )
    p_ann <- ggplot(ann_df, aes(xi, yi, fill = grp)) +
      geom_tile(colour = NA, width = 1, height = 1) +
      annotate(
        "rect", xmin = 0.5, xmax = 1.5, ymin = 0.5, ymax = n_mod + 0.5,
        colour = "black", fill = NA, linewidth = 0.45
      ) +
      scale_fill_manual(values = row_ann_cols, guide = "none", drop = FALSE) +
      scale_x_continuous(expand = c(0, 0), limits = c(0.5, 1.5)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0.5, n_mod + 0.5)) +
      theme_void() +
      theme(plot.margin = margin(0, 0, 0, 0))
    ann_png <- tempfile(fileext = ".png")
    ragg::agg_png(
      ann_png, width = ann_w_in, height = heat_h,
      units = "in", res = dpi, background = "white"
    )
    print(p_ann)
    grDevices::dev.off()
    gap_png <- tempfile(fileext = ".png")
    ragg::agg_png(
      gap_png, width = gap_in, height = heat_h,
      units = "in", res = dpi, background = "white"
    )
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
    grDevices::dev.off()
  }

  if (is.null(legend_barheight_cm)) {
    legend_barheight_cm <- min(10, max(5, n_mod * 0.35))
  }
  p_leg <- ggplot(hm, aes(xi, yi, fill = score)) +
    geom_tile() +
    scale_fill_gradientn(
      colours = div_cols, limits = c(-2, 2), oob = squish,
      name = legend_name,
      guide = guide_colorbar(
        title.position = "top",
        barwidth = unit(legend_barwidth_cm, "cm"),
        barheight = unit(legend_barheight_cm, "cm"),
        frame.colour = "black", ticks.colour = "black"
      )
    ) +
    theme_void(base_family = font_family) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = legend_title_fs, colour = "black",
                                  lineheight = 0.95),
      legend.text = element_text(size = legend_text_fs, colour = "black")
    )
  leg <- cowplot::get_legend(p_leg)
  if (has_ann) {
    ann_lev <- names(row_ann_cols)
    lab_map <- row_ann_labels
    if (is.null(names(lab_map))) names(lab_map) <- ann_lev
    dummy <- data.frame(
      x = 1, y = seq_along(ann_lev),
      g = factor(ann_lev, levels = ann_lev),
      stringsAsFactors = FALSE
    )
    p_annleg <- ggplot(dummy, aes(x, y, fill = g)) +
      geom_tile(colour = "black", linewidth = 0.25) +
      scale_fill_manual(
        values = row_ann_cols,
        breaks = ann_lev,
        labels = unname(lab_map[ann_lev]),
        name = row_ann_title,
        drop = FALSE,
        guide = guide_legend(
          title.position = "top",
          override.aes = list(colour = "black")
        )
      ) +
      theme_void(base_family = font_family) +
      theme(
        legend.position = "right",
        legend.title = element_text(size = legend_title_fs, colour = "black",
                                    lineheight = 0.95),
        legend.text = element_text(size = legend_text_fs, colour = "black"),
        legend.key.height = unit(4.2, "mm"),
        legend.key.width = unit(4.2, "mm")
      )
    leg_ann <- cowplot::get_legend(p_annleg)
    leg <- cowplot::plot_grid(
      cowplot::ggdraw(leg),
      cowplot::ggdraw(leg_ann),
      ncol = 1,
      rel_heights = c(2.15, 1),
      align = "v"
    )
  }
  leg_png <- tempfile(fileext = ".png")
  ragg::agg_png(
    leg_png, width = legend_w_in, height = heat_h,
    units = "in", res = dpi, background = "white"
  )
  grid::grid.newpage()
  if (inherits(leg, "ggplot")) print(leg) else grid::grid.draw(leg)
  grDevices::dev.off()

  title <- .module_hm_sanitize_lab(title)
  title_w <- lab_w + ann_block_w + heat_w + legend_w_in
  p_title <- ggplot() +
    annotate(
      "text", x = 0.01, y = 0.5, label = title,
      hjust = 0, vjust = 0.5, size = title_fs / .pt,
      family = font_family, colour = title_colour
    ) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_void()
  title_png <- tempfile(fileext = ".png")
  ragg::agg_png(
    title_png, width = title_w, height = title_h_in,
    units = "in", res = dpi, background = "white"
  )
  print(p_title)
  grDevices::dev.off()

  body <- .module_hm_read_png(body_png)
  xlab <- .module_hm_read_png(xlab_png)
  ylab <- .module_hm_read_png(ylab_png)
  lega <- .module_hm_read_png(leg_png)
  titl <- .module_hm_read_png(title_png)

  # Gene names | optional direction bar | heatmap. X labels sit under the
  # full block so 45° names can run left without a white gap.
  if (has_ann) {
    gapp <- .module_hm_read_png(gap_png)
    anna <- .module_hm_read_png(ann_png)
    top_mid <- .module_hm_paste_h(list(ylab, gapp, anna, gapp, body))
  } else {
    top_mid <- .module_hm_paste_h(list(ylab, body))
  }
  if (dim(xlab)[2] < dim(top_mid)[2]) {
    xlab <- .module_hm_paste_h(list(
      xlab,
      .module_hm_as_rgba(dim(top_mid)[2] - dim(xlab)[2], dim(xlab)[1])
    ))
  } else if (dim(xlab)[2] > dim(top_mid)[2]) {
    top_mid <- .module_hm_paste_h(list(
      top_mid,
      .module_hm_as_rgba(dim(xlab)[2] - dim(top_mid)[2], dim(top_mid)[1])
    ))
  }
  mid <- .module_hm_paste_v(list(top_mid, xlab))
  spacer_r <- .module_hm_as_rgba(dim(lega)[2], dim(xlab)[1])
  right <- .module_hm_paste_v(list(lega, spacer_r))
  row1 <- .module_hm_paste_h(list(mid, right))

  if (dim(titl)[2] < dim(row1)[2]) {
    pad <- .module_hm_as_rgba(dim(row1)[2] - dim(titl)[2], dim(titl)[1])
    titl <- .module_hm_paste_h(list(titl, pad))
  } else if (dim(titl)[2] > dim(row1)[2]) {
    pad <- .module_hm_as_rgba(dim(titl)[2] - dim(row1)[2], dim(row1)[1])
    row1 <- .module_hm_paste_h(list(row1, pad))
  }
  final <- .module_hm_paste_v(list(titl, row1))

  dir.create(dirname(out_stem), recursive = TRUE, showWarnings = FALSE)
  png::writePNG(final, paste0(out_stem, ".png"))

  pw <- dim(final)[2] / dpi
  ph <- dim(final)[1] / dpi
  grDevices::cairo_pdf(paste0(out_stem, ".pdf"), width = pw, height = ph)
  grid::grid.raster(final, interpolate = FALSE)
  grDevices::dev.off()

  invisible(list(
    width_in = pw, height_in = ph,
    label_in = lab_w, heat_w = heat_w, heat_h = heat_h,
    cell_mm = cell_mm
  ))
}
