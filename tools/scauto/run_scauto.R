#!/usr/bin/env Rscript
# PBMC Auto-Pilot — default-parameter 10x triplet pipeline.
#
# Start analysis (GUI) and this script both check R packages first and
# install any that are missing from CRAN, then run the analysis.

find_scauto_root <- function() {
  args_full <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args_full, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))))
  }
  if (file.exists("tools/scauto/run_scauto.R")) {
    return(normalizePath("tools/scauto"))
  }
  normalizePath(getwd())
}

scauto_root <- find_scauto_root()
source(file.path(scauto_root, "R", "00_utils.R"))
source(file.path(scauto_root, "R", "00_packages.R"))
source(file.path(scauto_root, "R", "00_usage.R"))

cli <- scauto_parse_args()
if (is.null(cli$input) || is.null(cli$outdir)) {
  stop("Required: --input <10x dir> --outdir <dir>")
}

project <- cli$project
if (is.null(project) || !nzchar(project)) {
  project <- Sys.getenv("PROJECT_DIR", Sys.getenv("LOCAL_PROJECT_DIR", ""))
}
if (exists("scauto_resolve_project", mode = "function")) {
  if (is.null(project) || !nzchar(project) || !file.exists(file.path(project, "viz", "palette.R"))) {
    project <- scauto_resolve_project(project)
  }
} else if (!nzchar(project)) {
  cand <- normalizePath(file.path(scauto_root, "..", ".."))
  if (dir.exists(file.path(cand, "viz"))) project <- cand
}
if (!nzchar(project) || !file.exists(file.path(project, "viz", "palette.R"))) {
  stop("Cannot find project root with viz/palette.R. Pass --project")
}

outdir <- cli$outdir
if (!grepl("^/", outdir) && !grepl("^[A-Za-z]:", outdir)) {
  outdir <- file.path(project, outdir)
}
ensure_dirs(outdir)
options(
  scauto.log_file = file.path(outdir, "logs", "pipeline.log"),
  scauto.project = project,
  scauto.scauto_root = scauto_root
)
Sys.setenv(
  PROJECT_DIR = project,
  LOCAL_PROJECT_DIR = project,
  SCAUTO_PALETTE = Sys.getenv("SCAUTO_PALETTE", "light")
)

log_msg("scauto root", scauto_root)
log_msg("project", project)
log_msg("outdir", outdir)
log_msg("RSS start", mem_gb(), "GB")
log_msg("Checking every required component (installing any that are missing)…")
ensure_scauto_packages(
  which_hard = c("pipeline"),
  which_soft = c("recommended"),
  install = TRUE,
  project = project,
  check_path = file.path(outdir, "logs", "env_check.tsv"),
  incremental = TRUE
)
suppressPackageStartupMessages({
  library(Seurat)
  library(ggplot2)
  library(Matrix)
  library(jsonlite)
  library(yaml)
})
if (requireNamespace("future", quietly = TRUE)) {
  future::plan("sequential")
}
source(file.path(scauto_root, "R", "01_ingest_qc.R"))
source(file.path(scauto_root, "R", "02_reduce_cluster.R"))
source(file.path(scauto_root, "R", "03_annotate.R"))
source(file.path(scauto_root, "R", "04_downstream.R"))
source(file.path(scauto_root, "R", "05_deg.R"))
source(file.path(scauto_root, "R", "06_cassia.R"))
source(file.path(scauto_root, "R", "07_pathway.R"))
source(file.path(scauto_root, "R", "08_trajectory.R"))
source(file.path(scauto_root, "R", "09_clinical_numeric.R"))
source_palette(project)
cfg <- load_scauto_config(scauto_root, cli$config)
if (!is.null(cli$config) && nzchar(cli$config)) {
  log_msg("config overlay", cli$config)
}
log_msg("Tissue", cfg$tissue, "QC max percent.mt", cfg$qc$max_percent_mt)

lit <- build_literature_catalog(project, scauto_root)
options(scauto.literature = lit)
write_literature_exports(
  lit, project, scauto_root,
  dest_tables = file.path(outdir, "tables"),
  dest_lineage_root = file.path(outdir, "lineage")
)

sample_dirs <- discover_sample_dirs(cli$input, cli$samples)
log_msg("Samples:", paste(names(sample_dirs), collapse = ", "))
sample_meta <- read_sample_meta(cli$meta, names(sample_dirs))
sample_meta <- merge_clinical_numeric_meta(sample_meta, cfg)
extra_g <- scauto_detect_extra_group_cols(sample_meta, cfg)
if (length(extra_g)) {
  cfg$clinical$extra_group_cols <- extra_g
  log_msg("Extra clinical group columns:", paste(extra_g, collapse = ", "))
}

ing <- ingest_and_qc(sample_dirs, sample_meta, cfg, outdir)
sample_meta <- scauto_apply_analysis_names(sample_meta)
write_csv(sample_meta, file.path(outdir, "tables", "sample_metadata_used.csv"))
mode <- choose_compute_mode(ing$n_qc, cfg, allow_large = cli$allow_large)
log_msg("Compute mode", mode$mode, "—", mode$note)

obj <- merge_qc_objects(ing$objs, cfg)
ing$objs <- NULL
gc_soft()
plot_qc_violins(obj, outdir)

obj <- run_reduce_cluster(obj, cfg)
tryCatch(
  plot_atlas_umaps(obj, outdir),
  error = function(e) log_msg("ERROR atlas UMAPs", conditionMessage(e))
)
mix_tab <- log_cluster_batch_mixing(
  obj, "cluster_atlas", harmony_cfg_value(cfg, "batch_var", "sample")
)
if (!is.null(mix_tab)) {
  write_csv(
    data.frame(cluster = rownames(mix_tab), mix_tab, check.names = FALSE),
    file.path(outdir, "tables", "atlas_cluster_by_sample.csv")
  )
}

marker_json <- file.path(project, "viz", "pbmc_lineage_markers.json")
if (!file.exists(marker_json)) {
  stop("Missing ", marker_json)
}

mk_atlas <- run_findallmarkers(
  obj, "cluster_atlas", cfg,
  file.path(outdir, "tables", "atlas_findallmarkers.csv")
)
obj <- score_lineage_modules(obj, marker_json)
lin <- cluster_lineage_table(obj, "cluster_atlas", cfg, marker_json)
write_csv(lin$table, file.path(outdir, "tables", "atlas_cluster_lineage.csv"))
write_csv(lin$scores, file.path(outdir, "tables", "atlas_cluster_lineage_scores.csv"))
obj <- attach_cluster_map(obj, "cluster_atlas", lin$table)
plot_lineage_dotplot(obj, "cluster_atlas", lin, outdir)
p_lin <- scauto_dimplot(obj, "lineage", "UMAP · atlas lineage (marker modules)", label = TRUE)
save_plot(p_lin, file.path(outdir, "figures", "umap_atlas_lineage"), width = 10, height = 7.5)
p_sup <- scauto_dimplot(obj, "superlineage", "UMAP · superlineage", label = TRUE)
save_plot(p_sup, file.path(outdir, "figures", "umap_atlas_superlineage"), width = 10, height = 7.5)

atlas_asg <- data.frame(
  cell = colnames(obj),
  sample = obj$sample,
  cluster_atlas = obj$cluster_atlas,
  lineage = obj$lineage,
  superlineage = obj$superlineage,
  subtype = obj$lineage,
  stringsAsFactors = FALSE
)
write_csv(atlas_asg, file.path(outdir, "tables", "atlas_cell_assignments.csv"))
run_clinical(obj, atlas_asg, cfg, outdir, "atlas_lineage")

supers <- sort(unique(obj$superlineage))
supers <- setdiff(supers, "Other")
traj_lin <- trajectory_wanted_lineages(cfg, supers)
all_asg <- list()
for (sp in supers) {
  res <- tryCatch(
    recluster_one_lineage(
      obj, sp, cfg, scauto_root, outdir,
      keep_object = sp %in% traj_lin
    ),
    error = function(e) {
      log_msg("ERROR recluster", sp, conditionMessage(e))
      NULL
    }
  )
  if (is.null(res)) next
  all_asg[[sp]] <- res$assignments
  tryCatch(
    run_functional_modules(obj, res$assignments, sp, scauto_root, project, outdir),
    error = function(e) log_msg("ERROR modules", sp, conditionMessage(e))
  )
  tryCatch(
    run_clinical(obj, res$assignments, cfg, outdir, sp),
    error = function(e) log_msg("ERROR clinical", sp, conditionMessage(e))
  )
  tryCatch(
    run_clinical_numeric(obj, res$assignments, sample_meta, cfg, outdir, sp),
    error = function(e) log_msg("ERROR clinical numeric", sp, conditionMessage(e))
  )
  tryCatch(
    run_clinical_deg(obj, res$assignments, cfg, outdir, sp),
    error = function(e) log_msg("ERROR DEG", sp, conditionMessage(e))
  )
  tryCatch(
    run_pathway_enrichment(obj, res$assignments, cfg, outdir, sp),
    error = function(e) log_msg("ERROR pathway", sp, conditionMessage(e))
  )
  if (!is.null(res$object) && sp %in% traj_lin) {
    tryCatch(
      run_lineage_trajectory(res$object, res$assignments, sp, cfg, outdir),
      error = function(e) log_msg("ERROR trajectory", sp, conditionMessage(e))
    )
    res$object <- NULL
    gc_soft()
  }
}

if (length(all_asg)) {
  comb <- do.call(rbind, all_asg)
  write_csv(comb, file.path(outdir, "tables", "all_lineage_subtype_assignments.csv"))
} else {
  comb <- NULL
}
write_sample_cell_count_tables(atlas_asg, comb, sample_meta, outdir)
write_combined_subtype_marker_tables(outdir, markers_n_top(cfg))

md <- obj[[]]
md$cell <- colnames(obj)
write_csv(md, file.path(outdir, "tables", "cell_metadata.csv"))
if (scauto_want_atlas_rds(cfg)) {
  obj <- scauto_slim_seurat_for_rds(obj)
  gc_soft()
  scauto_save_atlas_rds(obj, outdir, slim = FALSE)
} else {
  log_msg("Skip atlas RDS (turned off in settings)")
}

manifest <- list(
  project_name = cfg$project_name,
  finished = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  input = cli$input,
  meta = cli$meta,
  samples = names(sample_dirs),
  n_raw = ing$n_raw,
  n_qc = ncol(obj),
  n_hvg = length(Seurat::VariableFeatures(obj)),
  npcs = cfg$normalize$npcs,
  leiden_res = cfg$cluster$resolution,
  n_clusters = length(unique(obj$cluster_atlas)),
  compute_mode = mode$mode,
  compute_note = mode$note,
  rss_gb_end = mem_gb(),
  harmony = "harmony" %in% Seurat::Reductions(obj),
  harmony_theta = harmony_cfg_value(cfg, "theta", 4),
  harmony_max_iter = harmony_cfg_value(cfg, "max_iter", 20),
  harmony_early_stop = isTRUE(harmony_cfg_value(cfg, "early_stop", FALSE)),
  azimuth = FALSE,
  celltypist = FALSE
)
write_manifest(file.path(outdir, "run_manifest.json"), manifest)
write_report(outdir, manifest, ing$qc_by_sample, lin$table)
if (exists("scauto_usage_record_finish", mode = "function")) {
  scauto_usage_record_finish(
    project, outdir, ok = TRUE,
    mode = if (nzchar(Sys.getenv("SCAUTO_NO_PKG_INSTALL"))) "hpc" else "local"
  )
}
log_msg("DONE", outdir, "RSS", mem_gb(), "GB")
