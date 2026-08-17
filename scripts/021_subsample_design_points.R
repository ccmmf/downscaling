library(optparse)
args <- parse_args(OptionParser(option_list = list(
  make_option("--run_dir", type = "character",
    help = "Path to the run directory (required)"),
  make_option("--mode", type = "character", default = "production",
    help = "Run mode: production, dev, demo [default: %default]"),
  make_option("--n_design", type = "integer", default = 1000L,
    help = "Design point subsample size [default: %default]"),
  make_option("--clustered_sites_csv", type = "character",
    help = "Path to clustered sites CSV, from 020 (required)"),
  make_option("--covariates_csv", type = "character",
    help = "Path to site covariates CSV, from 010 (required)"),
  make_option("--anchor_sites_csv", type = "character",
    help = "Path to anchor sites CSV, from 011 (required)"),
  make_option("--cache_dir", type = "character",
    help = "Path to cache directory for clustering artifacts (required)"),
  make_option("--design_points_csv", type = "character",
    help = "Output path for design points CSV (required)")
)))
if (is.null(args$run_dir)) stop("--run_dir is required")
if (!args$mode %in% c("production", "dev", "demo")) stop("--mode must be one of: production, dev, demo")

run_dir   <- args$run_dir
cache_dir <- args$cache_dir
n_design  <- args$n_design

source(file.path(here::here(), "R", "fps_sampler.R"))
options(tibble.width = Inf, readr.show_col_types = FALSE)
PEcAn.logger::logger.info("*** Subsampling design points from clustered pool ***")

# 021 emits output_prepare/design_points.csv with n_design rows, sliced from
# the nested FPS order cached below.

clustering_path <- file.path(cache_dir, "clustering_pool.rds")
if (!file.exists(clustering_path)) {
  PEcAn.logger::logger.severe(
    "run 020_cluster_sites.R first; missing ", clustering_path
  )
}
clustering <- readRDS(clustering_path)

clustered_sites <- readr::read_csv(
  args$clustered_sites_csv,
  show_col_types = FALSE
) |>
  dplyr::mutate(site_id = as.character(site_id))

site_covariates <- readr::read_csv(
  args$covariates_csv,
  show_col_types = FALSE
) |>
  dplyr::mutate(site_id = as.character(site_id))

anchor_sites <- readr::read_csv(
  args$anchor_sites_csv,
  show_col_types = FALSE
) |>
  dplyr::mutate(site_id = as.character(site_id))
anchor_pft_lookup <- c(
  "woody perennial crop" = "woody perennial crop",
  "annual crop" = "annual crop",
  "herbaceous crop" = "annual crop"
)
anchor_sites$pft <- anchor_pft_lookup[anchor_sites$pft]

pool_size <- nrow(clustered_sites)
if (n_design > pool_size) {
  PEcAn.logger::logger.warn(
    "n_design (", n_design, ") > pool_size (", pool_size, "); clamping"
  )
  n_design <- pool_size
}

# per pft allocation; same proportional rule as 020, scaled to n_design.
pool_alloc <- clustering$allocation
n_by_pft <- round(pool_alloc * n_design / sum(pool_alloc))
drift <- n_design - sum(n_by_pft)
if (drift != 0) n_by_pft[which.max(n_by_pft)] <- n_by_pft[which.max(n_by_pft)] + drift

PEcAn.logger::logger.info(
  "subsample allocation (", n_design, " total): ",
  paste0(names(n_by_pft), "=", n_by_pft, collapse = ", ")
)

# greedy farthest point sampling (maximin) gives a nested ordering so
# any prefix is a valid subset. cache per PFT for skill vs N ladder.
# anchors prepended to each PFT's order so they're in every non empty subset
fps_cache_path <- file.path(cache_dir, "fps_order.rds")

# rebuild if clustering pool is newer than cached order (make style)
fps_fresh <- file.exists(fps_cache_path) &&
  file.mtime(fps_cache_path) > file.mtime(clustering_path)

if (fps_fresh) {
  fps_order <- readRDS(fps_cache_path)
  PEcAn.logger::logger.info("loaded FPS order from cache")
} else {
  fps_order <- list()
  for (pft_name in names(clustering$by_pft)) {
    r <- clustering$by_pft[[pft_name]]
    feat_cols <- r$feature_cols
    pool_ids <- clustered_sites |>
      dplyr::filter(pft == pft_name) |>
      dplyr::pull(site_id)

    feat_df <- site_covariates |>
      dplyr::filter(site_id %in% pool_ids) |>
      dplyr::select(site_id, dplyr::all_of(feat_cols)) |>
      tidyr::drop_na()

    x <- base::scale(
      as.matrix(feat_df[, feat_cols, drop = FALSE]),
      center = r$scaling_center,
      scale = r$scaling_scale
    )

    PEcAn.logger::logger.info(
      "FPS on ", nrow(x), " ", pft_name, " sites"
    )
    ord <- greedy_fps(x)
    ordered_ids <- feat_df$site_id[ord]

    # prepend anchors so small N still includes validation sites
    anchors_here <- anchor_sites |>
      dplyr::filter(pft == pft_name) |>
      dplyr::pull(site_id)
    anchors_in_pool <- intersect(anchors_here, ordered_ids)
    fps_order[[pft_name]] <- c(
      anchors_in_pool,
      setdiff(ordered_ids, anchors_in_pool)
    )
  }
  saveRDS(fps_order, fps_cache_path)
  PEcAn.logger::logger.info("cached FPS order -> ", fps_cache_path)
}

# slice the first n_by_pft[p] from each PFT's FPS order
design_ids <- unlist(purrr::imap(fps_order, function(ids, pft_name) {
  head(ids, n_by_pft[pft_name])
}))

design_points <- clustered_sites |>
  dplyr::filter(site_id %in% design_ids) |>
  dplyr::select(site_id, lat, lon, pft)

readr::write_csv(design_points, args$design_points_csv)
PEcAn.logger::logger.info(
  nrow(design_points), " design points -> ", args$design_points_csv
)
