source("000-config.R")
PEcAn.logger::logger.info("*** Subsampling design points from clustered pool ***")

# n_design is defined in 000-config.R. 021 emits data/design_points.csv
# with that many rows, sliced from the nested FPS order cached below.

clustering_path <- file.path(cache_dir, "clustering_pool.rds")
if (!file.exists(clustering_path)) {
  PEcAn.logger::logger.severe(
    "run 020_cluster_sites.R first; missing ", clustering_path
  )
}
clustering <- readRDS(clustering_path)

clustered_sites <- readr::read_csv(
  here::here("data", "clustered_sites.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(site_id = as.character(site_id))

site_covariates <- readr::read_csv(
  file.path(data_dir, "site_covariates.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(site_id = as.character(site_id))

anchor_sites <- readr::read_csv(
  here::here("data", "anchor_sites.csv"),
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

if (file.exists(fps_cache_path)) {
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

readr::write_csv(design_points, here::here("data", "design_points.csv"))
PEcAn.logger::logger.info(
  nrow(design_points), " design points -> data/design_points.csv"
)
