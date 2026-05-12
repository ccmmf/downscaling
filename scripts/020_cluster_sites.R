library(ggplot2)
source("000-config.R")
PEcAn.logger::logger.info("*** Clustering sites into pool ***")

# pool_size, pool_floors, subsample_threshold are defined in 000-config.R.
# pool is the frozen cluster artifact; 021 subsamples from it
seed <- 42L

# load covariates and coordinates
site_covariates <- readr::read_csv(
  file.path(data_dir, "site_covariates.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(site_id = as.character(site_id))

site_coords <- readr::read_csv(
  file.path(data_dir, "cadwr_crops_site_summary.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(site_id = as.character(site_id)) |>
  dplyr::select(site_id, lat, lon)

site_covariates <- site_covariates |>
  dplyr::left_join(site_coords, by = "site_id")

# features -- environmental + cropping-history EOFs + management + phenology
# tillage_intensity and tillage_freq are continuous; irr_canopy/irr_flood are
# binary one hots covering the v1.0 irrigation method vocabulary
base_covariates <- c("temp", "precip", "srad", "vapr", "clay", "ocd", "twi")
eof_cols <- sort(grep("^eof_", colnames(site_covariates), value = TRUE))
mgmt_cols <- intersect(
  c("tillage_rank", "tillage_freq", "irr_canopy", "irr_flood"),
  colnames(site_covariates)
)
phen_cols <- intersect(
  c("leafon_doy", "leafoff_doy", "leafon_doy_sd", "leafoff_doy_sd"),
  colnames(site_covariates)
)

if (length(eof_cols) == 0) {
  PEcAn.logger::logger.warn("no EOF columns; run 009 first")
}
if (length(mgmt_cols) == 0) {
  PEcAn.logger::logger.warn("no management covariates; run 012 first")
}
feature_cols <- c(base_covariates, eof_cols, phen_cols, mgmt_cols)

missing <- setdiff(feature_cols, colnames(site_covariates))
if (length(missing) > 0) {
  PEcAn.logger::logger.severe(
    "missing feature columns: ", paste(missing, collapse = ", ")
  )
}
PEcAn.logger::logger.info(
  "features (", length(feature_cols), "): ",
  paste(feature_cols, collapse = ", ")
)

# anchor sites to force include
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

# allocation across PFTs (area proportional with floors)
alloc <- allocate_design_points_by_pft(
  site_covariates = site_covariates,
  total = pool_size,
  floors = pool_floors
)

# cluster each PFT
results <- purrr::imap(alloc, function(n_clusters, pft_name) {
  PEcAn.logger::logger.info(
    "clustering ", pft_name, " (target ", n_clusters, " sites)"
  )
  pop <- site_covariates |> dplyr::filter(pft == pft_name)
  anchors <- anchor_sites |>
    dplyr::filter(pft == pft_name) |>
    dplyr::pull(site_id)
  cluster_pft_population(
    pop_data = pop,
    feature_cols = feature_cols,
    n_clusters = n_clusters,
    subsample_threshold = subsample_threshold,
    seed = seed,
    anchor_site_ids = anchors
  )
})

# clustered site pool, one row per cluster centroid representative
clustered_sites <- purrr::imap_dfr(results, function(r, pft_name) {
  r$design |> dplyr::mutate(pft = pft_name)
}) |>
  dplyr::select(site_id, lat, lon, pft, cluster, dist_to_centroid) |>
  dplyr::mutate(dplyr::across(c(lat, lon), \(x) round(x, 5)))

readr::write_csv(clustered_sites, here::here("data", "clustered_sites.csv"))

# cache for 021 (subsampling) and 022 (validation)
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
saveRDS(
  list(
    clustered_sites = clustered_sites,
    by_pft = results,
    allocation = alloc,
    feature_cols = feature_cols,
    seed = seed,
    timestamp = Sys.time(),
    session = utils::sessionInfo()
  ),
  file.path(cache_dir, "clustering_pool.rds")
)

# wide cache for 021 diagnostics
sites_clustered <- purrr::imap_dfr(results, function(r, pft_name) {
  r$cluster_assignment |> dplyr::mutate(pft = pft_name)
}) |>
  dplyr::left_join(
    site_covariates |> dplyr::select(-dplyr::any_of("pft")),
    by = "site_id"
  )

saveRDS(sites_clustered, file.path(cache_dir, "sites_clustered.rds"))

PEcAn.logger::logger.info(
  nrow(clustered_sites), " clustered sites -> data/clustered_sites.csv"
)
