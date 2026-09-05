library(ggplot2)
library(optparse)
args <- parse_args(OptionParser(option_list = list(
  make_option("--run_dir", type = "character",
    help = "Path to the run directory (required)"),
  make_option("--mode", type = "character", default = "production",
    help = "Run mode: production, dev, demo [default: %default]"),
  make_option("--pool_size", type = "integer", default = 10000L,
    help = "Clustered-site pool size [default: %default]"),
  make_option("--pool_floors", type = "character",
    default = "annual crop=700,woody perennial crop=300",
    help = "Per-PFT minimum pool allocation as 'pft=n,...' [default: %default]"),
  make_option("--subsample_threshold", type = "integer", default = 20000L,
    help = "Threshold triggering two-stage clustering [default: %default]"),
  make_option("--covariates_csv", type = "character",
    help = "Path to site covariates CSV, from 010 (required)"),
  make_option("--anchor_sites_csv", type = "character",
    help = "Path to anchor sites CSV, from 011 (required)"),
  make_option("--data_dir", type = "character",
    help = "Path to staged/cached data directory (required)"),
  make_option("--clustered_sites_csv", type = "character",
    help = "Output path for clustered sites CSV (required)"),
  make_option("--cache_dir", type = "character",
    help = "Path to cache directory for clustering artifacts (required)")
)))
if (is.null(args$run_dir)) PEcAn.logger::logger.severe("--run_dir is required")
if (!args$mode %in% c("production", "dev", "demo")) PEcAn.logger::logger.severe("--mode must be one of: production, dev, demo")

run_dir     <- args$run_dir
data_dir    <- args$data_dir
cache_dir   <- args$cache_dir

pool_size           <- args$pool_size
subsample_threshold <- args$subsample_threshold
pool_floors_pairs <- strsplit(args$pool_floors, ",")[[1]]
pool_floors <- setNames(
  as.integer(sapply(pool_floors_pairs, function(p) strsplit(p, "=")[[1]][2])),
  sapply(pool_floors_pairs, function(p) strsplit(p, "=")[[1]][1])
)

source(file.path(here::here(), "R", "cluster_design_points.R"))
options(tibble.width = Inf, readr.show_col_types = FALSE)
PEcAn.logger::logger.info("*** Clustering sites into pool ***")

# pool is the frozen cluster artifact; 021 subsamples from it
seed <- 42L

# load covariates and coordinates
site_covariates <- readr::read_csv(
  args$covariates_csv,
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

readr::write_csv(clustered_sites, args$clustered_sites_csv)

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
  nrow(clustered_sites), " clustered sites -> ", args$clustered_sites_csv
)
