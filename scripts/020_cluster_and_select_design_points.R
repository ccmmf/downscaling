library(ggplot2)
# settings
source("000-config.R")
PEcAn.logger::logger.info("***Starting Clustering and Design Point Selection***")
#' ## Load Site Environmental Data Covariates
#'
#' Environmental data was pre-processed in the previous workflows.
#'
#' Below is a summary of the covariates dataset
#'
#' - site_id: Unique identifier for each parcel (harmonized parcel_id)
#' - crop: Crop type
#' - pft: Plant functional type
#' - temp: Mean Annual Temperature from ERA5
#' - precip: Mean Annual Precipitation from ERA5
#' - srad: Solar Radiation
#' - vapr: Vapor pressure deficit
#' - clay: Clay content from SoilGrids
#' - ocd: Organic Carbon content from SoilGrids
#' - twi: Topographic Wetness Index
#' - climregion_id: identifier for each climate region
#' - eof_1 to eof_N: EOF scores capturing cropping system patterns (N determined by 009)
# EOF components are determined dynamically by 009 (80% variance threshold, max 10)
# so we detect which eof_* columns actually exist after loading covariates
base_covariates <- c("temp", "precip", "srad", "vapr", "clay", "ocd", "twi")

# Load harmonized CADWR field data (from 009_prepare_cadwr_crops.R)
ca_fields <- sf::st_read(file.path(data_dir, "cadwr_crops_sites.gpkg"), quiet = TRUE)
ca_climregions <- caladaptr::ca_aoipreset_geom("climregions") |>
  dplyr::rename(climregion_name = name, climregion_id = id)

# Load site covariates and coordinates
site_covariates_csv <- file.path(data_dir, "site_covariates.csv")
site_summary_csv <- file.path(data_dir, "cadwr_crops_site_summary.csv")

site_covariates <- readr::read_csv(site_covariates_csv, show_col_types = FALSE) |>
  dplyr::mutate(site_id = as.character(site_id))

# Get lat/lon from site summary (created by 009)
site_coords <- readr::read_csv(site_summary_csv, show_col_types = FALSE) |>
  dplyr::mutate(site_id = as.character(site_id)) |>
  dplyr::select(site_id, lat, lon)

site_covariates <- site_covariates |>
  dplyr::left_join(site_coords, by = "site_id")

# detect EOF columns dynamically -- 009 picks n_components based on 80% variance
eof_cols <- sort(grep("^eof_", colnames(site_covariates), value = TRUE))
if (length(eof_cols) == 0) {
  PEcAn.logger::logger.warn("No EOF columns found in site_covariates")
}
selected_covariates <- c(base_covariates, eof_cols)
PEcAn.logger::logger.info("Using ", length(eof_cols), " EOF components: ",
                           paste(eof_cols, collapse = ", "))

# Check that all selected_covariates are present, otherwise stop.
missing_covariates <- setdiff(selected_covariates, colnames(site_covariates))
if (length(missing_covariates) > 0) {
  PEcAn.logger::logger.error("Missing covariates:", paste(missing_covariates, collapse = ", "))
} else {
  PEcAn.logger::logger.info("All expected covariates are present:", paste(selected_covariates, collapse = ", "))
}

#' ## Load design points and reconcile site_ids
#'
#' site IDs changed from hash-based -> UniqueID -> parcel_id across phases.
#' use match_site_ids_by_location() (see R/match_site_ids_by_location.R) to
#' reconcile old design points to current parcel_id based site_ids.

old_design_points <- readr::read_csv("data/design_points.csv", show_col_types = FALSE) |>
  dplyr::mutate(site_id = as.character(site_id))

# check if reconciliation is needed -- current system uses numeric parcel_id
# old formats used hash strings or UniqueIDs that don't match
needs_reconciliation <- !all(old_design_points$site_id %in% site_covariates$site_id)

if (needs_reconciliation) {
  PEcAn.logger::logger.info("Reconciling old site_ids to parcel_id based IDs...")

  reconciled <- update_site_ids_by_location(
    target_df = old_design_points,
    reference_df = site_covariates,
    id_col = "site_id",
    target_lat_col = "lat",
    target_lon_col = "lon",
    reference_lat_col = "lat",
    reference_lon_col = "lon",
    crs = "EPSG:4326",
    max_distance = 500
  )

  # get updated lat/lon from new site_ids
  reconciled <- reconciled |>
    dplyr::select(-lat, -lon) |>
    dplyr::left_join(site_coords, by = "site_id")

  PEcAn.logger::logger.info("Reconciled ", nrow(reconciled), " design points")

  woody_design_points <- reconciled |>
    dplyr::filter(pft == "woody perennial crop") |>
    dplyr::select(site_id, lat, lon, pft)

} else {
  PEcAn.logger::logger.info("Design points already use parcel_id based IDs")
  woody_design_points <- old_design_points |>
    dplyr::filter(pft == "woody perennial crop") |>
    dplyr::select(site_id, lat, lon, pft)
}

# validate woody design points against current v4.1 PFT classification
# parcels may have been reclassified between dataset versions
woody_pft_check <- woody_design_points |>
  dplyr::left_join(
    site_covariates |> dplyr::select(site_id, pft),
    by = "site_id",
    suffix = c("_old", "_current")
  )

woody_pft_mismatch <- woody_pft_check |>
  dplyr::filter(is.na(pft_current) | pft_current != "woody perennial crop")

if (nrow(woody_pft_mismatch) > 0) {
  PEcAn.logger::logger.warn(
    nrow(woody_pft_mismatch), " woody design point(s) reclassified in v4.1 ",
    "Removing: ", paste(woody_pft_mismatch$site_id, collapse = ", ")
  )
  woody_design_points <- woody_pft_check |>
    dplyr::filter(!is.na(pft_current), pft_current == "woody perennial crop") |>
    dplyr::select(site_id, lat, lon, pft = pft_old)
}

PEcAn.logger::logger.info(nrow(woody_design_points), " woody design points retained")

anchor_sites <- readr::read_csv("data/anchor_sites.csv", show_col_types = FALSE) |>
  dplyr::mutate(site_id = as.character(site_id))
woody_anchor_sites <- anchor_sites |>
  dplyr::filter(pft == "woody perennial crop")

# find site_ids in anchor_sites where pft = 'woody perennial crop' but are not in design points
anchor_sites_woody <- anchor_sites |>
  dplyr::filter(pft == "woody perennial crop") |>
  dplyr::filter(!site_id %in% woody_design_points$site_id)

# Handle both old ("herbaceous crop") and new ("annual crop") PFT names
herbaceous_anchor_site_ids <- anchor_sites |>
  dplyr::filter(pft %in% c("herbaceous crop", "annual crop")) |>
  dplyr::select(site_id)

## At some point we will need to do this by PFT
## For now (2a) just clustering herbaceous fields
anchor_sites_for_clust <- herbaceous_anchor_site_ids |>
  dplyr::left_join(site_covariates, by = "site_id")

# validate anchor sites labeled herbaceous/annual in the raw file must also
# have pft == "annual crop" in site_covariates (from CADWR).  Mismatches occur
# when the nearest CADWR polygon has a different land use (e.g. research
# stations classified as nursery, or towers near wetland/crop boundaries).
if (nrow(anchor_sites_for_clust) > 0) {
  pft_mismatch <- anchor_sites_for_clust |>
    dplyr::filter(is.na(pft) | pft != "annual crop")

  if (nrow(pft_mismatch) > 0) {
    PEcAn.logger::logger.warn(
      nrow(pft_mismatch), " anchor site(s) labeled herbaceous/annual in raw ",
      "file but matched to a CADWR field with different PFT. Excluding from ",
      "annual crop clustering: ",
      paste(pft_mismatch$site_id, collapse = ", ")
    )
    anchor_sites_for_clust <- anchor_sites_for_clust |>
      dplyr::filter(!is.na(pft), pft == "annual crop")
  }
}

#' ### Subset LandIQ fields for clustering
#'
#' Now we perform separate clustering for woody perennial and annual crop PFTs
#'
## ----subset-for-clustering----------------------------------------------------
# Define sample size for testing and production
#' For testing / development use 100 design points from the clustered dataset.
#' For high resolution "Production" runs we expect to use approximately 10,000 design points.
#' These will be divided among PFTs proportional to their area

if (!PRODUCTION) {
  sample_size <- 100 # Use 100 for testing
  design_points_per_pft <- 10 # we have 100 for woody; woody ~ 30% of area
} else {
  sample_size <- 100 # Use 10,000 **per PFT** for final production
                     # Until we do a full "PRODUCTION" inventory, keep limited number of sites
  design_points_per_pft <- 100 # we have 100 for woody; woody ~ 30% of area
}

# Randomly sample site_covariates for clustering
pft_to_cluster <- "annual crop"
data_for_clust <- site_covariates |>
  dplyr::filter(pft == pft_to_cluster) |>
  # remove anchor sites
  dplyr::filter(!site_id %in% anchor_sites_for_clust$site_id) |>
  dplyr::sample_n(sample_size - nrow(anchor_sites_for_clust)) |>
  # now add anchor sites back
  dplyr::bind_rows(anchor_sites_for_clust) |>
  dplyr::mutate(
    crop = factor(crop),
    climregion_name = factor(climregion_name)
  ) |>
  dplyr::select(-lat, -lon)

PEcAn.logger::logger.info("Summary of downsampled data to be used for clustering:")
skimr::skim(data_for_clust |> tibble::as_tibble())

if (anyNA(data_for_clust)) {
  PEcAn.logger::logger.error("Error: NA values found in data_for_clust.")
}
#' ### K-means Clustering Function
#'
#' K-means on the numeric columns of covariates
#' Full list printed in message; currently (temp, precip, srad, vapr, clay, ocd, twi)

#' TODO
#' could include 'crop' as categorical with one-hot encoding) i.e.
#' crop1 crop2 crop3
#' 0     1    0
#' 1     0    0
#' by changing function below w/
# perform_clustering <- function(data, k_range = 2:20) {
# Perform one-hot encoding for the 'crop' variable
# encoded_crop <- model.matrix(~ crop - 1, data = data)
# clust_data <- data |>
#   select(where(is.numeric), -ends_with("id")) |>
#   cbind(encoded_crop)
# extract metrics

perform_clustering <- function(data, k_range = 2:20) {
  # Select numeric variables for clustering
  # exclude *_id columns and pft_* one-hot columns (constant within PFT subset -> zero variance)
  clust_data <- data |>
    dplyr::select(where(is.numeric), -ends_with("id"), -starts_with("pft_"))

  PEcAn.logger::logger.info(
    "Columns used for clustering: ",
    paste(names(clust_data), collapse = ", ")
  )
  # Standardize data
  clust_data_scaled <- scale(clust_data)

  # Determine optimal number of clusters using elbow method
  metrics_list <- furrr::future_map(
    k_range,
    function(k) {
      model <- factoextra::hkmeans(clust_data_scaled, k)
      total_withinss <- model$tot.withinss
      sil_score <- mean(cluster::silhouette(model$cluster, dist(clust_data_scaled))[, 3])
      # dunn_index        <- cluster::dunn(clust_data_scaled, model$cluster)
      # calinski_harabasz <- cluster::calinhara(clust_data_scaled, model$cluster)
      list(model = model, total_withinss = total_withinss, sil_score = sil_score)
    },
    .options = furrr::furrr_options(seed = TRUE)
  )

  metrics_df <- data.frame(
    # see also https://github.com/PecanProject/pecan/blob/b5322a0fc62760b4981b2565aabafc07b848a699/modules/assim.sequential/inst/sda_backup/bmorrison/site_selection/pick_sda_sites.R#L221
    k                 = k_range,
    tot.withinss      = purrr::map_dbl(metrics_list, "total_withinss"),
    sil_score         = purrr::map_dbl(metrics_list, "sil_score")
    # dunn_index        = purrr::map_dbl(metrics_list, "dunn_index"),
    # calinski_harabasz = purrr::map_dbl(metrics_list, "calinski_harabasz")
  )

  elbow_k <- pathviewr::find_curve_elbow(
    metrics_df[, c("k", "tot.withinss")],
    export_type = "k" # default uses row number instead of k
  )["k"]

  ## TODO check other metrics (b/c sil and elbow disagree)
  # other metrics
  sil_k <- metrics_df$k[which.max(metrics_df$sil_score)]
  # dunn_k <- metrics_df$k[which.max(metrics_df$dunn_index)]
  # calinski_harabasz_k <- metrics_df$k[which.max(metrics_df$calinski_harabasz)]

  txtplot::txtplot(
    x = metrics_df$k, y = metrics_df$tot.withinss,
    xlab = "k (number of clusters)",
    ylab = "SS(Within)"
  )
  PEcAn.logger::logger.info(
    "Optimal number of clusters according to Elbow Method: ", elbow_k,
    "(where the k vs ss(within) curve starts to flatten.)"
  )

  PEcAn.logger::logger.info("Silhouette scores computed. Higher values indicate better-defined clusters.")
  txtplot::txtplot(
    x = metrics_df$k, y = metrics_df$sil_score,
    xlab = "Number of Clusters (k)", ylab = "Score"
  )
  PEcAn.logger::logger.info(
    "Optimal number of clusters according to Silhouette Method: ", sil_k
  )

  # Perform hierarchical k-means clustering with optimal k
  final_hkmeans <- factoextra::hkmeans(clust_data_scaled, elbow_k)
  clust_data <- cbind(
    site_id = data$site_id,
    clust_data,
    cluster = final_hkmeans$cluster
  )

  return(clust_data)
}

#' ### Perform Clustering
sites_clustered <- perform_clustering(data_for_clust, k_range = 2:20)
#' ### Save Clustering Results For Diagnostics
saveRDS(
  sites_clustered,
  file.path(cache_dir, "sites_clustered.rds")
)

# Subsample new design points from clustered data
herb_design_points_ids <- sites_clustered |>
  # group_by(pft) |>
  dplyr::sample_n(design_points_per_pft) |>
  # ungroup() |>
  dplyr::select(site_id)

# Update design points
herb_design_points <- herb_design_points_ids |>
  dplyr::left_join(site_covariates, by = "site_id") |>
  dplyr::select(site_id, lat, lon, pft) |>
  dplyr::mutate(dplyr::across(c(lat, lon), ~ round(.x, 5))) 

# Combine woody + annual design points
# all site_ids are harmonized parcel_ids
all_design_points <- woody_design_points |>
  dplyr::bind_rows(herb_design_points) |>
  dplyr::select(site_id, lat, lon, pft)

readr::write_csv(all_design_points, here::here("data", "design_points.csv"))

PEcAn.logger::logger.info("design points written to data/design_points.csv")
