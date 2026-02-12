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
#' - site_id: Unique identifier for each polygon (LandIQ UniqueID)
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
#' - eof_1 to eof_10: EOF scores capturing cropping system patterns
selected_covariates <- c("temp", "precip", "srad", "vapr", "clay", "ocd", "twi",
                         "eof_1", "eof_2", "eof_3", "eof_4", "eof_5",
                         "eof_6", "eof_7", "eof_8", "eof_9", "eof_10")

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


# Check that all selected_covariates are present, otherwise stop.
missing_covariates <- setdiff(selected_covariates, colnames(site_covariates))
if (length(missing_covariates) > 0) {
  PEcAn.logger::logger.error("Missing covariates:", paste(missing_covariates, collapse = ", "))
} else {
  PEcAn.logger::logger.info("All expected covariates are present:", paste(selected_covariates, collapse = ", "))
}

#' ## Load Design Points and Reconcile site_ids
#' 
#' Phase 3: Convert old hash-based site_ids to LandIQ UniqueIDs

# Load existing design points (may have old hash-based site_ids)
old_design_points <- readr::read_csv("data/design_points.csv", show_col_types = FALSE)

# Check if reconciliation is needed (old format has hash strings, new format is numeric)
needs_reconciliation <- any(!grepl("^\\d+$", na.omit(old_design_points$site_id)))

if (needs_reconciliation) {
  PEcAn.logger::logger.info("Reconciling old site_ids to LandIQ UniqueIDs...")
  
  # Convert old design points to spatial
  old_design_pts_sf <- old_design_points |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(crs = 3310)
  
  # Load new CADWR fields
  ca_fields_albers <- ca_fields |>
    sf::st_transform(crs = 3310)
  
  # Find nearest LandIQ field for each old design point
  nearest_idx <- sf::st_nearest_feature(old_design_pts_sf, ca_fields_albers)
  nearest_fields <- ca_fields_albers[nearest_idx, ]
  
  # Calculate distances to verify matches
  distances <- sf::st_distance(old_design_pts_sf, nearest_fields, by_element = TRUE)
  
  # Build reconciled design points with new LandIQ site_ids
  reconciled_design_points <- old_design_points |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    dplyr::mutate(
      old_site_id = site_id,
      site_id = as.character(nearest_fields$site_id),
      distance_m = as.numeric(distances)
    )
  
  # Warn about distant matches (>100m suggests field boundary changed)
  far_matches <- reconciled_design_points |>
    dplyr::filter(distance_m > 100)
  
  if (nrow(far_matches) > 0) {
    PEcAn.logger::logger.warn(
      nrow(far_matches), " design points matched to fields >100m away. ",
      "Review these manually."
    )
    print(far_matches |> dplyr::select(old_site_id, site_id, lat, lon, distance_m, pft))
  }
  
  # Get lat/lon from new site_ids
  reconciled_design_points <- reconciled_design_points |>
    dplyr::select(-lat, -lon, -distance_m) |>
    dplyr::left_join(site_coords, by = "site_id")
  
  PEcAn.logger::logger.info(
    "Reconciled ", nrow(reconciled_design_points), " design points to LandIQ UniqueIDs"
  )
  
  woody_design_points <- reconciled_design_points |>
    dplyr::filter(pft == "woody perennial crop") |>
    dplyr::select(site_id, lat, lon, pft)
  
} else {
  PEcAn.logger::logger.info("Design points already use LandIQ UniqueIDs")
  woody_design_points <- old_design_points |>
    dplyr::mutate(site_id = as.character(site_id)) |>
    dplyr::filter(pft == "woody perennial crop") |>
    dplyr::select(site_id, lat, lon, pft)
}


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
  clust_data <- data |>
    dplyr::select(where(is.numeric), -ends_with("id"))

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

# Combine woody + herbaceous design points
# All site_ids are now LandIQ UniqueIDs
all_design_points <- woody_design_points |>
  dplyr::bind_rows(herb_design_points) |>
  dplyr::select(site_id, lat, lon, pft)  # Clean output, no UniqueID column

readr::write_csv(all_design_points, here::here("data", "design_points.csv"))

PEcAn.logger::logger.info("design points written to data/design_points.csv")