library(ggplot2)
# settings
source("000-config.R")
PEcAn.logger::logger.info("***Starting Clustering and Design Point Selection***")
#' ## Load Site Environmental Data Covariates
#'
#' Environmental data was pre-processed in the previous workflow 00-prepare.qmd.
#'
#' Below is a summary of the covariates dataset
#'
#' - site_id: Unique identifier for each polygon
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
selected_covariates <- c("temp", "precip", "srad", "vapr", "clay", "ocd", "twi")

ca_fields <- sf::st_read(file.path(data_dir, "ca_fields.gpkg"))
ca_climregions <- caladaptr::ca_aoipreset_geom("climregions") |>
  dplyr::rename(climregion_name = name, climregion_id = id)

site_covariates_csv <- file.path(data_dir, "site_covariates.csv")
site_covariates <- readr::read_csv(site_covariates_csv) |>
  dplyr::left_join(
    ca_fields  |> dplyr::select(site_id, lat, lon),
    by = "site_id")


# Check that all selected_covariates are present, otherwise stop.
missing_covariates <- setdiff(selected_covariates, colnames(site_covariates))
if (length(missing_covariates) > 0) {
  PEcAn.logger::logger.error("Missing covariates:", paste(missing_covariates, collapse = ", "))
} else {
  PEcAn.logger::logger.info("All expected covariates are present:", paste(selected_covariates, collapse = ", "))
}

#' ## Load Design Points

# select anchor sites not already in design points
woody_design_points <- readr::read_csv("data/design_points.csv") |>
  dplyr::filter(pft == "woody perennial crop")
anchor_sites <- readr::read_csv("data/anchor_sites.csv")
woody_anchor_sites <- anchor_sites |>
  dplyr::filter(pft == "woody perennial crop")

################################################
### Reconciling site_ids from original woody_design_points_1b with the 
### With new by matching on st_nearest
### This will need to be done once 
### But I'm not doing it now because in Phase 3 we will switch to the LandIQ site_ids

# Convert to sf objects
# woody_design_points_sf <- woody_design_points |>
#   sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
#   sf::st_transform(crs = 3310)
# anchor_sites_sf <- anchor_sites |>
#   sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
#   sf::st_transform(crs = 3310)
# woody_anchor_sites_sf <- anchor_sites_sf |>
#   dplyr::filter(pft == "woody perennial crop")

# nearest_idx <- sf::st_nearest_feature(woody_anchor_sites_sf, woody_design_points_sf)

# # Build lookup table for only anchor_sites_sf that are matched
# site_id_lookup <- tibble::tibble(
#   old_site_id = woody_anchor_sites_sf$site_id,
#   anchor_site_id = woody_design_points_sf$site_id[nearest_idx],
#   distance_m = as.numeric(sf::st_distance(
#     sf::st_geometry(woody_anchor_sites_sf),
#     sf::st_geometry(woody_design_points_sf[nearest_idx, ]),
#     by_element = TRUE
#   ))
# )

# w <- woody_anchor_sites |> 
#   dplyr::left_join(site_id_lookup, by = c("site_id" = "old_site_id")) 

# woody_design_points  |> dplyr::inner_join(w, by = c("site_id" = "anchor_site_id")) 

################################################

# find site_ids in anchor_sites where pft = 'woody perennial crop' but are not in design points
anchor_sites_woody <- anchor_sites |>
  dplyr::filter(pft == "woody perennial crop") |>
  dplyr::filter(!site_id %in% woody_design_points$site_id)

#

herbaceous_anchor_site_ids <- anchor_sites |>
  dplyr::filter(pft == "herbaceous crop") |>
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

# Combine woody + herbaceous design points and write out
all_design_points <- woody_design_points |>
  dplyr::bind_rows(herb_design_points)  
readr::write_csv(all_design_points, here::here("data", "design_points.csv"))

PEcAn.logger::logger.info("design points written to data/design_points.csv")