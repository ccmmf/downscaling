#' ---
#' title: "Cluster and Select Design Points"
#' author: "David LeBauer"
#' ---
#'
#' # Overview
#'
#' This workflow will:
#'
#' - Read in a dataset of site environmental data
#' - Perform K-means clustering to identify clusters
#' - Select design points for each cluster
#' - create design_points.csv and anchor_sites.csv
#'
#'
#' ## Setup
# general utilities
# spatial
# library(sf)
# library(terra)
# library(caladaptr) # to plot climate regions
# # parallel computing
# library(furrr)
# library(doParallel)
# # analysis
# library(cluster)
# library(factoextra)
library(ggplot2)

# settings
source("000-config.R")

#' ## Load Site Environmental Data Covariates
#'
#' Environmental data was pre-processed in the previous workflow 00-prepare.qmd.
#'
#' Below is a sumary of the covariates dataset
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

site_covariates_csv <- file.path(data_dir, "site_covariates.csv")
site_covariates <- readr::read_csv(site_covariates_csv) |>
  dplyr::left_join(ca_fields  |> dplyr::select(site_id, lat, lon), by = "site_id")


# Check that all selected_covariates are present, otherwise stop.
missing_covariates <- setdiff(selected_covariates, colnames(site_covariates))
if (length(missing_covariates) > 0) {
  PEcAn.logger::logger.error("Missing covariates:", paste(missing_covariates, collapse = ", "))
} else {
  PEcAn.logger::logger.info("All expected covariates are present:", paste(selected_covariates, collapse = ", "))
}

#' ## Load Design Points
# readr::read_csv("/usr2/collab/dlebaue1/ccmmf/ccmmf_phase_1b_20250319064759_14859/site_info.csv") |>
#   select(site_id = id, lat, lon) |>
#   mutate(pft = "woody perennial crop") |>
#   mutate(across(c(lat, lon), ~ round(.x, 5))) |>
#   readr::write_csv("data/woody_design_points_1b.csv")

# select anchor sites not already in design points
woody_design_points <- readr::read_csv("data/woody_design_points_1b.csv")
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

sample_size <- 10000 # Use 10,000 **per PFT** for production
design_points_per_pft <- 100 # we have 100 for woody; woody ~ 30% of area

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

# Subsample new design points from clustered data
herb_design_points_ids <- sites_clustered |>
  # group_by(pft) |>
  dplyr::sample_n(design_points_per_pft) |>
  # ungroup() |>
  dplyr::select(site_id)

# Update design points
herb_design_points <- herb_design_points_ids |>
  dplyr::left_join(site_covariates, by = "site_id") |>
  dplyr::select(site_id, lat, lon, pft)

# Combine woody + herbaceous design points and write out
#all_design_points <- woody_design_points |>
#  dplyr::bind_rows(herb_design_points)  
#readr::write_csv(all_design_points, file.path("data", "design_points.csv"))

readr::write_csv(herb_design_points, "data/herbaceous_design_points.csv")
readr::write_csv(woody_design_points, "data/woody_design_points.csv")

PEcAn.logger::logger.info("design points for Herbaceous PFT written to data/herbaceous_design_points.csv")
PEcAn.logger::logger.info("design points for Woody PFT written to data/woody_design_points.csv")


######### Diagnostics ##############

#' ### Check Clustering
#'
## ----check-clustering---------------------------------------------------------
# Summarize clusters
cluster_summary <- sites_clustered |>
  dplyr::group_by(cluster) |>
  dplyr::summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

knitr::kable(cluster_summary, digits = 0)

# Plot all pairwise numeric variables
ggpairs_plot <- sites_clustered |>
  dplyr::select(-site_id) |>
  # need small # pfts for ggpairs
  dplyr::sample_n(1000) |>
  GGally::ggpairs(
    # plot all values except site_id and cluster
    columns = setdiff(names(sites_clustered), c("site_id", "cluster")),
    mapping = aes(color = as.factor(cluster), alpha = 0.8)
  ) +
  theme_minimal()
ggsave(ggpairs_plot,
  filename = "figures/cluster_pairs.png",
  dpi = 300, width = 10, height = 10, units = "in"
)

# scale and reshape to long for plotting

# Normalize the cluster summary data
scaled_cluster_summary <- cluster_summary |>
  dplyr::mutate(across(-cluster, scale)) |>
  tidyr::pivot_longer(
    cols = -cluster,
    names_to = "variable",
    values_to = "value"
  )

cluster_plot <- ggplot(
  scaled_cluster_summary,
  aes(x = factor(variable), y = value)
) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~cluster) +
  coord_flip() +
  labs(x = "Variable", y = "Normalized Value") +
  theme_minimal()

ggsave(cluster_plot, filename = "figures/cluster_plot.png", dpi = 300)

#'
#' #### Stratification by Crops and Climate Regions
#'
## ----check-stratification-----------------------------------------------------
# Check stratification of clusters by categorical factors

# cols should be character, factor
crop_ids <- readr::read_csv(
  file.path(data_dir, "crop_ids.csv"),
  col_types = cols(
    crop_id = col_factor(),
    crop = col_character()
  )
)

climregion_ids <- readr::read_csv(
  file.path(data_dir, "climregion_ids.csv"),
  col_types = cols(
    climregion_id = col_factor(),
    climregion_name = col_character()
  )
)

## ----stratification-----------------------------------------------------------
# The goal here is to check the stratification of the clusters by crop and climregion
# to ensure that the clusters are not dominated by a single crop or climregion
# BUT probably best to normalize by the total number of fields in each cluster
# if this is to be useful
# is this useful?

# ca_attributes <- read_csv(file.path(data_dir, "ca_field_attributes.csv"))
# site_covariates <- read_csv(file.path(data_dir, "site_covariates.csv"))

# sites_clustered <- sites_clustered |>
#   left_join(ca_attributes, by = "site_id") |>
#   left_join(site_covariates, by = "site_id")

# factor_stratification <- list(
#     crop_id = table(sites_clustered$cluster, sites_clustered$crop),
#     climregion_id = table(sites_clustered$cluster, sites_clustered$climregion_name))

# normalize <- function(x) {
#   round(100 * x / rowSums(x), 1)
# }
# normalized_stratification <- lapply(factor_stratification, normalize)
# lapply(normalized_stratification, knitr::kable)

#'
#' ## Design Point Selection
#'
#' For phase 1b we need to supply design points for SIPNET runs.
#' For development we will use 100 design points from the clustered dataset that are _not_ already anchor sites.
#'
#' For the final high resolution runs we expect to use approximately 10,000 design points.
#' For woody croplands, we will start with a number proportional to the total number of sites with woody perennial pfts.
#'
#'

#'
#' ### How Many Design Points?
#'
#' Calculating Woody Cropland Proportion
#'
#' Here we calculate percent of California croplands that are woody perennial crops,
#' in order to estimate the number of design points that will be selected in the clustering step
#' This is commented out because it takes a while and the number won't change
# ca_attributes <- read_csv(file.path(data_dir, "ca_field_attributes.csv"))
## --- By area & number of fields ---
# ca_fields |>
#   left_join(ca_attributes, by = "site_id") |>
#   dplyr::select(site_id, pft, area_ha) |>
#   dtplyr::lazy_dt() |>
#   dplyr::mutate(woody_indicator = ifelse(pft == "woody perennial crop", 1L, 0L)) |>
#   dplyr::group_by(woody_indicator) |>
#   dplyr::summarize(field_count = dplyr::n(),
#                    pft_area = sum(area_ha)) |>
#   dplyr::mutate(pft_area_pct = pft_area / sum(pft_area) * 100,
#                 field_count_pct = field_count / sum(field_count) * 100)

# knitr::kable(pft_area, digits = 0)
# answer: 17% of California croplands were woody perennial crops in the
# 2016 LandIQ dataset
# So ... if we want to ultimately have 2000 design points, we should have ~ 400
# design points for woody perennial crops


#' ### Design Point Map
#'
#' Now some analysis of how these design points are distributed
#'
## ----design-point-map---------------------------------------------------------
# plot map of california and climregions

design_points_clust <- design_points |>
  dplyr::left_join(sites_clustered, by = "site_id") |>
  dplyr::select(site_id, lat, lon, cluster) |>
  dplyr::drop_na(lat, lon) |>
  dplyr::mutate(cluster = as.factor(cluster)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

ca_fields_pts <- ca_fields |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

design_pt_plot <- ggplot() +
  geom_sf(data = ca_climregions, fill = "white") +
  theme_minimal() +
  geom_sf(data = ca_fields, fill = "lightgrey", color = "lightgrey", alpha = 0.25) +
  geom_sf(data = design_points_clust) +
  geom_text(
    data = design_points_clust, aes(label = cluster, geometry = geometry),
    size = 2, stat = "sf_coordinates"
  )

ggsave(design_pt_plot, filename = "figures/design_points.png", dpi = 300, bg = "white")
