source("000-config.R")
library(readr)

######### Cluster Diagnostics ################

sites_clustered <- readRDS(file.path(cache_dir, "sites_clustered.rds"))
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

#' ### Design Point Map
#'
#' Now some analysis of how these design points are distributed
#'
## ----design-point-map---------------------------------------------------------
# plot map of california and climregions

design_points_clust <- design_points |>
  dplyr::left_join(sites_clustered, by = "site_id") |>
  dplyr::select(site_id, lat, lon, cluster) |>
  tidyr::drop_na(lat, lon) |>
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
