source("000-config.R")
PEcAn.logger::logger.info("***Starting Clustering Diagnostics***")
library(ggplot2)

ca_fields <- sf::st_read(file.path(data_dir, "ca_fields.gpkg"))
ca_climregions <- sf::st_read(file.path(data_dir, "ca_climregions.gpkg"))

######### Cluster Diagnostics ################

sites_clustered_path <- file.path(cache_dir, "sites_clustered.rds")
if (!file.exists(sites_clustered_path)) {
  PEcAn.logger::logger.severe("Expected clustering output not found:", sites_clustered_path)
}
sites_clustered <- readRDS(sites_clustered_path)
if (!("cluster" %in% names(sites_clustered))) {
  PEcAn.logger::logger.severe("Clustering object lacks 'cluster' column; check upstream clustering step.")
}
# Summarize clusters
cluster_summary <- sites_clustered |>
  dplyr::group_by(cluster) |>
  dplyr::summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

knitr::kable(cluster_summary, digits = 0)

# Plot all pairwise numeric variables

withr::with_seed(42, {
  ggpairs_plot <- sites_clustered |>
    dplyr::select(-site_id) |>
    # need small # pfts for ggpairs
    dplyr::slice_sample(
      n = min(nrow(sites_clustered), 10000)
    ) |>
    GGally::ggpairs(
      # plot all values except site_id and cluster
      columns = setdiff(names(sites_clustered), c("site_id", "cluster")),
      mapping = aes(color = as.factor(cluster), alpha = 0.8)
    ) +
    theme_minimal()
})

ggsave_optimized(
  "figures/cluster_pairs.webp",
  plot = ggpairs_plot,
  width = 10, height = 10, units = "in", dpi = 96
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

ggsave_optimized("figures/cluster_plot.svg", plot = cluster_plot)

#' ### Which covariates define the clusters? (Unsupervised VI)
#'
#' We estimate each variable's contribution to cluster separation using the
#' proportion of variance explained by clusters (eta-squared, <U+03B7><U+00B2>):
#' <U+03B7><U+00B2> = between-cluster variance / total variance. Higher values indicate
#' variables whose means differ more strongly across clusters.

# Compute eta-squared (<U+03B7><U+00B2>) per numeric variable
num_vars <- sites_clustered |>
  dplyr::select(where(is.numeric)) |>
  names()
num_vars <- setdiff(num_vars, c("cluster"))

eta2_tbl <- purrr::map_dfr(num_vars, function(vn) {
  x <- sites_clustered[[vn]]
  cl <- as.factor(sites_clustered$cluster)
  m <- mean(x, na.rm = TRUE)
  # counts and means by cluster (handle NAs per group)
  g_mean <- tapply(x, cl, function(v) mean(v, na.rm = TRUE))
  g_n <- tapply(x, cl, function(v) sum(!is.na(v)))
  N <- sum(!is.na(x))
  total <- stats::var(x, na.rm = TRUE) * max(N - 1, 1)
  between <- sum(g_n * (g_mean - m)^2, na.rm = TRUE)
  eta2 <- ifelse(total > 0, between / total, NA_real_)
  tibble::tibble(variable = vn, eta2 = eta2)
}) |>
  dplyr::arrange(dplyr::desc(eta2))

vi_cluster_plot <- ggplot(eta2_tbl, aes(x = reorder(variable, eta2), y = eta2)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Predictor", y = expression(eta^2 ~ " (between / total variance)"),
    title = "K-means cluster separation by predictor (<U+03B7><U+00B2>)"
  ) +
  theme_minimal()

ggsave_optimized("figures/cluster_variable_importance.svg", plot = vi_cluster_plot)

#'
#' #### Stratification by Crops and Climate Regions
#'
## ----check-stratification-----------------------------------------------------
# Check stratification of clusters by categorical factors

# cols should be character, factor
crop_ids <- readr::read_csv(
  file.path(data_dir, "crop_ids.csv"),
  col_types = readr::cols(
    crop_id = readr::col_factor(),
    crop = readr::col_character()
  )
)

climregion_ids <- readr::read_csv(
  file.path(data_dir, "climregion_ids.csv"),
  col_types = readr::cols(
    climregion_id = readr::col_factor(),
    climregion_name = readr::col_character()
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

design_points <- readr::read_csv(here::here("data", "design_points.csv"))

design_points_clust <- design_points |>
  dplyr::left_join(sites_clustered, by = "site_id") |>
  dplyr::select(site_id, lat, lon, cluster) |>
  tidyr::drop_na(lat, lon) |>
  dplyr::mutate(cluster = as.factor(cluster)) |>
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

ggsave_optimized("figures/design_points.webp", plot = design_pt_plot, width = 10, height = 6, units = "in", dpi = 96, bg = "white")
