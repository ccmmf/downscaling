library(ggplot2)
source("000-config.R")
PEcAn.logger::logger.info("***Preparing anchor sites for California CADWR fields***")

## Anchor Sites
anchor_sites <- readr::read_csv("data_raw/anchor_site_locations.csv")
anchor_sites_pts <- anchor_sites |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  sf::st_transform(crs = ca_albers_crs)


# climregions gpkg is cached by 010; fall back to older name if missing.
climregions_gpkg <- file.path(data_dir, "caladapt_climregions.gpkg")
if (!file.exists(climregions_gpkg)) {
  climregions_gpkg <- file.path(data_dir, "ca_climregions.gpkg")
}
ca_climregions <- sf::st_read(climregions_gpkg, quiet = TRUE) |>
  sf::st_transform(crs = ca_albers_crs)

# map of anchor sites
p <- anchor_sites_pts |>
  ggplot() +
  geom_sf(data = ca_climregions, aes(fill = climregion_name), alpha = 0.25) +
  labs(color = "Climate Region") +
  geom_sf(aes(color = pft)) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "PFT") +
  theme_minimal()
ggsave_optimized("figures/anchor_sites.webp", plot = p, dpi = 96, bg = "white")

# First subset ca_fields to only include those with covariates
cadwr_fields_gpkg <- file.path(data_dir, "cadwr_crops_sites.gpkg")
if (!file.exists(cadwr_fields_gpkg)) {
  PEcAn.logger::logger.severe(
    "CADWR fields not found. Run 009_prepare_cadwr_crops.R first.\n",
    "Expected: ", cadwr_fields_gpkg
  )
}

ca_fields <- sf::st_read(cadwr_fields_gpkg, quiet = TRUE) |>
  sf::st_transform(crs = ca_albers_crs)

site_covariates_csv <- file.path(data_dir, "site_covariates.csv")
site_covariates <- readr::read_csv(site_covariates_csv, show_col_types = FALSE) |>
  dplyr::mutate(site_id = as.character(site_id))

ca_fields_with_covariates <- ca_fields |>
  dplyr::filter(site_id %in% site_covariates$site_id)

# match anchor sites to fields
anchor_sites_with_ids <- match_anchor_sites(
  anchor_sites_pts,
  ca_fields_with_covariates,
  max_dist = units::set_units(250, "m")
)

# Validation checks
# Check for missing site_id, lat, or lon
if (any(is.na(anchor_sites_with_ids |> dplyr::select(site_id, lat, lon)))) {
  PEcAn.logger::logger.warn(
    "Some anchor sites **still** have missing site_id, lat, or lon!"
  )
}

# Check for anchor sites with any covariate missing
missing_cov <- anchor_sites_with_ids |>
  sf::st_drop_geometry() |>
  dplyr::left_join(site_covariates, by = "site_id") |>
  dplyr::select(
    site_id, lat, lon,
    clay, ocd, twi, temp, precip
  ) |>
  dplyr::filter(if_any(everything(), ~ is.na(.x)))

if (nrow(missing_cov) > 0) {
  PEcAn.logger::logger.warn(
    "Some anchor sites have missing environmental covariates!"
  )
}

# Save processed anchor sites
anchor_sites_with_ids |>
  sf::st_drop_geometry() |>
  dplyr::select(site_id, lat, lon, external_site_id, site_name, crops, pft) |>
  dplyr::mutate(across(c(lat, lon), ~ round(.x, 5))) |>
  readr::write_csv("data/anchor_sites.csv")
