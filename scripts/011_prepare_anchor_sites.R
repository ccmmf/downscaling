# must explicitly load for caladaptr functions to access caladapt data
# https://github.com/UCANR-IGIS/caladaptr/issues/5
library(caladaptr)
library(ggplot2)
source("000-config.R")
PEcAn.logger::logger.info("***Preparing anchor sites for California LandIQ fields***")

## Anchor Sites
anchor_sites <- readr::read_csv("data_raw/anchor_site_locations.csv")
anchor_sites_pts <- anchor_sites |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  sf::st_transform(crs = ca_albers_crs)


# create map of anchor sites
ca_climregions <- caladaptr::ca_aoipreset_geom("climregions") |>
  dplyr::rename(climregion_name = name, climregion_id = id)
p <- anchor_sites_pts |>
  ggplot() +
  geom_sf(data = ca_climregions, aes(fill = climregion_name), alpha = 0.25) +
  labs(color = "Climate Region") +
  geom_sf(aes(color = pft)) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "PFT") +
  theme_minimal()
ggsave(p, filename = "figures/anchor_sites.png", dpi = 300, bg = "white")

#' Match anchor sites to LandIQ fields 
#' 
#' Match anchor sites to LandIQ fields using `sf::st_within` to join. 
#' For anchor sites that do not fall within a LandIQ field, 
#' find and assign nearest field using `sf::st_nearest_feature` and
#' warn about any sites > 250m from LandIQ field.
#'
#' @param anchor_sites_pts sf POINT layer of anchor sites
#' @param ca_fields sf POLYGON layer of all LandIQ fields
#' @param ca_fields_with_covariates sf POLYGON layer of LandIQ fields with covariates
#' @param max_dist numeric maximum allowable distance (in meters) for nearest match
#' @return sf POINT layer with `site_id` assigned
match_anchor_sites <- function(
  anchor_sites_pts, 
  ca_fields, 
  ca_fields_with_covariates, 
  max_dist = units::set_units(250, "m")) {
  # within-based join
  assigned <- anchor_sites_pts |>
    sf::st_join(ca_fields_with_covariates, join = sf::st_within)
  
  # identify those still missing
  unmatched <- assigned |>
    dplyr::filter(is.na(site_id))
  matched <- assigned |>
    dplyr::filter(!is.na(site_id))
  
  if (nrow(unmatched) > 0) {
    PEcAn.logger::logger.info(
      "Found ", nrow(unmatched), " unmatched anchor sites.",
      "Attempting to match with nearest fields."
    )
    # nearest-field fallback
    idx <- sf::st_nearest_feature(unmatched, ca_fields)
    nearest_fields <- ca_fields |> dplyr::slice(idx)
    
    # assign values and calculate distances
    unmatched_dist <- unmatched |>
      dplyr::mutate(
        site_id = nearest_fields$site_id,
        lat = nearest_fields$lat,
        lon = nearest_fields$lon,
        distance_m = sf::st_distance(geometry, nearest_fields, by_element = TRUE)
      )
    
    PEcAn.logger::logger.info(
      nrow(unmatched_dist), 
      "anchor sites assigned to nearest fields."
    )
        
    # warn about distant matches
    far_sites <- unmatched_dist |> 
      dplyr::filter(distance_m > max_dist)

    # Report on sites that are > max_dist from their assigned field
    if (nrow(far_sites) == 0) {
      PEcAn.logger::logger.info(
        "All anchor sites assigned to fields within ", 
        max_dist, "m."
      )
    } else if (nrow(far_sites) > 0) {
      PEcAn.logger::logger.warn(
        "The following ", nrow(far_sites),
        " anchor sites assigned to fields more than ",
        max_dist, "m away."
      )
      far_sites |>
        dplyr::select(site_name, distance_m) |>
        sf::st_drop_geometry() |>
        dplyr::mutate(distance_m = signif(distance_m, 2)) |>
        knitr::kable()
    }
    
    # combine matched and unmatched
    assigned <- dplyr::bind_rows(
      matched,
      unmatched_dist |> 
        dplyr::select(-distance_m)
    )
  }
  return(assigned)
}

# First subset ca_fields to only include those with covariates
if (!exists("ca_fields")) {
  ca_fields_gpkg <- file.path(data_dir, "ca_fields.gpkg") 
  ca_fields <- sf::st_read(ca_fields_gpkg) |>
    sf::st_transform(crs = ca_albers_crs)
}
if (!exists("site_covariates")) {
  site_covariates_csv <- file.path(data_dir, "site_covariates.csv")
  site_covariates <- readr::read_csv(site_covariates_csv)
}

ca_fields_with_covariates <- ca_fields |>
  dplyr::filter(site_id %in% site_covariates$site_id)

# match anchor sites to fields
anchor_sites_with_ids <- match_anchor_sites(
  anchor_sites_pts,
  ca_fields,
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
  dplyr::left_join(site_covariates |> sf::st_drop_geometry(), by = "site_id") |>
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
