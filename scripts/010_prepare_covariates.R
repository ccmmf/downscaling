#' ---
#' title: "Workflow Setup and Data Preparation"
#' author: "David LeBauer"
#' ---
#' 
#' 
#' # Overview
#' 
#' - Prepare Inputs
#'   - Harmonized LandIQ dataset of woody California cropland from 2016-2023
#'   - SoilGrids soil properties (clay, ?)
#'   - CalAdapt climatology (mean annual temperature, mean annual precipitation)
#' - Use LandIQ to query covariates from SoilGrids and CalAdapt and create a table that includes crop type, soil properties, and climatology for each woody crop field
#' 
#' ## TODO
#' - Use consistent projection(s):
#'   - California Albers EPSG:33110 for joins and spatial operations
#'   - WGS84 EPSG:4326 for plotting, subsetting rasters
#' - Clean up domain code
#' - Create a bunch of tables and join all at once at the end
#' - Disambiguate the use of 'ca' in object names; currently refers to both California and Cal-Adapt
#' - decide if we need both Ameriflux shortnames and full names in anchor_sites.csv
#'    (prob. yes, b/c both are helpful); if so, come up w/ better name than 'location'
#' - make sure anchor_sites_ids.csv fields are defined in README 

# TODO move to config.yml / Renviron
source("000-config.R")

PEcAn.logger::logger.info("***Starting Data Preparation***")

## California County Boundaries

ca_counties_gpkg <- file.path(data_dir, "ca_counties.gpkg")
if(!file.exists(ca_counties_gpkg)) {
  ca_counties <- caladaptr::ca_aoipreset_geom("counties") |>
  dplyr::filter(state_name == "California") |>
  dplyr::select(
    county = name,
    state_name = state_name,
    fips = fips,
    geom
  )

ca_climregions_gpkg <- file.path(data_dir, "ca_climregions.gpkg")
if (!file.exists(ca_climregions_gpkg)) {
  ca_climregions <- caladaptr::ca_aoipreset_geom("climregions") |>
    dplyr::select(
      climregion_id = id,
      climregion_name = name,
      geom
    )
  sf::st_write(ca_climregions, ca_climregions_gpkg)
} else {
   ca_climregions <- sf::st_read(ca_climregions_gpkg)
}
  sf::st_write(ca_counties, ca_counties_gpkg, delete_dsn = TRUE)
  PEcAn.logger::logger.info("Created ", ca_counties_gpkg, "from Cal-Adapt counties.")
} else {
  PEcAn.logger::logger.info("Using existing California counties GeoPackage:" , ca_counties_gpkg)
}


### CADWR LandIQ Polygons
# Convert pre-processed LandIQ SHP files to more standardized GeoPackage
# library(PEcAn.data.land)
# input_file <- file.path(raw_data_dir, "i15_Crop_Mapping_2016_SHP/i15_Crop_Mapping_2016.shp")
# ca_fields_gpkg <- file.path(data_dir, "ca_fields.gpkg")
# ca_attributes_csv <- file.path(data_dir, "ca_field_attributes.csv")
# landiq2std(input_file, ca_fields_gpkg, ca_attributes_csv)


#' TODO: update with newer version
ca_fields_gpkg <- file.path(data_dir, 'ca_fields.gpkg')
ca_fields <- sf::st_read(ca_fields_gpkg)

ca_attributes_csv <- file.path(data_dir, 'ca_field_attributes.csv')
ca_attributes <- readr::read_csv(ca_attributes_csv)

#' ### Convert Polygons to Points.

# Using centroids to query raster data.

ca_fields_pts <- ca_fields |>
  dplyr::select(-lat, -lon) |>
  dplyr::left_join(ca_attributes, by = "site_id") |>
  sf::st_centroid() |>
  # and keep only the columns we need
  dplyr::select(site_id, crop, pft, geom)

#' ### Summarize fields by PFT
#' Here we calculate percent (by area and number) of California croplands that are associated 
#' with each PFT in order to estimate the number of design points that will be selected 
#' for in the clustering 
## --- By area & number of fields ---
PEcAn.logger::logger.info("California cropland field number and area by PFT")
ca_fields_pts |>
  dplyr::select(site_id, pft, area_ha) |>
  dtplyr::lazy_dt() |>
  dplyr::group_by(pft) |>
  dplyr::summarize(field_count = dplyr::n(),
                   pft_area = sum(area_ha)) |>
  dplyr::mutate(pft_area_pct = pft_area / sum(pft_area) * 100,
                field_count_pct = field_count / sum(field_count) * 100) |>
  knitr::kable(digits = 0)  

#' ## Assemble Environmental Covariates

#' ### SoilGrids

#' #### Load Prepared Soilgrids GeoTIFF
#' 
#' Using already prepared SoilGrids layers. 
#' TODO: move a copy of these files to data_dir
#' 
## ----load-soilgrids-----------------------------------------------------------
soilgrids_north_america_clay_tif <- '/projectnb/dietzelab/dongchen/anchorSites/NA_runs/soil_nc/soilgrids_250m/clay/clay_0-5cm_mean/clay/clay_0-5cm_mean.tif'
soilgrids_north_america_ocd_tif <- '/projectnb/dietzelab/dongchen/anchorSites/NA_runs/soil_nc/soilgrids_250m/ocd/ocd_0-5cm_mean/ocd/ocd_0-5cm_mean.tif'
## if we want to clip to CA
## use terra to read in that file and then extract values for each location

soilgrids_north_america_clay_rast <- terra::rast(soilgrids_north_america_clay_tif)
soilgrids_north_america_ocd_rast <- terra::rast(soilgrids_north_america_ocd_tif)


#' #### Extract clay and carbon stock from SoilGrids
#' 
## ----sg-clay-ocd--------------------------------------------------------------

clay <- terra::extract(
  soilgrids_north_america_clay_rast,
  terra::vect(ca_fields_pts |>
    sf::st_transform(crs = sf::st_crs(soilgrids_north_america_clay_rast)))) |>
  dplyr::select(-ID) |>
  dplyr::pull() / 10

ocd <- terra::extract(
  soilgrids_north_america_ocd_rast,
  terra::vect(ca_fields_pts |>
    sf::st_transform(crs = sf::st_crs(soilgrids_north_america_ocd_rast)))) |>
  dplyr::select(-ID) |>
  dplyr::pull()

#' ### Topographic Wetness Index
#' 
## ----twi----------------------------------------------------------------------
twi_tiff <- '/projectnb/dietzelab/dongchen/anchorSites/downscale/TWI/TWI_resample.tiff'
twi_rast <- terra::rast(twi_tiff) 

twi <- terra::extract(
  twi_rast,
  terra::vect(ca_fields_pts |>
    sf::st_transform(crs = sf::st_crs(twi_rast)))) |>
  dplyr::select(-ID) |>
  dplyr::pull()
#' 
#' ### ERA5 Met Data
#' 
## -----------------------------------------------------------------------------
era5met_dir <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/GridMET/"

# List all ERA5_met_*.tiff files for years 2012-2021
raster_files <- list.files(
  path = era5met_dir,
  pattern = "^ERA5_met_\\d{4}\\.tiff$",
  full.names = TRUE
)

# Read all rasters into a list of SpatRaster objects
rasters_list <- purrr::map(
  raster_files,
  ~ terra::rast(.x))

years <- purrr::map_chr(rasters_list, ~ {
  source_path <- terra::sources(.x)[1]
  stringr::str_extract(source_path, "\\d{4}")
})  |>
  as.integer()

names(rasters_list) <- years

extract_clim <- function(raster, points_sf) {
  terra::extract(
    raster, 
    points_sf |> 
      sf::st_transform(crs = sf::st_crs(raster))
    ) |>
    tibble::as_tibble() |>
    dplyr::select(-ID) |>
    dplyr::mutate(site_id = points_sf$site_id) |>
    dplyr::select(site_id, temp, prec, srad, vapr)
}

.tmp <-  rasters_list |>
  furrr::future_map_dfr(
    ~ extract_clim(.x, ca_fields_pts),
      .id = "year"
      )

clim_summaries <- .tmp |>
  dplyr::mutate(
    precip = units::ud_convert(prec, "second-1", "year-1")
) |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(
    temp = mean(temp),
    precip = mean(precip),
    srad = mean(srad),
    vapr = mean(vapr)
  )


#' Append CA Climate Region
#'
## Add Climregions
# load climate regions for mapping
#' ### Cal-Adapt Climate Regions

ca_field_climregions <- ca_fields |>
  sf::st_join(
    caladaptr::ca_aoipreset_geom("climregions") |>
      sf::st_transform(crs = ca_albers_crs),
    join = sf::st_within
  ) |>
  dplyr::select(
    site_id,
    climregion_id = id,
    climregion_name = name
  ) |>
  sf::st_drop_geometry()

site_covariates <- cbind(
  ca_fields_pts,
  clay = clay,
  ocd = ocd,
  twi = twi
) |>
  dplyr::inner_join(
    clim_summaries,
    by = "site_id",
    # enforce 1:1 maping
    unmatched = "error",
    relationship = "one-to-one" 
  ) |>
  dplyr::inner_join(
    ca_field_climregions,
    by = "site_id",
    unmatched = "error",
    relationship = "one-to-one"
  ) |>
  na.omit() |>
  dplyr::mutate(
    climregion_id = as.integer(climregion_id),
    across(where(is.numeric), ~ signif(., digits = 3))
  ) |> 
  sf::st_drop_geometry()

PEcAn.logger::logger.info(
  round(100 * (1 - nrow(site_covariates) / nrow(ca_fields)), 0), "% of LandIQ polygons (sites) have at least one missing environmental covariate"
)

readr::write_csv(site_covariates, file.path(data_dir, "site_covariates.csv"))
PEcAn.logger::logger.info(
  "Saved site covariates to ", file.path(data_dir, "site_covariates.csv")
)
