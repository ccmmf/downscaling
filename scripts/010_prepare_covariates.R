#' ---
#' title: "Environmental Covariate Preparation for Downscaling"
#' author: "David LeBauer, Akash B V"
#' ---
#'
#' # Overview
#'
#' This script assembles environmental covariates for California agricultural fields
#' from multiple geospatial data sources:
#'
#' - **LandIQ**: CADWR harmonized crop mapping data (2016-2023)
#' - **SoilGrids**: Soil properties (clay content, organic carbon density)
#' - **ERA5**: Climate normals (temperature, precipitation, solar radiation, vapor pressure)
#' - **TWI**: Topographic Wetness Index
#' - **Cal-Adapt**: California climate regions
#'
#' ## Output
#' Creates `site_covariates.csv` containing environmental attributes for each
#' agricultural field, used by downstream clustering and downscaling workflows.
#'
#' ## Coordinate Reference Systems
#' - EPSG:3310 (NAD83 / California Albers): All spatial joins and operations
#' - EPSG:4326 (WGS84): Output lat/lon coordinates and raster subsetting

# TODO move to config.yml / Renviron
source("000-config.R")
PEcAn.logger::logger.info("*** Starting Environmental Covariate Preparation ***")

# Coordinate Reference systems
CRS_ALBERS <- 3310L   # NAD83 / California Albers - for spatial joins
CRS_WGS84  <- 4326L   # WGS84 - for lat/lon output and raster queries

PEcAn.logger::logger.info("Loading California reference boundaries...")

# California county Boundaries (from Cal-Adapt)
calif_counties_gpkg <- file.path(data_dir, "calif_counties.gpkg")

if (!file.exists(calif_counties_gpkg)) {
  calif_counties <- caladaptr::ca_aoipreset_geom("counties") |>
    dplyr::filter(state_name == "California") |>
    dplyr::select(
      county = name,
      state_name,
      fips,
      geom
    ) |>
    sf::st_transform(CRS_ALBERS)
  
  sf::st_write(calif_counties, calif_counties_gpkg, delete_dsn = TRUE)
  PEcAn.logger::logger.info("Created: ", calif_counties_gpkg)
} else {
  calif_counties <- sf::st_read(calif_counties_gpkg, quiet = TRUE)
  PEcAn.logger::logger.info("Loaded existing: ", calif_counties_gpkg)
}

# California climate regions (from Cal-Adapt)
caladapt_climregions_gpkg <- file.path(data_dir, "caladapt_climregions.gpkg")

if (!file.exists(caladapt_climregions_gpkg)) {
  caladapt_climregions <- caladaptr::ca_aoipreset_geom("climregions") |>
    dplyr::select(
      climregion_id = id,
      climregion_name = name,
      geom
    ) |>
    sf::st_transform(CRS_ALBERS)
  
  sf::st_write(caladapt_climregions, caladapt_climregions_gpkg)
  PEcAn.logger::logger.info("Created: ", caladapt_climregions_gpkg)
} else {
  caladapt_climregions <- sf::st_read(caladapt_climregions_gpkg, quiet = TRUE)
  PEcAn.logger::logger.info("Loaded existing: ", caladapt_climregions_gpkg)
}


PEcAn.logger::logger.info("Loading CADWR LandIQ field data...")

# Load standardized field geometries and attributes
# Primary data source: harmonized multi-year CADWR crops data (2016-2023)
# Prepared by 009_prepare_cadwr_crops.R using standardize_cadwr_crops()
cadwr_sites_gpkg <- file.path(data_dir, "cadwr_crops_sites.gpkg")
cadwr_summary_csv <- file.path(data_dir, "cadwr_crops_site_summary.csv")
cadwr_features_csv <- file.path(data_dir, "cadwr_crops_features.csv")

# Check for harmonized data (required)
if (!file.exists(cadwr_sites_gpkg) || !file.exists(cadwr_summary_csv)) {
  PEcAn.logger::logger.severe(
    "Harmonized CADWR data not found. Run 009_prepare_cadwr_crops.R first.\n",
    "Expected: ", cadwr_sites_gpkg, "\n",
    "Expected: ", cadwr_summary_csv
  )
}

PEcAn.logger::logger.info("Using harmonized CADWR crops data (2016-2023)")

calif_fields <- sf::st_read(cadwr_sites_gpkg, quiet = TRUE) |>
  sf::st_transform(CRS_ALBERS)

calif_field_attributes <- readr::read_csv(cadwr_summary_csv, show_col_types = FALSE) |>
  dplyr::mutate(site_id = as.character(site_id)) |>
  dplyr::rename(
    crop = dominant_crop,
    pft = dominant_pft
  )

# Load EOF features if available (from 009_prepare_cadwr_crops.R)
# These will be merged into final covariates for clustering
eof_features <- NULL
if (file.exists(cadwr_features_csv)) {
  PEcAn.logger::logger.info("Loading EOF features from: ", basename(cadwr_features_csv))
  eof_features <- readr::read_csv(cadwr_features_csv, show_col_types = FALSE) |>
    dplyr::mutate(site_id = as.character(site_id)) |>
    dplyr::select(site_id, starts_with("eof_"), starts_with("pft_"))
}

PEcAn.logger::logger.info(
  "Loaded ", format(nrow(calif_fields), big.mark = ","), " agricultural fields"
)

# Convert field polygons to centroid points for raster extraction
calif_fields_pts <- calif_fields |>
  dplyr::left_join(
    calif_field_attributes |> dplyr::select(site_id, crop, pft, any_of("area_ha")),
    by = "site_id"
  ) |>
  sf::st_centroid() |>
  dplyr::select(site_id, crop, pft, any_of("area_ha"), geom)

# Summarize fields by PFT (for proportional design point allocation)
PEcAn.logger::logger.info("California cropland summary by PFT:")

pft_summary <- calif_fields_pts |>
  sf::st_drop_geometry() |>
  dplyr::group_by(pft) |>
  dplyr::summarize(
    field_count = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    field_pct = round(field_count / sum(field_count) * 100, 1)
  )

print(knitr::kable(pft_summary, format = "simple"))


PEcAn.logger::logger.info("Extracting environmental covariates...")

# SoilGrids: Clay content and Organic carbon density
# Data source: SoilGrids 250m
# Variables: clay (0-5cm mean), ocd (0-5cm mean)
PEcAn.logger::logger.info("Extracting SoilGrids data (clay, organic carbon)...")

soilgrids_clay_tif <- file.path(
  raw_data_dir, "soilgrids_250m",
  "clay", "clay_0-5cm_mean", "clay", "clay_0-5cm_mean.tif"
)

soilgrids_ocd_tif <- file.path(
  raw_data_dir, "soilgrids_250m",
  "ocd", "ocd_0-5cm_mean", "ocd", "ocd_0-5cm_mean.tif"
)

# Load rasters
soilgrids_clay_rast <- terra::rast(soilgrids_clay_tif)
soilgrids_ocd_rast <- terra::rast(soilgrids_ocd_tif)

# Extract values at field centroids
# Note: clay values are in g/kg * 10, so divide by 10 for g/kg
soil_clay <- terra::extract(
  soilgrids_clay_rast,
  terra::vect(sf::st_transform(calif_fields_pts, sf::st_crs(soilgrids_clay_rast)))
) |>
  dplyr::pull(2) / 10  # convert to g/kg

soil_ocd <- terra::extract(
  soilgrids_ocd_rast,
  terra::vect(sf::st_transform(calif_fields_pts, sf::st_crs(soilgrids_ocd_rast)))
) |>
  dplyr::pull(2)

PEcAn.logger::logger.info(
  "  Clay: ", sum(!is.na(soil_clay)), "/", length(soil_clay), " sites extracted"
)

# Topographic wetness index (TWI)
PEcAn.logger::logger.info("Extracting Topographic Wetness Index...")

twi_tif <- "/projectnb/dietzelab/dongchen/anchorSites/downscale/TWI/TWI_resample.tiff"
twi_rast <- terra::rast(twi_tif)

soil_twi <- terra::extract(
  twi_rast,
  terra::vect(sf::st_transform(calif_fields_pts, sf::st_crs(twi_rast)))
) |>
  dplyr::pull(2)

PEcAn.logger::logger.info(
  "  TWI: ", sum(!is.na(soil_twi)), "/", length(soil_twi), " sites extracted"
)

# ERA5 climate data (multi-year means)
# Variables: temp (C), precip (mm/yr), srad (W/m2), vapr (kPa)
PEcAn.logger::logger.info("Extracting ERA5 climate data...")

era5_dir <- "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/GridMET/"

era5_files <- list.files(
  path = era5_dir,
  pattern = "^ERA5_met_\\d{4}\\.tiff$",
  full.names = TRUE
)

if (length(era5_files) == 0) {
  PEcAn.logger::logger.severe("No ERA5 raster files found in: ", era5_dir)
}

# Function to extract climate variables from a single raster
extract_era5_year <- function(raster_file, points_sf) {
  rast <- terra::rast(raster_file)
  year <- stringr::str_extract(basename(raster_file), "\\d{4}")
  
  terra::extract(
    rast,
    terra::vect(sf::st_transform(points_sf, sf::st_crs(rast)))
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      site_id = points_sf$site_id,
      year = as.integer(year)
    ) |>
    dplyr::select(site_id, year, temp, prec, srad, vapr)
}

# Extract climate data for all years in parallel
clim_annual <- furrr::future_map_dfr(
  era5_files,
  ~ extract_era5_year(.x, calif_fields_pts),
  .options = furrr::furrr_options(seed = TRUE)
)

# Compute multi-year climate normals
clim_normals <- clim_annual |>
  dplyr::mutate(
    # Convert precipitation rate (kg/m2/s) to annual total (mm/yr)
    precip_mm_yr = PEcAn.utils::ud_convert(prec, "second-1", "year-1")
  ) |>
  dplyr::group_by(site_id) |>
  dplyr::summarize(
    temp = mean(temp, na.rm = TRUE),
    precip = mean(precip_mm_yr, na.rm = TRUE),
    srad = mean(srad, na.rm = TRUE),
    vapr = mean(vapr, na.rm = TRUE),
    .groups = "drop"
  )

PEcAn.logger::logger.info(
  "  Climate normals computed from ", dplyr::n_distinct(clim_annual$year), " years"
)

# Cal-Adapt climate regions
PEcAn.logger::logger.info("Assigning Cal-Adapt climate regions...")

field_climregions <- calif_fields |>
  sf::st_join(
    caladapt_climregions,
    join = sf::st_within
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(site_id, climregion_id, climregion_name)


# Assemble final covariate table
PEcAn.logger::logger.info("Assembling final site covariate table...")

# Build individual covariate tables for clean joining
soil_covariates <- tibble::tibble(
  site_id = calif_fields_pts$site_id,
  clay = soil_clay,
  ocd = soil_ocd,
  twi = soil_twi
)

# Join all covariates
site_covariates <- calif_fields_pts |>
  sf::st_drop_geometry() |>
  dplyr::select(site_id, crop, pft) |>
  dplyr::inner_join(soil_covariates, by = "site_id") |>
  dplyr::inner_join(clim_normals, by = "site_id") |>
  dplyr::inner_join(field_climregions, by = "site_id")

# Merge EOF features if available
if (!is.null(eof_features)) {
  site_covariates <- site_covariates |>
    dplyr::left_join(eof_features, by = "site_id")
  PEcAn.logger::logger.info("  Merged EOF features")
}

# Remove sites with any missing covariates (except EOF which can be NA)
site_covariates <- site_covariates |>
  tidyr::drop_na(crop, pft, clay, ocd, twi, temp, precip, srad, vapr, climregion_id) |>
  # Round numeric values for cleaner output
  dplyr::mutate(
    climregion_id = as.integer(climregion_id),
    dplyr::across(where(is.numeric), ~ signif(.x, digits = 3))
  )


output_csv <- file.path(data_dir, "site_covariates.csv")
readr::write_csv(site_covariates, output_csv)

PEcAn.logger::logger.info("Saved site covariates to: ", output_csv)
PEcAn.logger::logger.info("*** Covariate preparation complete ***")