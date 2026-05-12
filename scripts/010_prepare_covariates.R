# assemble env covariates per parcel: LandIQ + SoilGrids + ERA5 + TWI + Cal-Adapt.
# writes data/site_covariates.csv for downstream clustering in 020.
# CRS: EPSG:3310 for spatial joins, 4326 for lat/lon output.

source("000-config.R")
PEcAn.logger::logger.info("*** Starting Environmental Covariate Preparation ***")

CRS_ALBERS <- 3310L
CRS_WGS84  <- 4326L

PEcAn.logger::logger.info("Loading California reference boundaries...")

##county boundaries from Cal-Adapt
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

##climate regions from Cal-Adapt
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

##field geometries + attributes from 009
cadwr_sites_gpkg <- file.path(data_dir, "cadwr_crops_sites.gpkg")
cadwr_summary_csv <- file.path(data_dir, "cadwr_crops_site_summary.csv")
cadwr_features_csv <- file.path(data_dir, "cadwr_crops_features.csv")

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
  ) |>
  # drop urban / idle / greenhouse / nursery rows;
  # and the big urban polygons inflate area share by ~33%
  dplyr::filter(!grepl("urban|idle|greenhouse|nursery", crop, ignore.case = TRUE))

##EOF features from 009 (optional, merged later)
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

# polygons -> centroids for raster extraction.
# drop area_ha from attributes side; gpkg already has it,
# otherwise dplyr emits .x / .y suffixes and silently breaks the column
calif_fields_pts <- calif_fields |>
  dplyr::left_join(
    calif_field_attributes |>
      dplyr::select(-dplyr::any_of("area_ha")) |>
      dplyr::select(site_id, crop, pft),
    by = "site_id"
  ) |>
  sf::st_centroid() |>
  dplyr::select(site_id, crop, pft, dplyr::any_of("area_ha"), geom)

# field counts per PFT, for proportional design point allocation in 020
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

##SoilGrids 250m: clay + organic carbon density (0-5cm mean)
PEcAn.logger::logger.info("Extracting SoilGrids data (clay, organic carbon)...")

soilgrids_clay_tif <- file.path(
  raw_data_dir, "soilgrids_250m",
  "clay", "clay_0-5cm_mean", "clay", "clay_0-5cm_mean.tif"
)

soilgrids_ocd_tif <- file.path(
  raw_data_dir, "soilgrids_250m",
  "ocd", "ocd_0-5cm_mean", "ocd", "ocd_0-5cm_mean.tif"
)

soilgrids_clay_rast <- terra::rast(soilgrids_clay_tif)
soilgrids_ocd_rast <- terra::rast(soilgrids_ocd_tif)

# clay stored as g/kg * 10; divide by 10 to get g/kg
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

##topographic wetness index (TWI)
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

##ERA5 multi-year normals: temp (C), precip (mm/yr), srad (W/m2), vpd (kPa)
# raw tiff values are per 3hr step in native ERA5 units (tp m, ssrd J/m2, vapr hPa)
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

# extract climate vars from one year's raster at field centroids
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

# pull all years in parallel
clim_annual <- furrr::future_map_dfr(
  era5_files,
  ~ extract_era5_year(.x, calif_fields_pts),
  .options = furrr::furrr_options(seed = TRUE)
)

# multi-year normals + raw unit conversion to standard physical units.
# tp m/dt -> mm/s via *1000/dt then mm/s -> mm/yr;
# ssrd J/m2/dt -> W/m2 via /dt;
# vpd hPa -> kPa.
TIMESTEP_S <- 3 * 3600  # 8 steps/day for ERA5 ensemble single-levels
clim_normals <- clim_annual |>
  dplyr::mutate(
    precip_mm_yr = PEcAn.utils::ud_convert(prec * 1000 / TIMESTEP_S,
                                           "second-1", "year-1"),
    srad_W_m2    = srad / TIMESTEP_S,
    vpd_kPa      = PEcAn.utils::ud_convert(vapr, "hPa", "kPa")
  ) |>
  dplyr::group_by(site_id) |>
  dplyr::summarize(
    temp   = mean(temp,         na.rm = TRUE),
    precip = mean(precip_mm_yr, na.rm = TRUE),
    srad   = mean(srad_W_m2,    na.rm = TRUE),
    vapr   = mean(vpd_kPa,      na.rm = TRUE),
    .groups = "drop"
  )

PEcAn.logger::logger.info(
  "  Climate normals computed from ", dplyr::n_distinct(clim_annual$year), " years"
)

##assign Cal-Adapt climate region per field
PEcAn.logger::logger.info("Assigning Cal-Adapt climate regions...")

field_climregions <- calif_fields |>
  sf::st_join(
    caladapt_climregions,
    join = sf::st_within
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(site_id, climregion_id, climregion_name)


##assemble final covariate table
PEcAn.logger::logger.info("Assembling final site covariate table...")

soil_covariates <- tibble::tibble(
  site_id = calif_fields_pts$site_id,
  clay = soil_clay,
  ocd = soil_ocd,
  twi = soil_twi
)

site_covariates <- calif_fields_pts |>
  sf::st_drop_geometry() |>
  dplyr::select(site_id, crop, pft, dplyr::any_of("area_ha")) |>
  dplyr::inner_join(soil_covariates, by = "site_id") |>
  dplyr::inner_join(clim_normals, by = "site_id") |>
  dplyr::inner_join(field_climregions, by = "site_id")

if (!is.null(eof_features)) {
  site_covariates <- site_covariates |>
    dplyr::left_join(eof_features, by = "site_id")
  PEcAn.logger::logger.info("  Merged EOF features")
}

# drop rows with any NA core covariate; EOF NAs are tolerated.
# round numeric cols for a cleaner csv.
site_covariates <- site_covariates |>
  tidyr::drop_na(crop, pft, clay, ocd, twi, temp, precip, srad, vapr, climregion_id) |>
  dplyr::mutate(
    climregion_id = as.integer(climregion_id),
    dplyr::across(where(is.numeric), ~ signif(.x, digits = 3))
  )


output_csv <- file.path(data_dir, "site_covariates.csv")
readr::write_csv(site_covariates, output_csv)

PEcAn.logger::logger.info("Saved site covariates to: ", output_csv)
PEcAn.logger::logger.info("*** Covariate preparation complete ***")