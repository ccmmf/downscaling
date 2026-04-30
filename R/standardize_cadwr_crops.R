#' Standardize harmonized LandIQ crops data (v4.1) for downscaling
#'
#' Reads harmonized LandIQ crops parquet and parcels geopackage to produce
#' three outputs consumed by the downscaling pipeline:
#'   - cadwr_crops_sites.gpkg        -- one polygon per field (parcel_id)
#'   - cadwr_crops_attributes.csv    -- per-field-year-season crop records
#'   - cadwr_crops_site_summary.csv  -- one-row-per-field summary
#'
#' Uses parcel_id as stable cross-year site identifier. The harmonized data (v4.1)
#' uses NAs (no sentinel values) and EPSG:3310 coordinates
#'
#' @param input_parquet Path to crops_all_years.parq
#' @param parcels_gpkg Path to parcels.gpkg with field polygon geometries
#' @param output_dir Directory for output files (NULL = no write)
#' @param pft_mapping_csv Path to CARB_PFTs_table.csv
#' @param write_outputs Logical; write output files to output_dir?
#' @return Invisible list with sites (sf), attributes (tibble), site_summary (tibble)
#' @export
standardize_cadwr_crops <- function(input_parquet,
                                    parcels_gpkg,
                                    output_dir = NULL,
                                    pft_mapping_csv = NULL,
                                    write_outputs = TRUE) {

  if (!file.exists(input_parquet)) {
    PEcAn.logger::logger.severe("Parquet not found: ", input_parquet)
  }
  if (!file.exists(parcels_gpkg)) {
    PEcAn.logger::logger.severe("Parcels gpkg not found: ", parcels_gpkg)
  }

  # -- read crop records --
  PEcAn.logger::logger.info("Reading: ", input_parquet)
  crops_raw <- arrow::read_parquet(input_parquet) |> data.table::as.data.table()
  PEcAn.logger::logger.info(
    "Loaded ", format(nrow(crops_raw), big.mark = ","), " records, ",
    dplyr::n_distinct(crops_raw$parcel_id), " parcels"
  )

  # -- compute centroids (centx/centy are EPSG:3310) --
  # one centroid per parcel, transformed to WGS84 for lat/lon output
  centroid_dt <- crops_raw[
    !is.na(centx) & !is.na(centy),
    .(centx = data.table::first(centx), centy = data.table::first(centy)),
    by = parcel_id
  ]
  centroid_sf <- sf::st_as_sf(centroid_dt, coords = c("centx", "centy"), crs = 3310L) |>
    sf::st_transform(4326L)

  centroid_dt[, `:=`(
    lon = sf::st_coordinates(centroid_sf)[, 1],
    lat = sf::st_coordinates(centroid_sf)[, 2]
  )]
  coords_lookup <- centroid_dt[, .(parcel_id, lat, lon)]

  # -- load PFT mapping --
  if (is.null(pft_mapping_csv)) {
    PEcAn.logger::logger.severe("pft_mapping_csv is required")
  }
  if (!file.exists(pft_mapping_csv)) {
    PEcAn.logger::logger.severe("PFT mapping not found: ", pft_mapping_csv)
  }

  PEcAn.logger::logger.info("Loading PFT mapping: ", basename(pft_mapping_csv))
  pft_map <- readr::read_csv(pft_mapping_csv, show_col_types = FALSE) |>
    dplyr::select(class = crop_type, subclass = crop_code, crop_desc, pft_group) |>
    dplyr::mutate(
      subclass = as.character(subclass),
      pft = dplyr::case_when(
        pft_group == "woody" ~ "woody perennial crop",
        pft_group == "herbaceous" ~ "annual crop",
        TRUE ~ NA_character_
      )
    )

  # -- standardize crop attributes --
  # parcel_id is the stable cross year identifier (UniqueID varies by year)
  # SUBCLASS is numeric in v4.1; cast to character for PFT join
  # NA subclass -> "0" to match class-level fallback rows in PFT table
  # PCNT 0 means 100% in DWR data (single use field); non-zero = actual %
  PEcAn.logger::logger.info("Standardizing crop attributes...")

  crops_std <- crops_raw[!is.na(CLASS)] |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      site_id = as.character(parcel_id),
      class = CLASS,
      subclass = tidyr::replace_na(as.character(as.integer(SUBCLASS)), "0"),
      pcnt = dplyr::if_else(PCNT == 0 | is.na(PCNT), 100L, as.integer(PCNT)),
      county = COUNTY
    ) |>
    dplyr::left_join(coords_lookup, by = "parcel_id") |>
    dplyr::select(site_id, parcel_id, year, season, lat, lon, county, class, subclass, pcnt)

  # apply PFT mapping
  crops_std <- crops_std |>
    dplyr::left_join(
      pft_map |> dplyr::select(class, subclass, crop = crop_desc, pft),
      by = c("class", "subclass")
    )

  PEcAn.logger::logger.info(
    "Standardized ", format(nrow(crops_std), big.mark = ","), " crop records"
  )

  # -- site summary (one row per parcel) --
  PEcAn.logger::logger.info("Computing site summaries...")

  dominant_pft <- crops_std |>
    dplyr::filter(!is.na(pft), !is.na(pcnt)) |>
    dplyr::summarize(total_pcnt = sum(pcnt), .by = c(site_id, pft)) |>
    dplyr::slice_max(total_pcnt, n = 1, with_ties = FALSE, by = site_id) |>
    dplyr::select(site_id, dominant_pft = pft)

  dominant_crop <- crops_std |>
    dplyr::filter(!is.na(crop)) |>
    dplyr::count(site_id, crop) |>
    dplyr::slice_max(n, n = 1, with_ties = FALSE, by = site_id) |>
    dplyr::select(site_id, dominant_crop = crop)

  site_summary <- crops_std |>
    dplyr::summarize(
      lat = dplyr::first(na.omit(lat)),
      lon = dplyr::first(na.omit(lon)),
      county = {
        x <- county[!is.na(county)]
        if (length(x) == 0) NA_character_ else names(sort(table(x), decreasing = TRUE))[1]
      },
      years_observed = dplyr::n_distinct(year),
      year_min = min(year),
      year_max = max(year),
      n_crops = dplyr::n_distinct(crop, na.rm = TRUE),
      .by = site_id
    ) |>
    dplyr::left_join(dominant_pft, by = "site_id") |>
    dplyr::left_join(dominant_crop, by = "site_id")

  PEcAn.logger::logger.info("Created ", format(nrow(site_summary), big.mark = ","), " site summaries")

  # -- build spatial layer from parcels.gpkg --
  # real polygon geometries instead of centroid points
  PEcAn.logger::logger.info("Loading parcel polygons: ", basename(parcels_gpkg))
  parcels <- sf::st_read(parcels_gpkg, quiet = TRUE) |>
    sf::st_transform(3310L)

  parcels$site_id <- as.character(parcels$parcel_id)

  # join summary attributes to parcel polygons
  sites_sf <- parcels |>
    dplyr::select(site_id, geom) |>
    dplyr::inner_join(site_summary, by = "site_id")

  # compute area from actual geometry
  sites_sf$area_ha <- as.numeric(sf::st_area(sites_sf)) / 10000

  PEcAn.logger::logger.info(
    "Spatial layer: ", format(nrow(sites_sf), big.mark = ","),
    " features in EPSG:3310"
  )

  # -- write outputs --
  if (write_outputs && !is.null(output_dir)) {
    sites_gpkg <- file.path(output_dir, "cadwr_crops_sites.gpkg")
    if (file.exists(sites_gpkg)) unlink(sites_gpkg)
    sf::st_write(sites_sf, sites_gpkg, quiet = TRUE)
    PEcAn.logger::logger.info("Wrote: ", sites_gpkg)

    readr::write_csv(
      crops_std |> dplyr::select(-parcel_id),
      file.path(output_dir, "cadwr_crops_attributes.csv")
    )
    readr::write_csv(site_summary, file.path(output_dir, "cadwr_crops_site_summary.csv"))
    PEcAn.logger::logger.info("Wrote outputs to: ", output_dir)
  }

  invisible(list(
    sites = sites_sf,
    attributes = crops_std,
    site_summary = site_summary
  ))
}
