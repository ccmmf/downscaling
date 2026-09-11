#' Load the LandIQ crop code lookup as a crop -> PFT map
#'
#' The lookup carries both DWR legends: `legend_year == 2016` rows are codes that were
#' renumbered in 2021, `legend_year == 2021` is the current legend. Harmonized LandIQ
#' uses 2021 codes where observed and gap-fills a specific subclass where the 2021
#' legend records only `**`, so both sections are needed to cover the data.
#'
#' @param path Path to LandIQ_cropCode_lookup_table.csv
#' @return tibble of class, subclass, crop, pft
load_landiq_pft_map <- function(path) {
  if (!file.exists(path)) {
    PEcAn.logger::logger.severe("crop code lookup not found: ", path)
  }
  raw <- readr::read_csv(
    path, show_col_types = FALSE, na = c("", "NA"),
    col_types = readr::cols(.default = readr::col_character())
  )
  required <- c("legend_year", "PFT", "CLASS", "SUBCLASS", "SUBCLASS_desc")
  missing_cols <- setdiff(required, names(raw))
  if (length(missing_cols) > 0) {
    PEcAn.logger::logger.severe(
      "crop code lookup missing columns: ", paste(missing_cols, collapse = ", ")
    )
  }

  # non-crop classes (urban, idle, native veg, water) carry pft = NA
  map <- raw |>
    dplyr::transmute(
      class = CLASS,
      subclass = standardize_subclass(SUBCLASS),
      crop = SUBCLASS_desc,
      pft = dplyr::case_when(
        PFT == "woody" ~ "woody perennial crop",
        PFT %in% c("row", "hay", "rice") ~ "annual crop",
        TRUE ~ NA_character_
      )
    )

  # the two legend sections are disjoint on the raw key, but "**" and an explicit
  # class level row both normalize to "0", so collisions are possible. tolerate a
  # duplicate only when it says the same thing; never silently pick one.
  conflict <- map |>
    dplyr::summarize(n_pft = dplyr::n_distinct(pft), .by = c(class, subclass)) |>
    dplyr::filter(n_pft > 1)
  if (nrow(conflict) > 0) {
    PEcAn.logger::logger.severe(
      "crop code lookup maps the same class/subclass to conflicting PFTs: ",
      paste(conflict$class, conflict$subclass, sep = "/", collapse = ", ")
    )
  }

  map <- dplyr::distinct(map, class, subclass, .keep_all = TRUE)
  PEcAn.logger::logger.info(
    "Loaded ", nrow(map), " crop codes; ", sum(!is.na(map$pft)), " with a crop PFT"
  )
  map
}

#' Normalize a LandIQ SUBCLASS column to the PFT table's key
#'
#' SUBCLASS is a double in v4.1 and a character in v4.1.2, where it also carries "**"
#' for a known class with unspecified subclass. Both "**" and NA map to "0", the
#' class-level key in the lookup. Handled explicitly so an unrecognized value fails
#' rather than silently becoming "0".
#'
#' @param x SUBCLASS column, numeric or character
#' @return character vector of subclass keys
standardize_subclass <- function(x) {
  chr <- trimws(as.character(x))
  chr[is.na(chr) | chr == "" | chr == "**"] <- "0"
  num <- suppressWarnings(as.numeric(chr))
  if (any(is.na(num))) {
    PEcAn.logger::logger.severe(
      "unrecognized SUBCLASS values: ",
      paste(unique(chr[is.na(num)]), collapse = ", ")
    )
  }
  as.character(as.integer(num))
}

#' Standardize harmonized LandIQ crops data for downscaling
#'
#' Reads harmonized LandIQ crops parquet and parcels geopackage to produce
#' three outputs consumed by the downscaling pipeline:
#'   - cadwr_crops_sites.gpkg        -- one polygon per field (parcel_id)
#'   - cadwr_crops_attributes.csv    -- per-field-year-season crop records
#'   - cadwr_crops_site_summary.csv  -- one-row-per-field summary
#'
#' Uses parcel_id as stable cross-year site identifier. The harmonized data
#' uses NAs (no sentinel values) and EPSG:3310 coordinates
#'
#' @param input_parquet Path to crops_all_years.parq
#' @param parcels_gpkg Path to parcels-consolidated.gpkg with field polygon geometries
#' @param output_dir Directory for output files (NULL = no write)
#' @param pft_mapping_csv Path to LandIQ_cropCode_lookup_table.csv
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
  # one centroid per parcel, transformed to WGS84 for lat/lon output.
  # data.table on purpose here, per parcel first() over ~600k rows is the
  # one heavy aggregation in this function, the rest stays dplyr/sf
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
  pft_map <- load_landiq_pft_map(pft_mapping_csv)

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
      subclass = standardize_subclass(SUBCLASS),
      pcnt = dplyr::if_else(PCNT == 0 | is.na(PCNT), 100L, as.integer(PCNT)),
      county = COUNTY
    ) |>
    dplyr::left_join(coords_lookup, by = "parcel_id") |>
    dplyr::select(site_id, parcel_id, year, season, lat, lon, county, class, subclass, pcnt)

  # apply PFT mapping
  crops_std <- crops_std |>
    dplyr::left_join(
      pft_map |> dplyr::select(class, subclass, crop, pft),
      by = c("class", "subclass")
    )

  PEcAn.logger::logger.info(
    "Standardized ", format(nrow(crops_std), big.mark = ","), " crop records"
  )

  # -- site summary (one row per parcel) --
  PEcAn.logger::logger.info("Computing site summaries...")

  # pft is NA for non-crop codes (urban, idle, native veg, water). consolidation can
  # merge those into very large polygons, so a parcel counts as cropland only when its
  # cropped records outweigh its uncropped ones, pcnt weighted. ties go to cropland.
  pcnt_by_pft <- crops_std |>
    dplyr::filter(!is.na(pcnt)) |>
    dplyr::summarize(total_pcnt = sum(pcnt), .by = c(site_id, pft))

  cropland <- pcnt_by_pft |>
    dplyr::summarize(
      cropped = sum(total_pcnt[!is.na(pft)]) >= sum(total_pcnt[is.na(pft)]),
      .by = site_id
    ) |>
    dplyr::filter(cropped)

  dominant_pft <- pcnt_by_pft |>
    dplyr::filter(!is.na(pft), site_id %in% cropland$site_id) |>
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

  # per-parcel area from polygon geometry
  parcel_area <- tibble::tibble(
    site_id = parcels$site_id,
    area_ha = as.numeric(sf::st_area(parcels)) / 10000
  )

  site_summary <- site_summary |>
    dplyr::left_join(parcel_area, by = "site_id")

  # join summary attributes to parcel polygons
  # sf geometry is sticky, selecting site_id keeps the geom column
  # regardless of its name (geom / geometry / etc.)
  sites_sf <- parcels |>
    dplyr::select(site_id) |>
    dplyr::inner_join(site_summary, by = "site_id")

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
