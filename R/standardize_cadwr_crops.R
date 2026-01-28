#' Standardize CADWR Crops Data
#'
#' Converts harmonized crops_all_years.csv to standardized format
#' for the downscaling workflow.
#'
#' @param input_csv Path to crops_all_years.csv
#' @param output_dir Directory for output files
#' @param pft_mapping_csv Path to CARB_PFTs_table.csv
#' @param write_outputs Write output files?
#' @return List with sites, attributes, site_summary
#' @export
standardize_cadwr_crops <- function(input_csv,
                                    output_dir = NULL,
                                    pft_mapping_csv = NULL,
                                    write_outputs = TRUE) {

  if (!file.exists(input_csv)) {
    PEcAn.logger::logger.severe("File not found: ", input_csv)
  }

  PEcAn.logger::logger.info("Reading: ", input_csv)
  crops_raw <- data.table::fread(input_csv, showProgress = FALSE)
  PEcAn.logger::logger.info("Loaded ", nrow(crops_raw), " records, ", 
                            dplyr::n_distinct(crops_raw$UniqueID, na.rm = TRUE), " fields")

  # Remove records with NA UniqueID
  crops_raw <- crops_raw[!is.na(UniqueID)]

  # Load PFT mapping
  if (is.null(pft_mapping_csv)) {
    pft_mapping_csv <- file.path(dirname(input_csv), "CARB_PFTs_table.csv")
  }

  pft_map <- NULL
  if (file.exists(pft_mapping_csv)) {
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
  }

  # Get coordinates (EPSG:3857 -> WGS84)
  coords <- crops_raw[!is.na(centx) & !is.na(centy),
    .(centx = first(centx), centy = first(centy)),
    by = UniqueID
  ] |>
    sf::st_as_sf(coords = c("centx", "centy"), crs = 3857) |>
    sf::st_transform(4326)

  coords$lon <- sf::st_coordinates(coords)[, 1]
  coords$lat <- sf::st_coordinates(coords)[, 2]
  coords <- sf::st_drop_geometry(coords)

  # Standardize attributes
  crops_std <- crops_raw[!is.na(CLASS)] |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      site_id = as.character(UniqueID),
      class = CLASS,
      subclass = tidyr::replace_na(as.character(SUBCLASS), "0"),
      pcnt = PCNT,
      county = dplyr::if_else(COUNTY %in% c("", "****") | is.na(COUNTY), NA_character_, COUNTY)
    ) |>
    dplyr::left_join(coords, by = "UniqueID") |>
    dplyr::select(site_id, year, season, lat, lon, county, class, subclass, pcnt)

  # Apply PFT mapping
  if (!is.null(pft_map)) {
    crops_std <- crops_std |>
      dplyr::left_join(
        pft_map |> dplyr::select(class, subclass, crop = crop_desc, pft),
        by = c("class", "subclass")
      )
  } else {
    crops_std$crop <- NA_character_
    crops_std$pft <- NA_character_
  }

  # Site summary
  PEcAn.logger::logger.info("Computing site summaries...")

  # Dominant PFT per site
  dominant_pft <- crops_std |>
    dplyr::filter(!is.na(pft), !is.na(pcnt)) |>
    dplyr::group_by(site_id, pft) |>
    dplyr::summarize(total_pcnt = sum(pcnt), .groups = "drop") |>
    dplyr::group_by(site_id) |>
    dplyr::slice_max(total_pcnt, n = 1, with_ties = FALSE) |>
    dplyr::select(site_id, dominant_pft = pft)

  # Dominant crop per site
  dominant_crop <- crops_std |>
    dplyr::filter(!is.na(crop)) |>
    dplyr::count(site_id, crop) |>
    dplyr::group_by(site_id) |>
    dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
    dplyr::select(site_id, dominant_crop = crop)

  # Site-level stats
  site_summary <- crops_std |>
    dplyr::group_by(site_id) |>
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
      .groups = "drop"
    ) |>
    dplyr::left_join(dominant_pft, by = "site_id") |>
    dplyr::left_join(dominant_crop, by = "site_id")

  PEcAn.logger::logger.info("Created ", nrow(site_summary), " site summaries")

  # Spatial layer
  sites_sf <- site_summary |>
    dplyr::filter(!is.na(lat), !is.na(lon)) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
    sf::st_transform(3310)


  if (write_outputs && !is.null(output_dir)) {
    sites_gpkg <- file.path(output_dir, "cadwr_crops_sites.gpkg")
    if (file.exists(sites_gpkg)) unlink(sites_gpkg)
    sf::st_write(sites_sf, sites_gpkg, quiet = TRUE)

    readr::write_csv(crops_std, file.path(output_dir, "cadwr_crops_attributes.csv"))
    readr::write_csv(site_summary, file.path(output_dir, "cadwr_crops_site_summary.csv"))

    PEcAn.logger::logger.info("Outputs: ", output_dir)
  }

  invisible(list(
    sites = sites_sf,
    attributes = crops_std,
    site_summary = site_summary
  ))
}