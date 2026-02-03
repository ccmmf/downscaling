#' Match site IDs by ID, then nearest location (or location only)
#'
#' By default, preserves any exact ID matches between `target_df` and `reference_df`,
#' and for targets whose IDs are not present in `reference_df`, matches the nearest
#' reference site by location. Optionally, set `prefer_location = TRUE` to ignore
#' IDs entirely and match every target to its nearest reference by location.
#' Returns one row per target with: target_site_id, matched_site_id, coordinates,
#' distance_m, and a coarse proximity class.
#'
#' @param target_df data.frame with at least id/lat/lon columns
#' @param reference_df data.frame with at least id/lat/lon columns
#' @param target_id_col character. ID column in target_df (default "site_id")
#' @param reference_id_col character. ID column in reference_df (default "site_id")
#' @param target_lat_col character. Latitude column in target_df (default "lat")
#' @param target_lon_col character. Longitude column in target_df (default "lon")
#' @param reference_lat_col character. Latitude column in reference_df (default "lat")
#' @param reference_lon_col character. Longitude column in reference_df (default "lon")
#' @param crs character. CRS of the INPUT coordinates (default "EPSG:4326").
#'        This must reflect how your `lat`/`lon` columns are expressed:
#'        use a geographic CRS like "EPSG:4326" when coordinates are decimal degrees;
#'        only use a projected CRS (e.g., "EPSG:3310") if your coordinates are
#'        already in that projection and in linear units. This function does not
#'        reproject the input; set the CRS to what it is. If you need to transform
#'        coordinates, do so prior to calling (e.g., with `terra::project`).
#' @param prefer_location logical. If TRUE, ignore IDs and match all targets by
#'        nearest location (location-only mode). If FALSE (default), first match
#'        by ID and only compute nearest for targets whose IDs are missing.
#' @param map_all logical. If TRUE, compute nearest distances for all target rows.
#'        If FALSE, only compute nearest distances for IDs missing from reference;
#'        ID-matched rows are returned with distance 0. Note: `map_all` does not
#'        change which ID is returned; it only controls distance calculations.
#' @param max_distance numeric. Maximum allowable distance (m) for a match; error if exceeded (default 100 m).
#' @return a tibble with mapping and distances (same number of rows as target_df)
match_site_ids_by_location <- function(
    target_df,
    reference_df,
    target_id_col = "site_id",
    reference_id_col = "site_id",
    target_lat_col = "lat",
    target_lon_col = "lon",
    reference_lat_col = "lat",
    reference_lon_col = "lon",
    crs = "EPSG:4326",
    prefer_location = FALSE,
    map_all = FALSE,
    max_distance = 100) {
  # Validate columns
  req_target <- c(target_id_col, target_lat_col, target_lon_col)
  req_ref <- c(reference_id_col, reference_lat_col, reference_lon_col)

  if (!all(req_target %in% colnames(target_df))) {
    PEcAn.logger::logger.error(
      "target_df is missing required columns: ",
      paste(setdiff(req_target, colnames(target_df)), collapse = ", ")
    )
  }
  if (!all(req_ref %in% colnames(reference_df))) {
    PEcAn.logger::logger.error(
      "reference_df is missing required columns: ",
      paste(setdiff(req_ref, colnames(reference_df)), collapse = ", ")
    )
  }

  # Annotate rows and, unless in location-only mode, split by ID membership
  by_id <- stats::setNames(reference_id_col, target_id_col)
  target_df <- target_df |>
    dplyr::mutate(`..row..` = dplyr::row_number())

  matched_id <- NULL
  mismatched_id <- target_df
  if (!isTRUE(prefer_location)) {
    matched_id <- target_df |>
      dplyr::inner_join(reference_df, by = by_id, suffix = c(".t", ".r"))

    mismatched_id <- target_df |>
      dplyr::anti_join(reference_df, by = by_id)

    n_needs <- nrow(mismatched_id)
    if (n_needs == 0) {
      PEcAn.logger::logger.info("All target IDs found in reference by ID.")
    } else {
      PEcAn.logger::logger.warn(
        paste(n_needs, "target sites not in reference by ID; matching by nearest location.")
      )
    }
  } else {
    PEcAn.logger::logger.info("prefer_location=TRUE: matching ALL targets to nearest reference by location (ignoring IDs).")
  }

  # Compute nearest for mismatches (always)
  mapping_miss <- NULL
  if (nrow(mismatched_id) > 0) {
    tgt_vect_miss <- terra::vect(
      mismatched_id,
      geom = c(target_lon_col, target_lat_col),
      crs = crs
    )
    ref_vect <- terra::vect(
      reference_df,
      geom = c(reference_lon_col, reference_lat_col),
      crs = crs
    )
    nearest_site <- terra::nearest(tgt_vect_miss, ref_vect)
    idx <- nearest_site$to_id
    dist_m <- nearest_site$distance

    mapping_miss <- tibble::tibble(
      `..row..` = mismatched_id$`..row..`,
      target_site_id = mismatched_id[[target_id_col]],
      matched_site_id = reference_df[[reference_id_col]][idx],
      target_lat = mismatched_id[[target_lat_col]],
      target_lon = mismatched_id[[target_lon_col]],
      ref_lat = reference_df[[reference_lat_col]][idx],
      ref_lon = reference_df[[reference_lon_col]][idx],
      distance_m = dist_m
    )
  }

  # Build mapping for matched-by-ID rows (skipped in location-only mode)
  mapping_match <- NULL
  if (!isTRUE(prefer_location) && nrow(matched_id) > 0) {
    # Prepare base table
    mapping_match <- tibble::tibble(
      `..row..` = matched_id$`..row..`,
      # join key keeps one column name (no suffix) <U+2013> use it for both
      target_site_id = matched_id[[target_id_col]],
      matched_site_id = matched_id[[target_id_col]],
      target_lat = matched_id[[paste0(target_lat_col, ".t")]],
      target_lon = matched_id[[paste0(target_lon_col, ".t")]],
      ref_lat = matched_id[[paste0(reference_lat_col, ".r")]],
      ref_lon = matched_id[[paste0(reference_lon_col, ".r")]]
    )

    # Distances for matched rows
    if (isTRUE(map_all)) {
      tgt_pts <- terra::vect(mapping_match, geom = c("target_lon", "target_lat"), crs = crs)
      ref_pts <- terra::vect(mapping_match, geom = c("ref_lon", "ref_lat"), crs = crs)
      dvec <- vapply(seq_len(nrow(mapping_match)), function(i) as.numeric(terra::distance(tgt_pts[i], ref_pts[i])), numeric(1))
      mapping_match$distance_m <- dvec
    } else {
      mapping_match$distance_m <- 0
    }
  }

  # Combine matched and mismatched, preserve original row order
  mapping <- dplyr::bind_rows(mapping_match, mapping_miss) |>
    dplyr::arrange(`..row..`) |>
    dplyr::mutate(
      close = dplyr::case_when(
        distance_m <= 10 ~ "same location",
        distance_m <= 100 ~ "very close (<=100m)",
        distance_m <= 500 ~ "close (100-500m)",
        distance_m <= 1000 ~ "moderate (500-1000m)",
        distance_m <= 5000 ~ "far (1000-5000m)",
        TRUE ~ "far (>5000m)"
      )
    ) |>
    dplyr::select(-`..row..`) |>
    dplyr::distinct()

  # Enforce maximum allowable distance
  if (any(mapping$distance_m > max_distance, na.rm = TRUE)) {
    n_exceed <- sum(mapping$distance_m > max_distance, na.rm = TRUE)
    PEcAn.logger::logger.severe(
      n_exceed, " matched target rows exceed max_distance of ", max_distance, " m"
    )
  }

  return(mapping)
}

#' Update site IDs in a target data.frame by nearest reference site
#'
#' If some target IDs don't exist in reference, update them to the nearest reference IDs by location.
#' If all target IDs exist in reference, returns the original target_df unchanged.
#'
#' @param target_df data.frame
#' @param reference_df data.frame
#' @inheritParams match_site_ids_by_location
#' @return data.frame with potentially updated ID column
update_site_ids_by_location <- function(
    target_df,
    reference_df,
    id_col = "site_id",
    target_lat_col = "lat",
    target_lon_col = "lon",
    reference_id_col = "site_id",
    reference_lat_col = "lat",
    reference_lon_col = "lon",
    crs = "EPSG:4326",
    max_distance = 100) {
  mapping <- match_site_ids_by_location(
    target_df = target_df,
    reference_df = reference_df,
    target_id_col = id_col,
    reference_id_col = reference_id_col,
    target_lat_col = target_lat_col,
    target_lon_col = target_lon_col,
    reference_lat_col = reference_lat_col,
    reference_lon_col = reference_lon_col,
    crs = crs,
    map_all = FALSE,
    max_distance = max_distance
  )

  # Replace IDs where mapping exists
  tdf <- target_df
  orig_id_col <- id_col
  # unify id column name for join
  if (orig_id_col != "site_id") {
    names(tdf)[names(tdf) == orig_id_col] <- "site_id"
  }

  updated <- tdf |>
    dplyr::left_join(
      mapping |>
        dplyr::select(target_site_id, matched_site_id),
      by = c("site_id" = "target_site_id")
    ) |>
    dplyr::mutate(site_id = dplyr::if_else(is.na(matched_site_id), site_id, matched_site_id)) |>
    dplyr::select(-matched_site_id)

  # rename back to original id name if needed
  if (orig_id_col != "site_id") {
    names(updated)[names(updated) == "site_id"] <- orig_id_col
  }

  return(updated)
}
