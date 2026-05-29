#' Match anchor sites to CADWR fields
#'
#' Match anchor sites to CADWR fields using `sf::st_within` to join. For anchor
#' sites that do not fall within a CADWR field with covariates, find and assign
#' the nearest such field using `sf::st_nearest_feature`, and warn about any
#' sites farther than `max_dist` from their assigned field.
#'
#' The nearest fallback searches `ca_fields_with_covariates` (not the full
#' CADWR field set) so an unmatched anchor cannot silently land on a non ag or
#' covariate free field that would later drop at clustering.
#'
#' @param anchor_sites_pts sf POINT layer of anchor sites.
#' @param ca_fields_with_covariates sf POLYGON layer of CADWR fields that have
#'   the covariates required for clustering.
#' @param max_dist numeric maximum allowable distance (in meters) for nearest
#'   match; sites farther than this raise a warning.
#' @return sf POINT layer with `site_id` assigned.
#' @export
match_anchor_sites <- function(
    anchor_sites_pts,
    ca_fields_with_covariates,
    max_dist = units::set_units(250, "m")) {
  assigned <- anchor_sites_pts |>
    sf::st_join(ca_fields_with_covariates, join = sf::st_within)

  unmatched <- assigned |>
    dplyr::filter(is.na(site_id))
  matched <- assigned |>
    dplyr::filter(!is.na(site_id))

  if (nrow(unmatched) > 0) {
    PEcAn.logger::logger.info(
      "Found ", nrow(unmatched), " unmatched anchor sites.",
      "Attempting to match with nearest fields."
    )
    idx <- sf::st_nearest_feature(unmatched, ca_fields_with_covariates)
    nearest_fields <- ca_fields_with_covariates |> dplyr::slice(idx)

    nearest_coords <- nearest_fields |>
      sf::st_transform(4326) |>
      sf::st_centroid() |>
      sf::st_coordinates()

    unmatched_dist <- unmatched |>
      dplyr::mutate(
        site_id = nearest_fields$site_id,
        lon = nearest_coords[, 1],
        lat = nearest_coords[, 2],
        distance_m = sf::st_distance(geometry, nearest_fields, by_element = TRUE)
      )

    PEcAn.logger::logger.info(
      nrow(unmatched_dist),
      "anchor sites assigned to nearest fields."
    )

    far_sites <- unmatched_dist |>
      dplyr::filter(distance_m > max_dist)

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

    assigned <- dplyr::bind_rows(
      matched,
      unmatched_dist |>
        dplyr::select(-distance_m)
    )
  }
  return(assigned)
}
