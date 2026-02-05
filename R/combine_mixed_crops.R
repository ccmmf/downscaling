#' Combine two-PFT outputs to represent mixed cropping systems
#'
#' Rules for combining woody and annual PFT outputs (stocks or accumulated
#' flux totals) to represent a mixed cropping system. Supports two methods:
#'  - "weighted": PFTs partition area (woody_cover + annual_cover = 1) and the
#'    output is a weighted average: `woody_cover * woody_value + annual_cover * annual_value`.
#'  - "incremental": preserves the full-area woody baseline (requires `woody_cover == 1`)
#'    and treats annual as an increment relative to an annual initial baseline:
#'    `annual_delta = annual_value - annual_init`; `result = woody_value + annual_cover * annual_delta`.
#'
#' All inputs must be vectors each of length 1 or a shared common length.
#'
#' This function is intended to be applied to pools or integrated fluxes (e.g., annual NPP, annual N2O flux).
#'
#' Validation rules (severe errors unless otherwise noted):
#'  * No input values may be NA (including covers, pool sizes, annual_init if required)
#'  * Covers must lie within [0,1]
#'  * Method "incremental": `woody_cover` must be 1 (within 0.1%); if not, severe error
#'  * Method "incremental": `annual_init` required
#'  * Method "weighted": rows whose `woody_cover + annual_cover` differ from 1 by more than tolerance
#'    are set to NA in the result; a single aggregated warning is emitted listing the count
#'
#' Inputs may be any quantity expressed per unit area (stocks such as
#' kg/m2 or fluxes accumulated over a defined time step, e.g., kg/m2 per
#' hour or year).
#'
#' @param woody_value numeric. Pool or accumulated flux for the woody PFT (kg/m2).
#' @param annual_value numeric. Pool or accumulated flux for the annual PFT (kg/m2).
#' @param annual_init numeric, required for method = "incremental"; the initial annual pool
#'        size at t0 (kg C m-2). Used to compute the delta: (annual_value - annual_init).
#' @param annual_cover numeric. Fractional cover of the annual PFT (0-1).
#' @param woody_cover numeric. Fractional cover of the woody PFT (0-1). Must be 1 when `method` is "incremental".
#' @param method character. One of "weighted" or "incremental".
#'
#' @return numeric vector of combined values.
#'
#' @examples
#' # Discrete mixing (weights sum to 1)
#' combine_mixed_crops(
#'   woody_value = 100, annual_value = 50,
#'   annual_cover = 0.2, woody_cover = 0.8, method = "weighted"
#' )
#'
#' # Overlap: preserve woody baseline (woody_cover==1), add annual increment scaled by cover
#' combine_mixed_crops(
#'   woody_value = 200, annual_value = 220, annual_init = 200,
#'   annual_cover = 0.3, woody_cover = 1.0, method = "incremental"
#' )
combine_mixed_crops <- function(woody_value,
                                annual_value,
                                annual_cover,
                                woody_cover,
                                annual_init = NULL,
                                method = c("weighted", "incremental")) {
  method <- match.arg(method)

  # Internal tolerance for floating point comparisons
  tol <- 1e-3

  # Accept scalars for cover and vectors for values
  recycled <- vctrs::vec_recycle_common(
    woody_value = woody_value,
    annual_value = annual_value,
    annual_cover = annual_cover,
    woody_cover = woody_cover,
    annual_init = annual_init,
    .size = vctrs::vec_size_common(
      woody_value,
      annual_value,
      annual_cover,
      woody_cover,
      annual_init
    )
  )

  woody_value <- recycled$woody_value
  annual_value <- recycled$annual_value
  annual_cover <- recycled$annual_cover
  woody_cover <- recycled$woody_cover
  annual_init <- recycled$annual_init

  # NA checks (annual_init only if required for incremental)
  na_checks <- list(
    woody_value = woody_value,
    annual_value = annual_value,
    annual_cover = annual_cover,
    woody_cover = woody_cover
  )
  if (method == "incremental") {
    if (is.null(annual_init)) {
      PEcAn.logger::logger.severe("incremental: annual_init is required but missing.")
    }
    na_checks$annual_init <- annual_init
  }
  na_found <- vapply(names(na_checks), function(nm) anyNA(na_checks[[nm]]), logical(1))
  if (any(na_found)) {
    fields <- paste(names(na_found)[na_found], collapse = ", ")
    PEcAn.logger::logger.severe(paste0("NA values not allowed in inputs: ", fields))
  }

  # Range checks for covers
  out_of_range_annual <- (annual_cover < -tol) | (annual_cover > 1 + tol)
  out_of_range_woody <- (woody_cover < -tol) | (woody_cover > 1 + tol)
  if (any(out_of_range_annual | out_of_range_woody, na.rm = TRUE)) {
    n_bad <- sum(out_of_range_annual | out_of_range_woody, na.rm = TRUE)
    PEcAn.logger::logger.severe("weighted: cover fractions must be in the range [0,1] (+/- tol). ", n_bad, " rows violate this constraint.")
  }

  if (method == "incremental") {
    not_one <- abs(woody_cover - 1) > tol
    if (any(not_one, na.rm = TRUE)) {
      n_bad <- sum(not_one, na.rm = TRUE)
      PEcAn.logger::logger.severe("incremental: woody_cover must be 1 (+/- ", tol, "); ", n_bad, " rows violate.")
    }
    res <- woody_value + annual_cover * (annual_value - annual_init)
    return(res)
  }

  # weighted method
  sum_cov <- woody_cover + annual_cover
  bad_sum <- abs(sum_cov - 1) > tol | is.na(sum_cov)
  res <- woody_cover * woody_value + annual_cover * annual_value
  if (any(bad_sum, na.rm = TRUE)) {
    n_bad <- sum(bad_sum, na.rm = TRUE)
    PEcAn.logger::logger.warn(paste0("weighted: ", n_bad, " rows with cover fractions not summing to 1 (<U+00B1>tol); results set to NA for those rows."))
    res[bad_sum] <- NA_real_
  }
  return(res)
}
