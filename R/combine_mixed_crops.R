#' Combine two-PFT outputs to represent mixed cropping systems
#'
#' Rules for combining woody and annual PFT outputs to represent a
#' mixed cropping system. Supports two methods:
#'  - "weighted": PFTs partition area (woody_cover + annual_cover = 1) and the
#'    output is a weighted average: `woody_cover * woody_value + annual_cover * annual_value`.
#'  - "incremental": preserves the full-area woody baseline (requires `woody_cover == 1`)
#'    and treats annual as an increment relative to an annual initial baseline:
#'    `annual_delta = annual_value - annual_init`; `result = woody_value + annual_cover * annual_delta`.
#'
#' All inputs must be vectors each of length 1 or a shared common length.
#'
#' Validation rules (severe errors unless otherwise noted):
#'  * No input values may be NA (including covers, pool sizes, annual_init if required)
#'  * Covers must lie within [0,1]
#'  * Method "incremental": `woody_cover` must be 1 (within 0.1%); if not, severe error
#'  * Method "incremental": `annual_init` required
#'  * Method "weighted": rows whose `woody_cover + annual_cover` differ from 1 by more than tolerance
#'    are set to NA in the result; a single aggregated warning is emitted listing the count
#'
#' @param woody_value numeric. Pool size for the woody PFT (kg/m2).
#' @param annual_value numeric. Pool size for the annual PFT (kg/m2).
#' @param annual_init numeric, required for method = "incremental"; the initial annual pool.
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
                                annual_init = NULL,
                                annual_cover,
                                woody_cover,
                                method = c("weighted", "incremental")) {
  method <- match.arg(method)


  # Accept scalars for cover and vectors for values
  # Collect inputs for recycling (add annual_init only if provided)
  recycle_inputs <- list(
    woody_value = woody_value,
    annual_value = annual_value,
    annual_cover = annual_cover,
    woody_cover = woody_cover
  )
  if (!is.null(annual_init)) recycle_inputs$annual_init <- annual_init

  recycled <- vctrs::vec_recycle_common(
    !!!recycle_inputs,
    .size = vctrs::vec_size_common(!!!recycle_inputs)
  )

  woody_value <- recycled$woody_value
  annual_value <- recycled$annual_value
  annual_cover <- recycled$annual_cover
  woody_cover <- recycled$woody_cover
  if (!is.null(annual_init)) annual_init <- recycled$annual_init
  # Internal tolerance for floating point comparisons
  tol <- 1e-3

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
  out_of_range_annual <- (annual_cover < 0 - tol) | (annual_cover > 1 + tol)
  out_of_range_woody <- (woody_cover < 0 - tol) | (woody_cover > 1 + tol)
  if (any(out_of_range_annual | out_of_range_woody, na.rm = TRUE)) {
    n_bad <- sum(out_of_range_annual | out_of_range_woody, na.rm = TRUE)
    PEcAn.logger::logger.severe(paste0(n_bad, " rows have cover fractions outside [0,1] (<U+00B1>tol)."))
  }

  if (method == "incremental") {
    not_one <- abs(woody_cover - 1) > tol
    if (any(not_one, na.rm = TRUE)) {
      n_bad <- sum(not_one, na.rm = TRUE)
      PEcAn.logger::logger.severe(paste0("incremental: woody_cover must be 1 (+/- ", tol, "); ", n_bad, " rows violate."))
    }
    res <- woody_value + annual_cover * (annual_value - annual_init)
    return(as.numeric(res))
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
  return(as.numeric(res))
}
