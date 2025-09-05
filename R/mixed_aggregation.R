#' Combine two-PFT outputs to represent mixed cropping systems
#'
#' Canonical rules for combining woody and annual PFT outputs. The function
#' implements two scenarios:
#'  - "discrete": the woody and annual covers are assumed to partition the area
#'    (woody_cover + annual_cover must equal 1) and the output is a weighted
#'    average: woody_cover * woody_value + annual_cover * annual_value.
#'  - "overlap": the woody baseline is preserved and the annual contribution is
#'    treated as an increment relative to an initial annual baseline:
#'    annual_delta = annual_value - annual_init, result = woody_value +
#'    annual_cover * annual_delta. In this case annual_init must be provided.
#'
#' Variable classes currently supported include carbon stocks "AGB" and "TotSoilCarb"
#'
#' @param var character. Variable name, must be one of "AGB" or "TotSoilCarb"
#' @param woody_value numeric. Pool size for the woody PFT (kg/m2)
#' @param annual_value numeric. Pool size for the annual PFT (kg/m2)
#' @param annual_init numeric, optional. Required and only used for "overlap"
#'   scenario; the initial conditions used to compute the change
#'   attributed to the annual crop (annual_value - annual_init).
#' @param annual_cover numeric. Fractional cover of the annual PFT (f_annual).
#' @param woody_cover numeric. Fractional cover of the woody PFT (f_woody).
#' @param scenario character. One of "discrete" (default) or "overlap".
#'
#' @details
#' - Discrete: enforces woody_cover + annual_cover == 1 and returns a simple weighted sum.
#' - Overlap: requires annual_init; computes the incremental effect of the annual crop and
#'   adds the increment scaled by annual_cover to the woody baseline.
#'
#' @return Numeric. The combined value according to the selected scenario.
#'
#' @author David LeBauer
#' @examples
#' # Discrete mixing (weights sum to 1)
#' combine_value("AGB", woody_value = 100, annual_value = 50,
#'               annual_cover = 0.2, woody_cover = 0.8, scenario = "discrete")
#'
#' # Overlap: preserve woody baseline, add annual increment scaled by cover
#' combine_value("TotSoilCarb", woody_value = 200,
#'               annual_value = 220, annual_init = 200,
#'               annual_cover = 0.3, woody_cover = 0.9, scenario = "overlap")
combine_value <- function(woody_value, annual_value, annual_init = NULL,
                          annual_cover, woody_cover,
                          method = c("weighted", "incremental")) {
  method <- match.arg(method, choices = c("weighted", "incremental"))

  # Basic NA checks for covers
  if (is.na(woody_cover) || is.na(annual_cover)) {
    PEcAn.logger::logger.warn("One or both cover fractions are NA; returning NA for combine_value.")
    return(NA_real_)
  }

  if (method == "weighted") {
    sum_cov <- woody_cover + annual_cover
    # exact within tolerance -> normal weighted average
    if (abs(sum_cov - 1) <= 1e-8) {
      return(woody_cover * woody_value + annual_cover * annual_value)
    }
  }

  if (method == "incremental") {
    if (is.null(annual_init) || is.na(annual_init)) {
      PEcAn.logger::logger.warn("incremental: annual_init is missing; returning NA.")
      return(NA_real_)
    }
    annual_delta <- annual_value - annual_init
    return(woody_value + annual_cover * annual_delta)
  }
}
