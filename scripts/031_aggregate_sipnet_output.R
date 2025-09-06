## Simulate multi-PFT scenarios by aggregating SIPNET output from two PFTs
## Two approaches: 
##  - overlap (e.g. orchard + herbaceous ground cover; geometric overlap)
##  - discrete (e.g. annual crop monoculture with hedgerows; partitions area)
##
## Input:
##   ensemble_output.csv (long)
##   site_cover_fractions.csv with columns:
##     site_id, year, woody_cover, annual_cover, scenario  (scenario ∈ {overlap, discrete})
##     For development, a mock grid of (woody_cover, annual_cover, scenario) is generated and 
##     applied to all sites)
##
## Notation:
##   woody_cover  -> f_{woody}
##   annual_cover -> f_{annual}

source("000-config.R")

PEcAn.logger::logger.info("*** Starting multi-PFT aggregation ***")
source(here::here("R", "mixed_aggregation.R"))

# ---- Load ensemble output ----------------------------------------------------
ensemble_output_csv <- file.path(model_outdir, "ensemble_output.csv")
ensemble_data <- readr::read_csv(ensemble_output_csv) |>
  # rename EFI std names for clarity
  # efi name   | new name
  # parameter  | ensemble_id
  # prediction | value
  dplyr::rename(
    ensemble_id = parameter,
    value = prediction
  ) |>
  dplyr::filter(variable %in% c("AGB", "TotSoilCarb"))

# ---- Load or build cover fractions for each year -----------------------------

# the following file will be generated from monitoring workflow
# cover_fractions_csv <- file.path(model_outdir, "site_cover_fractions.csv")

# distinct site-year combinations
distinct_site_year <- ensemble_data |>
  dplyr::mutate(year = lubridate::year(datetime)) |>
  dplyr::distinct(site_id, year) 

# Scenarios for development
# - 100% woody perennial (orchard / monoculture)
# - 100% annual herbaceous (monoculture)
# - Orchard + 25% herbaceous ground cover
# - Orchard + 50% herbaceous ground cover
# - Annual crop + 25% woody hedgerows
# - Annual crop + 50% woody hedgerows
scenarios <- tibble::tibble(
  annual_cover = c(0, 1, 0.25, 0.5, 0.75, 0.5),
  woody_cover =  c(1, 0, 1,    1,   0.25, 0.5),
  mix_description = c(
    "100% woody", "100% annual",
    "100% woody + 25% annual", "100% woody + 50% annual",
    "75% annual + 25% woody", "50% annual + 50% woody"
  ),
  # scenario: use 'overlap' for orchard/ground-cover cases and
  # 'discrete' for annual-with-hedgerow (area-partition) cases
  method = c("weighted", "weighted", "incremental", "incremental", "weighted", "weighted"),
  scenario = c("discrete", "discrete", "overlap", "overlap", "discrete", "discrete")
) |>
  tidyr::crossing(distinct_site_year)

# --- Define output attributes
# pfts
woody_pft <- "woody perennial crop"
annual_pft <- "annual crop"
mixed_overlap_pft <- "woody_annual_overlap_100_50"  # new synthetic PFT label

# ensemble members
ensemble_ids <- unique(ensemble_data$ensemble_id)

# annual_init values for each site x ensemble: value at the earliest datetime
annual_init <- ensemble_data |>
  dplyr::filter(pft == "annual crop") |>
  dplyr::group_by(site_id, variable, ensemble_id) |>
  dplyr::slice_min(order_by = datetime, n = 1, with_ties = FALSE) |>
  dplyr::mutate(
    annual_init = dplyr::case_when(
      # TODO: think through this
      # we want to add AGB to the ecosystem level value
      # for SOC, we only want the diff
      # this probably isn't the best place to store this logic
      # also,  
      variable == "AGB" ~ 0,
      variable == "TotSoilCarb" ~ value
    ) 
  ) |> 
  dplyr::select(site_id, ensemble_id, variable, annual_init)

# ---- Reshape ensemble output (wide by PFT) -----------------------------------
.ens_wide <- ensemble_data |>
  dplyr::mutate(year = lubridate::year(datetime)) |>
  dplyr::select(
    datetime, year, site_id, lat, lon,
    ensemble_id, variable, pft, value
  ) |>
  tidyr::pivot_wider(names_from = pft, values_from = value)

scenarios_x_vars <- scenarios |>
    tidyr::crossing(ensemble_id = ensemble_ids)

ens_wide <- .ens_wide |>
  dplyr::rename(
    woody_value  = !!rlang::sym(woody_pft),
    annual_value = !!rlang::sym(annual_pft)
  ) |>
  # add initial values for annual crop
  dplyr::left_join(annual_init, by = c("site_id", "ensemble_id", "variable")) |>
  # Map each row in .ens_wide to each cover scenario (many-to-many).
  dplyr::left_join(
    scenarios_x_vars,
    by = c("site_id", "year", "ensemble_id"),
    relationship = "many-to-many"
  )

# Diagnostic: find weighted rows where covers are NA or don't sum to ~1
bad_weighted <- ens_wide |>
  dplyr::filter(method == "weighted") |>
  dplyr::mutate(sum_cover = woody_cover + annual_cover) |>
  dplyr::filter(is.na(sum_cover) | abs(sum_cover - 1) > 1e-8)

n_bad <- nrow(bad_weighted)
PEcAn.logger::logger.info("Weighted-scenario cover mismatches: ", n_bad, " rows (out of ", nrow(ens_wide), ")")
if (n_bad > 0) {
  sample_keys <- bad_weighted |>
    dplyr::select(site_id, ensemble_id, variable, year, woody_cover, annual_cover, sum_cover) |>
    dplyr::distinct() |>
    dplyr::slice_head(n = 200)
  readr::write_csv(sample_keys, file.path(model_outdir, "diagnostics_weighted_cover_mismatch_sample.csv"))
  PEcAn.logger::logger.info("Wrote sample diagnostics to diagnostics_weighted_cover_mismatch_sample.csv")
}

# ---- Check for missing values ---------------------------------------
# TBD
if(any(is.na(ens_wide))) {
  PEcAn.logger::logger.severe(
    "Missing values found in ensemble wide data. Examples:\n"
  )
  head(ens_wide[is.na(ens_wide)], 10)
}

# ---- Combine values (row-wise) ----------------------------------------------

ens_combined <- ens_wide |>
  dplyr::rowwise() |>
  dplyr::mutate(
    value_combined = combine_value(
      woody_value  = woody_value,
      annual_value = annual_value,
      annual_init  = ifelse(method == "incremental", annual_init, NULL),
      annual_cover = annual_cover,
      woody_cover  = woody_cover,
      method       = method
    )
  ) 

################################################################################
# ---- Write outputs -----------------------------------------------------------


# 
ens_wide_csv <- file.path(model_outdir, "multi_pft_ensemble_output.csv")
readr::write_csv(ens_wide, ens_wide_csv)
PEcAn.logger::logger.info("Wrote wide diagnostic: ", ens_wide_csv)

ens_combined_csv <- file.path(model_outdir, "combined_ensemble_output.csv")
readr::write_csv(ens_combined, ens_combined_csv)
PEcAn.logger::logger.info("Wrote aggregated output: ", ens_combined_csv)

# ---- Create EFI-compliant ensemble file with mixed overlap PFT ----
# Original EFI-style rows (restore EFI column names)
efi_original <- ensemble_data |>
  dplyr::rename(parameter = ensemble_id, prediction = value)

# Extract the specific overlap scenario: 100% woody, 50% annual (orchard + 50% ground cover)
mixed_overlap_rows <- ens_combined |>
  dplyr::ungroup() |>
  dplyr::filter(
    mix_description == "100% woody + 50% annual"
  ) |>
  dplyr::select(
    datetime, site_id, lat, lon,
    ensemble_id, variable, value_combined
  ) |>
  dplyr::mutate(
    pft = mixed_overlap_pft
  ) |>
  dplyr::rename(
    parameter  = ensemble_id,
    prediction = value_combined
  ) |>
  dplyr::relocate(
    datetime, site_id, lat, lon, pft, parameter, variable, prediction
  )

n_mixed <- nrow(mixed_overlap_rows)
if (n_mixed == 0) {
  PEcAn.logger::logger.severe("No rows produced for mixed overlap PFT; check scenario filters.")
} else {
  PEcAn.logger::logger.info("Produced ", n_mixed, " rows for mixed overlap PFT: ", mixed_overlap_pft)
}

ensemble_with_mixed <- dplyr::bind_rows(efi_original, mixed_overlap_rows)

mixed_output_csv <- file.path(model_outdir, "ensemble_output_with_mixed.csv")
readr::write_csv(ensemble_with_mixed, mixed_output_csv)
PEcAn.logger::logger.info("Wrote EFI-compliant ensemble with mixed PFT: ", mixed_output_csv)

PEcAn.logger::logger.info("*** Finished two-PFT aggregation ***")
