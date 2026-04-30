#### ---- Aggregate to County Level ---- ####
# Inputs: downscaling outputs saved by 040_downscale.R
# Outputs: county_summaries.csv


PEcAn.logger::logger.info("***Starting Aggregation to County Level***")



# Load configuration and paths
source("000-config.R")

downscale_preds_csv <- file.path(model_outdir, "downscaled_preds.csv")
downscale_preds <- vroom::vroom(
  downscale_preds_csv,
  col_types = readr::cols(
    pft = readr::col_character(),
    model_output = readr::col_character(),
    ensemble = readr::col_double(),
    site_id = readr::col_character(),
    county = readr::col_character(),
    area_ha = readr::col_double(),
    density_per_ha = readr::col_double(),
    total_per_field = readr::col_double()
  )
)

ensemble_ids <- unique(downscale_preds$ensemble)


# For testing, sample predictions evenly across counties and pfts
if (!PRODUCTION) {
  # Sample the same site_ids per county across all PFTs
  site_sample <- downscale_preds |>
    dplyr::distinct(county, site_id) |>
    dplyr::group_by(county) |>
    dplyr::slice_sample(n = pmin(10L, dplyr::n())) |>
    dplyr::ungroup()

  downscale_preds <- downscale_preds |>
    dplyr::inner_join(site_sample, by = c("county", "site_id"))
}

### TODO Debug and catch if NAs appears again
na_summary <- downscale_preds |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.x)))) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "column", values_to = "n_na") |>
  dplyr::filter(n_na > 0)

if (nrow(na_summary) > 0) {
  ## concise log message
  PEcAn.logger::logger.warn(
    "YOU NEED TO DEBUG THIS!!!\n\n",
    "NA values detected in `downscale_preds`:\n"
  )
  knitr::kable(na_summary, format = "simple")
  # remove all rows with NA values
  downscale_preds <- tidyr::drop_na(downscale_preds)
}

ens_county_preds <- downscale_preds |>
  dplyr::group_by(model_output, pft, county, ensemble) |>
  dplyr::summarize(
    n = dplyr::n(),
    total_per_county = sum(total_per_field),
    total_ha = sum(area_ha),
    .groups = "drop"
  ) |>
  dplyr::filter(total_ha > 0) |>
  dplyr::mutate(
    density_per_ha = total_per_county / total_ha
  ) |>
  dplyr::arrange(model_output, pft, county, ensemble)

ens_members_by_county <- ens_county_preds |>
  dplyr::group_by(model_output, pft, county) |>
  dplyr::summarize(n_vals = dplyr::n_distinct(total_per_county), .groups = "drop")

if (all(ens_members_by_county$n_vals == length(ensemble_ids))) {
  PEcAn.logger::logger.info("All counties have the correct number of ensemble members: (", length(ensemble_ids), ")")
} else {
  z <- ens_members_by_county |>
    dplyr::group_by(county) |>
    dplyr::summarise(n = mean(n_vals))
  PEcAn.logger::logger.error(
    sum(z$n != length(ensemble_ids)) / length(z$n),
    "counties have the wrong number of ensemble members after downscaling.",
    "Check ens_county_preds object."
  )
}

county_summaries <- ens_county_preds |>
  dplyr::group_by(model_output, pft, county) |>
  dplyr::summarize(
    n = max(n),
    mean_total_per_county = mean(total_per_county),
    sd_total_per_county = sd(total_per_county),
    mean_density_per_ha = mean(density_per_ha),
    sd_density_per_ha = sd(density_per_ha),
    mean_total_ha = mean(total_ha),
    sd_total_ha = sd(total_ha),
    .groups = "drop"
  ) |>
  (
    function(df) {
      if (any(df$n == 1, na.rm = TRUE)) {
        PEcAn.logger::logger.severe(
          "At least one (model_output, pft, county) group has n == 1; variability across ensembles cannot be assessed."
        )
      }
      if (any(df$sd_total_per_county == 0, na.rm = TRUE)) {
        PEcAn.logger::logger.severe(
          "At least one (model_output, pft, county) group has zero variability across ensembles (sd_total_per_county == 0)."
        )
      }
      df
    }
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = c(mean_total_per_county, sd_total_per_county, mean_density_per_ha, sd_density_per_ha),
      .fns = ~ signif(.x, 3)
    )
  )

readr::write_csv(
  county_summaries,
  file.path(model_outdir, "county_summaries.csv")
)
PEcAn.logger::logger.info("County summaries written to", file.path(model_outdir, "county_summaries.csv"))

PEcAn.logger::logger.info(
  "Finished aggregation to county level.\n"
)
