# This workflow does the following:
#
# - Use environmental covariates to predict SIPNET estimated SOC for each field in the LandIQ dataset
#   - Uses Random Forest [may change to CNN later] trained on site-scale model runs.
#   - Build a model for each ensemble member
# - Write out a table with predicted biomass and SOC to maintain ensemble structure, ensuring correct error propagation and spatial covariance.
# - Aggregates County-level biomass and SOC inventories
#
## ----setup--------------------------------------------------------------------

source("000-config.R")
PEcAn.logger::logger.info("***Starting Downscaling and Aggregation***")
# library(tidyverse)
# library(sf)
# library(terra)
# library(furrr)
library(patchwork) # for combining plots

ensemble_file <- file.path(model_outdir, "ensemble_output.csv")
ensemble_data <- readr::read_csv(ensemble_file) 
ensemble_ids <- ensemble_data |>
  dplyr::pull(ensemble) |>
  unique()

start_date <- format(as.Date(min(ensemble_data$datetime)), "%Y-%m-%d")
end_date <- format(as.Date(max(ensemble_data$datetime)), "%Y-%m-%d")

### Random Forest using PEcAn downscale workflow
## -----------------------------------------------------------------------------
design_points <- ensemble_data |>
  dplyr::select(site_id, lat, lon, pft) |>
  dplyr::distinct()

covariates_csv <- file.path(data_dir, "site_covariates.csv")
covariates <- readr::read_csv(covariates_csv) |>
  dplyr::select(
    site_id, where(is.numeric),
    -climregion_id
  )

covariate_names <- names(covariates |>
  dplyr::select(where(is.numeric)))

design_covariates <- design_points |>
  dplyr::left_join(covariates, by = "site_id") |>
  dplyr::select(site_id, dplyr::all_of(covariate_names)) |>
  # randomForest pkg requires data frame
  as.data.frame() |>
  # scale covariates as for consistency with model
  dplyr::mutate(dplyr::across(dplyr::all_of(covariate_names), scale))


if (length(setdiff(design_points$site_id, unique(covariates$site_id))) > 0) {
  PEcAn.logger::logger.error("Design points not in covariates:", length(not_in_covariates))
}

# Subset data for testing / speed purposes
if (!PRODUCTION) {
  if (!exists(".Random.seed")) set.seed(123)
  covariates <- covariates |>
    dplyr::anti_join(design_points, by = "site_id") |>
    dplyr::slice_sample(n = 10000) |>
    dplyr::bind_rows(design_covariates) # limit to 10k sites with reproducible sampling
}

## TODO migrate to PEcAnAssimSequential
downscale_model_output <- function(date, model_output) {
  filtered_ens_data <- PEcAnAssimSequential::subset_ensemble(
    ensemble_data = ensemble_data,
    site_coords   = design_points,
    date          = date,
    carbon_pool   = model_output
  )

  # Downscale the data
  downscale_output <- PEcAnAssimSequential::ensemble_downscale(
    ensemble_data = filtered_ens_data,
    site_coords   = design_points,
    covariates    = covariates,
    seed          = 123
  )
  return(downscale_output)
}

# not using furrr b/c it is used inside downscale
downscale_output_list <- purrr::map(
  outputs_to_extract,
  ~ {
    PEcAn.logger::logger.info("Starting downscaling for", .x)
    result <- downscale_model_output(date = end_date, model_output = .x)
    PEcAn.logger::logger.info("Downscaling complete for", .x)
    result
  }
) |>
purrr::set_names(outputs_to_extract)
PEcAn.logger::logger.info("Downscaling complete for all model outputs")

## Check variable importance

## Save to make it easier to restart
# saveRDS(downscale_output, file = here::here("cache/downscale_output.rds"))

PEcAn.logger::logger.info("Downscaling model results for each ensemble member:")
metrics <- lapply(downscale_output_list, PEcAnAssimSequential::downscale_metrics)
for (name in names(metrics)) {
  cat("\n\n###", name)
  print(knitr::kable(metrics[[name]]))
}

median_metrics <- purrr::map(metrics, function(m) {
  m |>
    dplyr::select(-ensemble) |>
    dplyr::summarise( # do equivalent of colmeans but for medians
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = list(median = ~ median(.x)),
        .names = "{col}"
      )
    )
})

PEcAn.logger::logger.info("Median downscaling model metrics:")
dplyr::bind_rows(median_metrics, .id = "model_output") |>
  knitr::kable()

#'
#'
#' ## Aggregate to County Level
#'
## -----------------------------------------------------------------------------

# ca_fields <- readr::read_csv(here::here("data/ca_field_attributes.csv")) |>
#   dplyr::select(id, lat, lon) |>
#   rename(site = id)
PEcAn.logger::logger.info("Aggregating to County Level")

ca_fields_full <- sf::read_sf(file.path(data_dir, "ca_fields.gpkg"))

ca_fields <- ca_fields_full |>
  dplyr::select(site_id, county, area_ha)

if(!PRODUCTION) {
  # For testing, use a subset of fields
  # could be even faster if we queried from gpkg:
  #    sf::read_sf(..., sql = "SELECT * FROM ca_fields WHERE site_id IN (...)")
  ca_fields <- ca_fields |>
    dplyr::right_join(covariates, by = "site_id") 
}

# Convert list to table with predictions and site identifier
get_downscale_preds <- function(downscale_output_list) {
  purrr::map(
    downscale_output_list$predictions,
    ~ tibble::tibble(site_id = covariates$site_id, prediction = .x)
  ) |>
    dplyr::bind_rows(.id = "ensemble") |>
    dplyr::left_join(ca_fields, by = "site_id") 
}

downscale_preds <- purrr::map(downscale_output_list, get_downscale_preds) |>
  dplyr::bind_rows(.id = "model_output") |>
  # Convert kg/m2 to Mg/ha using PEcAn.utils::ud_convert
  dplyr::mutate(c_density_Mg_ha = PEcAn.utils::ud_convert(prediction, "kg/m2", "Mg/ha")) |>
  # Calculate total Mg per field: c_density_Mg_ha * area_ha
  dplyr::mutate(total_c_Mg = c_density_Mg_ha * area_ha)

ens_county_preds <- downscale_preds |>
  # Now aggregate to get county level totals for each pool x ensemble
  dplyr::group_by(model_output, county, ensemble) |>
  dplyr::summarize(
    n = dplyr::n(),
    total_c_Mg = sum(total_c_Mg), # total Mg C per county
    total_ha = sum(area_ha),
    .groups = "drop_last"
  ) |>
  dplyr::ungroup() |>
  # counties with no fields will result in NA below
  dplyr::filter(total_ha > 0) |>
  dplyr::mutate(
    total_c_Tg = PEcAn.utils::ud_convert(total_c_Mg, "Mg", "Tg"),
    mean_c_density_Mg_ha = total_c_Mg / total_ha
  ) |>
  dplyr::arrange(model_output, county, ensemble)

ens_county_preds |>
  dplyr::group_by(model_output, county) |>
  dplyr::summarize(n_vals = dplyr::n_distinct(total_c_Mg), .groups = "drop") |>
  dplyr::pull(n_vals) |>
  unique()

county_summaries <- ens_county_preds |>
  dplyr::group_by(model_output, county) |>
  dplyr::summarize(
    # Number of fields in county should be same for each ensemble member
    n = max(n),
    mean_total_c_Tg = mean(total_c_Tg),
    sd_total_c_Tg = sd(total_c_Tg),
    mean_c_density_Mg_ha = mean(mean_c_density_Mg_ha),
    sd_c_density_Mg_ha = sd(mean_c_density_Mg_ha),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    # Only save 3 significant digits
    dplyr::across(
      .cols = c(mean_total_c_Tg, sd_total_c_Tg, mean_c_density_Mg_ha, sd_c_density_Mg_ha),
      .fns = ~ signif(.x, 3)
    )
  )
    
readr::write_csv(
  county_summaries,
  file.path(model_outdir, "county_summaries.csv")
)
PEcAn.logger::logger.info("County summaries written to", file.path(model_outdir, "county_summaries.csv"))

#### ---Create checkpoint for downstream analysis---####
checkpoint_file <- file.path(cache_dir, "design_checkpoint.RData")
start_end <- system.time(
  save(downscale_output_list, covariates, design_points, design_covariates, ensemble_ids,
    file = checkpoint_file,
    compress = FALSE
  )
)
PEcAn.logger::logger.info(
  "Downscaling output objects saved to", checkpoint_file,
  "\nIt took", round(start_end[3]/60, 2), "minutes"
)

PEcAn.logger::logger.info("***Finished downscaling and aggregation***")
