# Aggregation to County Level
# Loads downscaling outputs saved by 040_downscale.R and aggregates to counties

# Load configuration and paths
source("000-config.R")

ca_attributes_csv <- file.path(data_dir, "ca_field_attributes.csv")
ca_attributes <- readr::read_csv(ca_attributes_csv)



# Load downscaling checkpoint created by 040_downscale.R
checkpoint_file <- file.path(cache_dir, "downscaling_output.RData")
load(checkpoint_file)

# Ensure required objects are present
required_objs <- c("downscale_output_list", "ensemble_ids", "covariates")
missing_objs <- required_objs[!vapply(required_objs, exists, logical(1))]
if (length(missing_objs) > 0) {
  stop(paste("Missing required objects in checkpoint:", paste(missing_objs, collapse = ", ")))
}

# Load field geometry/attributes needed for aggregation
if (!exists("ca_fields")) {
  ca_fields_full <- sf::read_sf(file.path(data_dir, "ca_fields.gpkg"))
  ca_fields <- ca_fields_full |>
    dplyr::select(site_id, county, area_ha)
}

PEcAn.logger::logger.info("***Starting Aggregation to County Level***")
#### ---- Aggregate to County Level ---- ####
#### TODO Split into separate script?

PEcAn.logger::logger.info("Aggregating to County Level")


if (!PRODUCTION) {
  # For testing, use a subset of fields
  # could be even faster if we queried from gpkg:
  #    sf::read_sf(..., sql = "SELECT * FROM ca_fields WHERE site_id IN (...)")
  ca_fields <- ca_fields |>
    dplyr::right_join(covariates, by = "site_id")
}

# Convert list to table with predictions and site identifier
# Helper: Convert a single downscale object to tidy predictions table
get_downscale_preds <- function(downscale_obj) {
  purrr::map(
    downscale_obj$predictions,
    ~ tibble::tibble(site_id = downscale_obj$site_ids, prediction = .x)
  ) |>
    dplyr::bind_rows(.id = "ensemble") |>
    dplyr::left_join(ca_fields, by = "site_id")
}

# Assemble predictions; carry PFT label by parsing element name: "{pft}::{pool}"
downscale_preds <- purrr::map(downscale_output_list, get_downscale_preds) |>
  dplyr::bind_rows(.id = "spec") |>
  tidyr::separate(
    col = "spec",
    into = c("pft", "model_output"),
    sep = "::",
    remove = TRUE
  ) |>
  # Convert kg/m2 to Mg/ha using PEcAn.utils::ud_convert
  dplyr::mutate(c_density_Mg_ha = PEcAn.utils::ud_convert(prediction, "kg/m2", "Mg/ha")) |>
  # Calculate total Mg per field: c_density_Mg_ha * area_ha
  dplyr::mutate(total_c_Mg = c_density_Mg_ha * area_ha)

## Write out downscaled predictions

readr::write_csv(
  downscale_preds,
  file.path(model_outdir, "downscaled_preds.csv")
)

### TODO Debug and catch if it appears again
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
  # Now aggregate to get county level totals for each pool x ensemble
  dplyr::group_by(model_output, pft, county, ensemble) |>
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
    c_density_Mg_ha = total_c_Mg / total_ha
  ) |>
  dplyr::arrange(model_output, pft, county, ensemble)

ens_members_by_county <- ens_county_preds |>
  dplyr::group_by(model_output, pft, county) |>
  dplyr::summarize(n_vals = dplyr::n_distinct(total_c_Mg), .groups = "drop")

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
    # Number of fields in county should be same for each ensemble member
    n = max(n),
    mean_total_c_Tg = mean(total_c_Tg),
    sd_total_c_Tg = sd(total_c_Tg),
    mean_c_density_Mg_ha = mean(c_density_Mg_ha),
    sd_c_density_Mg_ha = sd(c_density_Mg_ha),
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

PEcAn.logger::logger.info(
  rep("<U+0001F31F><U+0001F31F><U+0001F31F>  ", 5), "\n\n",
  "Finished aggregation to County level", "\n\n",
  rep("<U+0001F31F><U+0001F31F><U+0001F31F>  ", 5)
)
