#### ---- Aggregate to County and State Level ---- ####
# Inputs: downscaling outputs saved by 040_downscale.R
# Outputs:
#   - county_aggregated_preds.csv: County-level summaries by scenario
#   - county_aggregated_deltas.csv: County-level delta summaries by scenario
#   - state_summaries.csv: State-level totals by scenario
#   - aggregation_metadata.json: Metadata for aggregated outputs

PEcAn.logger::logger.info("***Starting Aggregation to County and State Level***")

# Load configuration and paths
source("000-config.R")

# ---- Helper function for writing outputs ----
write_output <- function(data, path_base, description = "data") {
  csv_path <- paste0(path_base, ".csv")
  readr::write_csv(data, csv_path)
  PEcAn.logger::logger.info(description, " written to ", csv_path)
  
  # write parquet for large files (>100k rows) when arrow is available
  if (nrow(data) > 100000 && requireNamespace("arrow", quietly = TRUE)) {
    parquet_path <- paste0(path_base, ".parquet")
    arrow::write_parquet(data, parquet_path)
    PEcAn.logger::logger.info(description, " (parquet) written to ", parquet_path)
  }
}

# ---- Load field-level predictions ----
downscale_preds_csv <- file.path(model_outdir, "downscaled_preds.csv")
downscale_preds <- vroom::vroom(
  downscale_preds_csv,
  col_types = readr::cols(
    scenario = readr::col_character(),
    pft = readr::col_character(),
    model_output = readr::col_character(),
    ensemble = readr::col_double(),
    site_id = readr::col_character(),
    county = readr::col_character(),
    area_ha = readr::col_double(),
    c_density_Mg_ha = readr::col_double(),
    total_c_Mg = readr::col_double()
  )
)

# Backward compatibility: add scenario column if missing
if (!"scenario" %in% names(downscale_preds)) {
  downscale_preds <- downscale_preds |>
    dplyr::mutate(scenario = "baseline")
  PEcAn.logger::logger.info("No scenario column found; using 'baseline' for backward compatibility")
}

ensemble_ids <- unique(downscale_preds$ensemble)
scenarios <- unique(downscale_preds$scenario)

PEcAn.logger::logger.info(
  "Loaded predictions: ", nrow(downscale_preds), " rows; ",
  dplyr::n_distinct(downscale_preds$site_id), " sites; ",
  length(ensemble_ids), " ensembles; ",
  length(scenarios), " scenarios"
)

# For testing, sample predictions evenly across counties and pfts
if (!PRODUCTION) {
  # Sample up to 10 site_ids per county (slice_sample handles groups with <10 rows)
  site_sample <- downscale_preds |>
    dplyr::distinct(county, site_id) |>
    dplyr::group_by(county) |>
    dplyr::slice_sample(n = 10) |>
    dplyr::ungroup()

  downscale_preds <- downscale_preds |>
    dplyr::inner_join(site_sample, by = c("county", "site_id"))
}

# ---- Check for NA values ----
na_summary <- downscale_preds |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.x)))) |>
  tidyr::pivot_longer(dplyr::everything(), names_to = "column", values_to = "n_na") |>
  dplyr::filter(n_na > 0)

if (nrow(na_summary) > 0) {
  PEcAn.logger::logger.warn(
    "NA values detected in `downscale_preds`:\n",
    paste(capture.output(knitr::kable(na_summary, format = "simple")), collapse = "\n")
  )
  downscale_preds <- tidyr::drop_na(downscale_preds)
}

# ---- Aggregate to county level per ensemble ----
ens_county_preds <- downscale_preds |>
# Now aggregate to get county level totals for each pool x ensemble
  dplyr::group_by(scenario, model_output, pft, county, ensemble) |>
  dplyr::summarize(
    n = dplyr::n(),
    total_c_Mg = sum(total_c_Mg), # total Mg C per county
    total_ha = sum(area_ha),
    .groups = "drop"
  ) |>
  dplyr::filter(total_ha > 0) |>
  dplyr::mutate(
    total_c_Tg = PEcAn.utils::ud_convert(total_c_Mg, "Mg", "Tg"),
    c_density_Mg_ha = total_c_Mg / total_ha
  ) |>
  dplyr::arrange(scenario, model_output, pft, county, ensemble)

# ---- Validate ensemble counts ----
ens_members_by_county <- ens_county_preds |>
  dplyr::group_by(scenario, model_output, pft, county) |>
  dplyr::summarize(n_ens = dplyr::n_distinct(ensemble), .groups = "drop")

expected_ens <- length(ensemble_ids)
mismatched <- ens_members_by_county |> dplyr::filter(n_ens != expected_ens)
if (nrow(mismatched) == 0) {
  PEcAn.logger::logger.info("All county/scenario combinations have ", expected_ens, " ensemble members")
} else {
  PEcAn.logger::logger.warn(
    nrow(mismatched), " county/scenario combinations have wrong ensemble count. ",
    "Expected: ", expected_ens

  )
}

# ---- County summaries (mean/sd across ensembles) ----
county_summaries <- ens_county_preds |>
  dplyr::group_by(scenario, model_output, pft, county) |>
  dplyr::summarize(
    n = max(n),
    mean_total_c_Tg = mean(total_c_Tg),
    sd_total_c_Tg = sd(total_c_Tg),
    mean_c_density_Mg_ha = mean(c_density_Mg_ha),
    sd_c_density_Mg_ha = sd(c_density_Mg_ha),
    mean_total_ha = mean(total_ha),
    sd_total_ha = sd(total_ha),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    dplyr::across(
      .cols = c(mean_total_c_Tg, sd_total_c_Tg, mean_c_density_Mg_ha, sd_c_density_Mg_ha),
      .fns = ~ signif(.x, 3)
    )
  )

write_output(
  county_summaries,
  file.path(model_outdir, "county_aggregated_preds"),
  "County aggregated predictions"
)

# ---- State-level summaries ----
state_summaries <- ens_county_preds |>
  dplyr::group_by(scenario, model_output, pft, ensemble) |>
  dplyr::summarize(
    n_counties = dplyr::n_distinct(county),
    n_fields = sum(n),
    total_c_Tg = sum(total_c_Tg),
    total_ha = sum(total_ha),
    .groups = "drop"
  ) |>
  dplyr::group_by(scenario, model_output, pft) |>
  dplyr::summarize(
    n_counties = max(n_counties),
    n_fields = max(n_fields),
    mean_total_c_Tg = mean(total_c_Tg),
    sd_total_c_Tg = sd(total_c_Tg),
    mean_total_ha = mean(total_ha),
    sd_total_ha = sd(total_ha),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    mean_c_density_Mg_ha = PEcAn.utils::ud_convert(mean_total_c_Tg, "Tg", "Mg") / mean_total_ha,
    dplyr::across(
      .cols = c(mean_total_c_Tg, sd_total_c_Tg),
      .fns = ~ signif(.x, 3)
    )
  )

write_output(
  state_summaries,
  file.path(model_outdir, "state_summaries"),
  "State-level summaries"
)

# ---- Process deltas ----
delta_csv <- file.path(model_outdir, "downscaled_deltas.csv")
if (file.exists(delta_csv)) {
  PEcAn.logger::logger.info("Processing delta predictions")
  
  deltas <- vroom::vroom(
    delta_csv,
    col_types = readr::cols(
      scenario = readr::col_character(),
      site_id = readr::col_character(),
      pft = readr::col_character(),
      ensemble = readr::col_double(),
      delta_c_density_Mg_ha = readr::col_double(),
      delta_total_c_Mg = readr::col_double(),
      area_ha = readr::col_double(),
      county = readr::col_character(),
      model_output = readr::col_character()
    )
  )
  
  # backward compatibility
  if (!"scenario" %in% names(deltas)) {
    deltas <- deltas |> dplyr::mutate(scenario = "baseline")
  }
  
  # County-level delta aggregation per ensemble
  ens_county_deltas <- deltas |>
    dplyr::group_by(scenario, model_output, pft, county, ensemble) |>
    dplyr::summarize(
      n = dplyr::n(),
      delta_total_Mg = sum(delta_total_c_Mg),
      total_ha = sum(area_ha),
      .groups = "drop"
    ) |>
    dplyr::filter(total_ha > 0) |>
    dplyr::mutate(
      delta_total_Tg = PEcAn.utils::ud_convert(delta_total_Mg, "Mg", "Tg"),
      delta_density_Mg_ha = delta_total_Mg / total_ha
    )
  
  # Summarize across ensembles
  county_deltas <- ens_county_deltas |>
    dplyr::group_by(scenario, model_output, pft, county) |>
    dplyr::summarize(
      # Number of fields in county should be same for each ensemble member
      n = max(n),
      mean_delta_total_Tg = mean(delta_total_Tg),
      sd_delta_total_Tg = sd(delta_total_Tg),
      mean_delta_density_Mg_ha = mean(delta_density_Mg_ha),
      sd_delta_density_Mg_ha = sd(delta_density_Mg_ha),
      mean_total_ha = mean(total_ha),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # Only save 3 significant digits
      dplyr::across(
        .cols = c(mean_delta_total_Tg, sd_delta_total_Tg, mean_delta_density_Mg_ha, sd_delta_density_Mg_ha),
        .fns = ~ signif(.x, 3)
      )
    )
  
  write_output(
    county_deltas,
    file.path(model_outdir, "county_aggregated_deltas"),
    "County aggregated deltas"
  )
  
} else {
  PEcAn.logger::logger.warn("downscaled_deltas.csv not found; skipping delta aggregation")
}

# ---- Write aggregation metadata ----
metadata <- list(
  title = "Aggregated County and State Summaries",
  description = "SIPNET downscaled outputs aggregated to county and state level",
  created = Sys.time(),
  scenarios = scenarios,
  pfts = unique(county_summaries$pft),
  outputs = unique(county_summaries$model_output),
  n_counties = dplyr::n_distinct(county_summaries$county),
  n_ensembles = length(ensemble_ids),
  files = list(
    county_preds = "county_aggregated_preds.csv",
    county_deltas = if (file.exists(delta_csv)) "county_aggregated_deltas.csv" else NULL,
    state_summaries = "state_summaries.csv"
  ),
  columns = list(
    scenario = "Management scenario identifier",
    model_output = "Carbon pool (AGB, TotSoilCarb)",
    pft = "Plant functional type",
    county = "California county name",
    n = "Number of fields",
    mean_total_c_Tg = "Mean total carbon stock (Tg)",
    sd_total_c_Tg = "SD of total carbon stock across ensembles",
    mean_c_density_Mg_ha = "Mean carbon density (Mg/ha)",
    sd_c_density_Mg_ha = "SD of carbon density across ensembles"
  )
)

jsonlite::write_json(
  metadata,
  file.path(model_outdir, "aggregation_metadata.json"),
  pretty = TRUE,
  auto_unbox = TRUE
)
PEcAn.logger::logger.info("Aggregation metadata written to ", file.path(model_outdir, "aggregation_metadata.json"))

PEcAn.logger::logger.info(
  paste0(
    "Aggregation complete!\n",
    "  - County summaries: ", nrow(county_summaries), " rows\n",
    "  - State summaries: ", nrow(state_summaries), " rows\n",
    "  - Scenarios: ", paste(scenarios, collapse = ", ")
  )
)