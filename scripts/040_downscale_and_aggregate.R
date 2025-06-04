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

settings <- PEcAn.settings::read.settings(file.path(pecan_outdir, "pecan.CONFIGS.xml"))

ensemble_file <- file.path(model_outdir, "ensemble_output.csv")
ensemble_data <- readr::read_csv(ensemble_file) 

### Random Forest using PEcAn downscale workflow
## -----------------------------------------------------------------------------
design_pt_csv <- "data/design_points.csv"
design_points <- readr::read_csv(design_pt_csv)  |> 
  dplyr::filter(pft == "annual crop")

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

##### Subset data for plotting (speed + visible rug plots) #######
# Subset data for testing / speed purposes
if (!PRODUCTION) {
  outputs_to_extract <- c("AGB")
  if(!exists(".Random.seed")) set.seed(123)
  covariate_subset <- covariates |>
    dplyr::anti_join(design_points, by = "site_id") |>
    dplyr::slice_sample(n = 10000) # limit to 10k sites with reproducible sampling
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
    result <- downscale_model_output(date = "2018-12-31", model_output = .x)
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
  # could be even faster if we did this in sf::read_sf(..., sql = "SELECT * FROM ca_fields WHERE site_id IN (...)")
  ca_fields <- ca_fields |>
    dplyr::left_join(covariates, by = "site_id") 
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
      n = max(n), # Number of fields in county should be same for each ensemble member
      mean_total_c_Tg = mean(total_c_Tg),
      sd_total_c_Tg = sd(total_c_Tg),
      mean_c_density_Mg_ha = mean(mean_c_density_Mg_ha),
      sd_c_density_Mg_ha = sd(mean_c_density_Mg_ha),
      .groups = "drop"
    )
    
readr::write_csv(
  county_summaries,
  file.path(model_outdir, "county_summaries.csv")
)
PEcAn.logger::logger.info("County summaries written to", file.path(model_outdir, "county_summaries.csv"))

county_boundaries <- sf::st_read(file.path(data_dir, "ca_counties.gpkg"))

co_preds_to_plot <- county_summaries |>
  dplyr::right_join(county_boundaries, by = "county") |>
  dplyr::arrange(county, model_output) |>
  tidyr::pivot_longer(
    cols = c(mean_total_c_Tg, sd_total_c_Tg, mean_c_density_Mg_ha, sd_c_density_Mg_ha),
    names_to = "stat",
    values_to = "value"
  ) |>
  dplyr::mutate(units = dplyr::case_when(
    stringr::str_detect(stat, "total_c") ~ "Carbon Stock (Tg)",
    stringr::str_detect(stat, "c_density") ~ "Carbon Density (Mg/ha)"
  ), stat = dplyr::case_when(
    stringr::str_detect(stat, "mean") ~ "Mean",
    stringr::str_detect(stat, "sd") ~ "SD"
  ))

units <- rep(unique(co_preds_to_plot$units), each = length(outputs_to_extract))
pool_x_units <- co_preds_to_plot |>
  dplyr::select(model_output, units) |>
  dplyr::distinct() |>
  # remove na
  dplyr::filter(!is.na(model_output)) |> # why is one field in SF county NA?
  dplyr::arrange(model_output, units) |> 
  dplyr::filter(!is.na(model_output))

p <- purrr::map2(pool_x_units$model_output, pool_x_units$units, function(pool, unit) {
  .p <- ggplot2::ggplot(
    dplyr::filter(co_preds_to_plot, model_output == pool & units == unit),
    ggplot2::aes(geometry = geom, fill = value)
  ) +
    ggplot2::geom_sf(data = county_boundaries, fill = "lightgrey", color = "black") +
    ggplot2::geom_sf() +
    ggplot2::scale_fill_viridis_c(option = "plasma") +
    ggplot2::theme_minimal() +
    ggplot2::facet_grid(model_output ~ stat) +
    ggplot2::labs(
      title = paste("County-Level Predictions for", pool, unit),
      fill = "Value"
    )

  unit <- ifelse(unit == "Carbon Stock (Tg)", "stock",
    ifelse(unit == "Carbon Density (Mg/ha)", "density", NA)
  )

  plotfile <- here::here("figures", paste0("county_", pool, "_carbon_", unit, ".png"))
  PEcAn.logger::logger.info("Creating county-level plot for", pool, unit)
  ggplot2::ggsave(
    plot = .p,
    filename = plotfile,
    width = 10, height = 5,
    bg = "white"
  )
  return(.p)
})

# Variable Importance and Partial Dependence Plots
importance_summary <- purrr::map_dfr(outputs_to_extract, function(output) {
  # Extract the importance for each ensemble model in the carbon pool
  importances <- purrr::map(1:20, function(i) {
    model <- downscale_output_list[[output]][["model"]][[i]]
    randomForest::importance(model)[, "%IncMSE"]
  })

  # Turn the list of importance vectors into a data frame
  importance_df <- purrr::map_dfr(importances, ~ tibble::tibble(importance = .x), .id = "ensemble") |>
    dplyr::group_by(ensemble) |>
    dplyr::mutate(predictor = names(importances[[1]])) |>
    dplyr::ungroup()

  # Now summarize median and IQR for each predictor across ensembles
  summary_df <- importance_df |>
    dplyr::group_by(predictor) |>
    dplyr::summarize(
      median_importance = median(importance, na.rm = TRUE),
      lcl_importance = quantile(importance, 0.25, na.rm = TRUE),
      ucl_importance = quantile(importance, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(model_output = output)

  summary_df
})


# TODO consider separating out plotting
#### ---Create checkpoint---####
# system.time(save(downscale_output_list, importance_summary, covariates, outputs_to_extract# these are ~500MB
#   file = file.path(cache_dir, "checkpoint.RData"),
#   compress = FALSE
# ))


##### Importance Plots #####

for (output in outputs_to_extract) {
  # Top 2 predictors for this carbon pool
  top_predictors <- importance_summary |>
    filter(model_output == output) |>
    arrange(desc(median_importance)) |>
    slice_head(n = 2) |>
    pull(predictor)

  # Prepare model and subset of covariates for plotting
  model <- downscale_output_list[[output]][["model"]][[1]]

  # Set up PNG for three panel plot
  png(
    filename = here::here("figures", paste0(output, "_importance_partial_plots.png")),
    width = 14, height = 6, units = "in", res = 300, bg = "white"
  )
  par(mfrow = c(1, 3))

  # Panel 1: Variable importance plot
  output_importance <- importance_summary |> filter(model_output == output)
  par(mar = c(5, 10, 4, 2))
  with(
    output_importance,
    dotchart(median_importance,
      labels = reorder(predictor, median_importance),
      xlab = "Median Increase MSE (SD)",
      main = paste("Importance -", output),
      pch = 19, col = "steelblue", cex = 1.2
    )
  )
  with(
    output_importance,
    segments(lcl_importance,
      seq_along(predictor),
      ucl_importance,
      seq_along(predictor),
      col = "gray50"
    )
  )

  # Panel 2: Partial plot for top predictor
  par(mar = c(5, 5, 4, 2))
  randomForest::partialPlot(model,
    pred.data = design_covariates,
    x.var = top_predictors[1],
    main = paste("Partial Dependence -", top_predictors[1]),
    xlab = top_predictors[1],
    ylab = paste("Predicted", output),
    col = "steelblue",
    lwd = 2
  )

  # Panel 3: Partial plot for second predictor
  randomForest::partialPlot(model,
    pred.data = design_covariates,
    x.var = top_predictors[2],
    main = paste("Partial Dependence -", top_predictors[2]),
    xlab = top_predictors[2],
    ylab = paste("Predicted", output),
    col = "steelblue",
    lwd = 2
  )
  dev.off() # Save combined figure
  PEcAn.logger::logger.info("Saved importance and partial plots for", output)
}

PEcAn.logger::logger.info("***Finished downscaling and aggregation***")


##### Importance and Partial Plots #####

## Using pdp + ggplot2
# # Loop over carbon pools
# for (output in outputs_to_extract) {
#   # Top 2 predictors for this carbon pool
#   top_predictors <- importance_summary |>
#     filter(model_output == output) |>
#     arrange(desc(median_importance)) |>
#     slice_head(n = 2) |>
#     pull(predictor)

#   # Retrieve model and covariate data
#   model <- downscale_output_list[[output]][["model"]][[1]]
#   design_covariates <- covariates_df # Already scaled

#   ## 1. Create Variable Importance Plot with ggplot2
#   output_importance <- importance_summary |>
#     filter(model_output == output)

#   p_importance <- ggplot(output_importance, aes(x = median_importance, y = reorder(predictor, median_importance))) +
#     geom_point(color = "steelblue", size = 3) +
#     geom_errorbarh(aes(xmin = lcl_importance, xmax = ucl_importance),
#       height = 0.2,
#       color = "gray50"
#     ) +
#     labs(
#       title = paste("Importance -", output),
#       x = "Median Increase in MSE (SD)",
#       y = ""
#     ) +
#     theme_minimal()

#   ## 2. Create Partial Dependence Plot for the top predictor
#   pd_data1 <- pdp::partial(
#     object = model,
#     pred.var = top_predictors[1],
#     pred.data = design_covariates,
#     train = design_covariates,
#     plot = FALSE
#   )
#   ## Partial dependence for predictor 1
#   p_partial1 <- ggplot(pd_data1, aes_string(x = top_predictors[1], y = "yhat")) +
#     geom_line(color = "steelblue", size = 1.2) +
#     geom_rug(
#       data = design_covariates, aes_string(x = top_predictors[1]),
#       sides = "b", alpha = 0.5
#     ) +
#     labs(
#       title = paste("Partial Dependence -", top_predictors[1]),
#       x = top_predictors[1],
#       y = paste("Predicted", output)
#     ) +
#     theme_minimal()

#   ## Partial dependence for predictor 2
#   pd_data2 <- pdp::partial(
#     object = model,
#     pred.var = top_predictors[2],
#     pred.data = design_covariates,
#     plot = TRUE,
#     train = design_covariates,
#     parallel = TRUE
#   )

#   p_partial2 <- ggplot(pd_data2, aes_string(x = top_predictors[2], y = "yhat")) +
#     geom_line(color = "steelblue", size = 1.2) +
#     geom_rug(
#       data = design_covariates, aes_string(x = top_predictors[2]),
#       sides = "b", alpha = 0.5
#     ) +
#     labs(
#       title = paste("Partial Dependence -", top_predictors[2]),
#       x = top_predictors[2],
#       y = paste("Predicted", output)
#     ) +
#     theme_minimal()

#   combined_plot <- p_importance + p_partial1 + p_partial2 + plot_layout(ncol = 3)

#   output_file <- here("downscale/figures", paste0(output, "_importance_partial_plots.png"))
#   ggsave(
#     filename = output_file,
#     plot = combined_plot,
#     width = 14, height = 6, dpi = 300, bg = "white"
#   )

#   # also save pdp-generated plot 
#   pdp_plots <- p_data1 + p_data2
#   ggsave(pdp_plots,
#     filename = here::here("downscale/figures", paste0(output, "_PDP_", 
#       top_predictors[1], "_", top_predictors[2], ".png")),
#     width = 6, height = 4, dpi = 300, bg = "white"
#   )
# }

## Using randomForest::partialPlot()
# Combined importance + partial plots for each carbon pool


# for (output in outputs_to_extract) {
#   # Top 2 predictors for this carbon pool
#   top_predictors <- importance_summary |>
#     filter(model_output == output) |>
#     arrange(desc(median_importance)) |>
#     slice_head(n = 2) |>
#     pull(predictor)

#   # Prepare model and subset of covariates for plotting
#   model <- downscale_output_list[[output]][["model"]][[1]]

#   # Set up PNG for three panel plot
#   png(
#     filename = here::here("downscale/figures", paste0(output, "_importance_partial_plots.png")),
#     width = 14, height = 6, units = "in", res = 300, bg = "white"
#   )
#   par(mfrow = c(1, 3))

#   # Panel 1: Variable importance plot
#   output_importance <- importance_summary |> filter(model_output == output)
#   par(mar = c(5, 10, 4, 2))
#   with(
#     output_importance,
#     dotchart(median_importance,
#       labels = reorder(predictor, median_importance),
#       xlab = "Median Increase MSE (SD)",
#       main = paste("Importance -", output),
#       pch = 19, col = "steelblue", cex = 1.2
#     )
#   )
#   with(
#     output_importance,
#     segments(lcl_importance,
#       seq_along(predictor),
#       ucl_importance,
#       seq_along(predictor),
#       col = "gray50"
#     )
#   )

#   # Panel 2: Partial plot for top predictor
#   par(mar = c(5, 5, 4, 2))
#   randomForest::partialPlot(model,
#     pred.data = design_covariates,
#     x.var = top_predictors[1],
#     main = paste("Partial Dependence -", top_predictors[1]),
#     xlab = top_predictors[1],
#     ylab = paste("Predicted", output),
#     col = "steelblue",
#     lwd = 2
#   )

#   # Panel 3: Partial plot for second predictor
#   randomForest::partialPlot(model,
#     pred.data = design_covariates,
#     x.var = top_predictors[2],
#     main = paste("Partial Dependence -", top_predictors[2]),
#     xlab = top_predictors[2],
#     ylab = paste("Predicted", output),
#     col = "steelblue",
#     lwd = 2
#   )
#   dev.off() # Save combined figure
# }
