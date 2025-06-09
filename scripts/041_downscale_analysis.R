# Load required libraries and data
source("000-config.R")
checkpoint_file <- file.path(cache_dir, "downscaling_output.RData")
checkpoint_objects <- load(checkpoint_file)
PEcAn.logger::logger.info("Loaded checkpoint objects:", paste(checkpoint_objects, collapse = ","))
# downscale_output_list
# covariates
# design_points
# design_covariates
# ensemble_ids 

##### Variable Importance Analysis #####
importance_summary <- purrr::map_dfr(outputs_to_extract, ~ {
  # Extract the importance for each ensemble model in the carbon pool
  importances <- purrr::map(ensemble_ids, function(i) {
    model <- downscale_output_list[[.x]][["model"]][[i]]
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
    dplyr::mutate(model_output = .x)
  return(summary_df)
})

for (output in outputs_to_extract) {
  # Top 2 predictors for this carbon pool
  top_predictors <- importance_summary |>
    dplyr::filter(model_output == output) |>
    dplyr::arrange(dplyr::desc(median_importance)) |>
    dplyr::slice_head(n = 2) |>
    dplyr::pull(predictor)

  # Prepare model and subset of covariates for plotting
  model <- downscale_output_list[[output]][["model"]][[1]]

  # Set up PNG for three panel plot
  PEcAn.logger::logger.info("Creating importance and partial plots for", output)
  importance_partial_plot_fig <- here::here(
    "figures",
    paste0(output, "_importance_partial_plots.png")
  ) # Ensure the directory exists

  png(
    filename = importance_partial_plot_fig,
    width = 14, height = 6, units = "in", res = 300, bg = "white"
  )
  par(mfrow = c(1, 3))

  # Panel 1: Variable importance plot
  output_importance <- importance_summary |>
    dplyr::filter(model_output == output)
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
  PEcAn.logger::logger.info(
    "Saved importance and partial plots for",
    output, " to ",
    importance_partial_plot_fig
  )
}


## ALE Plots
# robust marginal effect estimates even with correlated predictors.
# library(iml)

PEcAn.logger::logger.info("***Starting ALE plots***")

for (output in outputs_to_extract) {
  model <- downscale_output_list[[output]][["model"]][[1]]
  # Use design points covariates instead of all covariates

  top_predictors <- importance_summary |>
    dplyr::filter(model_output == output) |>
    dplyr::arrange(dplyr::desc(median_importance)) |>
    dplyr::slice_head(n = 2) |>
    dplyr::pull(predictor)

  predictor_obj <- iml::Predictor$new(
    model = model,
    data = design_covariates,
    y = NULL,
    predict.function = function(m, newdata) predict(m, newdata)
  )

  for (i in seq_along(top_predictors)) {
    pred_var_name <- top_predictors[i]

    PEcAn.logger::logger.info("Starting ALE calculation for predictor:", pred_var_name)
    ale <- iml::FeatureEffect$new(predictor_obj, feature = pred_var_name, method = "ale")

    PEcAn.logger::logger.info("Saving ALE plot for predictor:", pred_var_name)
    ggplot2::ggsave(
      filename = here::here("figures", paste0(output, "_ALE_predictor", i, ".png")),
      plot = plot(ale) + ggplot2::ggtitle(paste("ALE for", pred_var_name, "on", output)),
      width = 6, height = 4, units = "in", dpi = 300
    )
  }
}

## ICE Plots
PEcAn.logger::logger.info("Creating ICE plots for top predictors")

for (output in outputs_to_extract) {
  model <- downscale_output_list[[output]][["model"]][[1]]
  # Use design points covariates instead of all covariates
  

  top_predictors <- importance_summary |>
    dplyr::filter(model_output == output) |>
    dplyr::arrange(dplyr::desc(median_importance)) |>
    dplyr::slice_head(n = 2) |>
    dplyr::pull(predictor)

  predictor_obj <- iml::Predictor$new(
    model = model,
    data = design_covariates,
    y = NULL, # y is not used by predict.randomForest
    predict.function = function(m, newdata) stats::predict(m, newdata)
  )

  for (i in seq_along(top_predictors)) {
    pred_var_name <- top_predictors[i]
    ice <- iml::FeatureEffect$new(predictor_obj, feature = pred_var_name, method = "ice")

    # Save plot
    ggplot2::ggsave(
      filename = here::here("figures", paste0(output, "_ICE_predictor", i, ".png")),
      plot = plot(ice) + ggplot2::ggtitle(paste("ICE for", pred_var_name, "on", output)),
      width = 6, height = 4, units = "in", dpi = 300
    )
  }
}
