# Load required libraries and data
source("000-config.R")
checkpoint_file <- file.path(cache_dir, "design_checkpoint.RData")
checkpoint_objects <- load(checkpoint_file)
PEcAn.logger::logger.info("Loaded checkpoint objects:", paste(checkpoint_objects, collapse = ","))

##### Variable Importance Analysis #####

### Much quicker than reading settings:
### but next time can get from design_checkpoint.RData
ensemble_ids <- readr::read_csv(
  file.path(model_outdir, "ensemble_output.csv"),
  col_types = readr::cols(ensemble = readr::col_double())
) |>
  dplyr::pull(ensemble) |>
  unique()

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
  summary_df
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
  png(
    filename = here::here("figures", paste0(output, "_importance_partial_plots.png")),
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
  PEcAn.logger::logger.info("Saved importance and partial plots for", output)
}


## ALE Plots
# robust marginal effect estimates even with correlated predictors.
# library(iml)

PEcAn.logger::logger.info("***Starting ALE plots***")

downscale_output_list <- readr::read_rds(file = file.path(cache_dir, "downscale_output_list.rds"))

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


##### Importance and Partial Plots #####

## Using pdp + ggplot2
# # Loop over carbon pools
# for (output in outputs_to_extract) {
#   # Top 2 predictors for this carbon pool
#   top_predictors <- importance_summary |>
#     dplyr::filter(model_output == output) |>
#     dplyr::arrange(dplyr::desc(median_importance)) |>
#     dplyr::slice_head(n = 2) |>
#     dplyr::pull(predictor)

#   # Retrieve model and covariate data
#   model <- downscale_output_list[[output]][["model"]][[1]]
#   design_covariates <- covariates_df # Already scaled

#   ## 1. Create Variable Importance Plot with ggplot2
#   output_importance <- importance_summary |>
#     dplyr::filter(model_output == output)

#   p_importance <- ggplot2::ggplot(output_importance, ggplot2::aes(x = median_importance, y = reorder(predictor, median_importance))) +
#     ggplot2::geom_point(color = "steelblue", size = 3) +
#     ggplot2::geom_errorbarh(ggplot2::aes(xmin = lcl_importance, xmax = ucl_importance),
#       height = 0.2,
#       color = "gray50"
#     ) +
#     ggplot2::labs(
#       title = paste("Importance -", output),
#       x = "Median Increase in MSE (SD)",
#       y = ""
#     ) +
#     ggplot2::theme_minimal()

#   ## 2. Create Partial Dependence Plot for the top predictor
#   pd_data1 <- pdp::partial(
#     object = model,
#     pred.var = top_predictors[1],
#     pred.data = design_covariates,
#     train = design_covariates,
#     plot = FALSE
#   )
#   ## Partial dependence for predictor 1
#   p_partial1 <- ggplot2::ggplot(pd_data1, ggplot2::aes_string(x = top_predictors[1], y = "yhat")) +
#     ggplot2::geom_line(color = "steelblue", size = 1.2) +
#     ggplot2::geom_rug(
#       data = design_covariates, ggplot2::aes_string(x = top_predictors[1]),
#       sides = "b", alpha = 0.5
#     ) +
#     ggplot2::labs(
#       title = paste("Partial Dependence -", top_predictors[1]),
#       x = top_predictors[1],
#       y = paste("Predicted", output)
#     ) +
#     ggplot2::theme_minimal()

#   ## Partial dependence for predictor 2
#   pd_data2 <- pdp::partial(
#     object = model,
#     pred.var = top_predictors[2],
#     pred.data = design_covariates,
#     plot = TRUE, # Note: pdp::partial with plot=TRUE returns a ggplot object directly
#     train = design_covariates,
#     parallel = TRUE 
#   )

#   p_partial2 <- ggplot2::ggplot(pd_data2, ggplot2::aes_string(x = top_predictors[2], y = "yhat")) + # This line might be redundant if pdp::partial returns a ggplot
#     ggplot2::geom_line(color = "steelblue", size = 1.2) +
#     ggplot2::geom_rug(
#       data = design_covariates, ggplot2::aes_string(x = top_predictors[2]),
#       sides = "b", alpha = 0.5
#     ) +
#     ggplot2::labs(
#       title = paste("Partial Dependence -", top_predictors[2]),
#       x = top_predictors[2],
#       y = paste("Predicted", output)
#     ) +
#     ggplot2::theme_minimal()


#   combined_plot <- p_importance + p_partial1 + p_partial2 + plot_layout(ncol = 3)

#   output_file <- here::here("downscale/figures", paste0(output, "_importance_partial_plots.png"))
#   ggplot2::ggsave(
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
#     dplyr::filter(model_output == output) |>
#     dplyr::arrange(dplyr::desc(median_importance)) |>
#     dplyr::slice_head(n = 2) |>
#     dplyr::pull(predictor)

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
