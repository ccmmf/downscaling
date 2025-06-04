# Load required libraries
source("000-config.R")
# library(ggplot2)
# library(dplyr)
# library(randomForest)
# library(pdp)
# library(here)

## ALE Plots
# robust marginal effect estimates even with correlated predictors.
# library(iml)

PEcAn.logger::logger.info("***Starting ALE plots***")

for (output in outputs_to_extract) {
  model <- downscale_output_list[[output]][["model"]][[1]]
  # Use design points covariates instead of all covariates

  top_predictors <- importance_summary |>
    dplyr::filter(model_output == output) |>
    dplyr::arrange(desc(median_importance)) |>
    dplyr::slice_head(n = 2) |>
    dplyr::pull(predictor)

  predictor <- iml::Predictor$new(
    model = model,
    data = design_covariates,
    y = NULL,
    predict.function = function(m, newdata) predict(m, newdata)
  )

  for (pred in top_predictors) {
    PEcAn.logger::logger.info("Starting ALE calculation for predictor:", pred)
    ale <- iml::FeatureEffect$new(predictor, feature = pred, method = "ale")

    PEcAn.logger::logger.info("Saving ALE plot for predictor:", pred)
    ggplot2::ggsave(
      filename = here::here("figures", paste0(output, "_ALE_", pred, ".png")),
      plot = plot(ale) + ggplot2::ggtitle(paste("ALE for", pred, "on", output)),
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
    dplyr::arrange(desc(median_importance)) |>
    dplyr::slice_head(n = 2) |>
    dplyr::pull(predictor)

  predictor <- iml::Predictor$new(
    model = model,
    data = design_covariates,
    y = NULL,
    predict.function = function(m, newdata) predict(m, newdata)
  )

  for (pred in top_predictors) {
    ice <- iml::FeatureEffect$new(predictor, feature = pred, method = "ice")

    # Save plot
    ggplot2::ggsave(
      filename = here::here("figures", paste0(output, "_ICE_", pred, ".png")),
      plot = plot(ice) + ggplot2::ggtitle(paste("ICE for", pred, "on", output)),
      width = 6, height = 4, units = "in", dpi = 300
    )
  }
}
