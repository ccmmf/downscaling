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

PEcAn.logger::logger.info("Starting ALE plots")
for (output in outputs_to_extract) {
  model <- downscale_output_list[[output]][["model"]][[1]]
  cov_df <- covariates_df

  top_predictors <- importance_summary |>
    filter(carbon_pool == output) |>
    arrange(desc(median_importance)) |>
    slice_head(n = 2) |>
    pull(predictor)

  predictor <- iml::Predictor$new(
    model = model,
    data = cov_df,
    y = NULL,
    predict.function = function(m, newdata) predict(m, newdata)
  )

  for (pred in top_predictors) {
    PEcAn.logger::logger.info("Starting ALE calculation for predictor:", pred)
    ale <- FeatureEffect$new(predictor, feature = pred, method = "ale")

    PEcAn.logger::logger.info("Saving ALE plot for predictor:", pred)
    ggsave(
      filename = here::here("downscale/figures", paste0(cp, "_ALE_", pred, ".png")),
      plot = plot(ale) + ggtitle(paste("ALE for", pred, "on", cp)),
      width = 6, height = 4, units = "in", dpi = 300
    )
  }
}

## ICE Plots
PEcAn.logger::logger.info("Creating ICE plots for top predictors")

for (cp in cpools) {
  model <- downscale_output_list[[cp]][["model"]][[1]]
  cov_df <- covariates_df

  top_predictors <- importance_summary |>
    filter(carbon_pool == cp) |>
    arrange(desc(median_importance)) |>
    slice_head(n = 2) |>
    pull(predictor)

  predictor <- Predictor$new(
    model = model,
    data = cov_df,
    y = NULL,
    predict.function = function(m, newdata) predict(m, newdata)
  )

  for (pred in top_predictors) {
    ice <- FeatureEffect$new(predictor, feature = pred, method = "ice")

    # Save plot
    ggsave(
      filename = here::here("downscale/figures", paste0(cp, "_ICE_", pred, ".png")),
      plot = plot(ice) + ggtitle(paste("ICE for", pred, "on", cp)),
      width = 6, height = 4, units = "in", dpi = 300
    )
  }
}
