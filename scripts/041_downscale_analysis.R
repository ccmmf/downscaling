# Load required libraries and data
source("000-config.R")
checkpoint_file <- file.path(cache_dir, "downscaling_output.RData")
checkpoint_objects <- load(checkpoint_file)
PEcAn.logger::logger.info("Loaded checkpoint objects:", paste(checkpoint_objects, collapse = ","))
# Objects expected:
# - downscale_output_list (named like "woody::AGB", "annual::AGB", ...)
# - covariates, design_points, design_covariates, ensemble_ids

# Identify available PFT+pool specs from names
spec_table <- tibble::tibble(spec = names(downscale_output_list)) |>
  tidyr::separate(spec, into = c("pft_key", "model_output"), sep = "::", remove = FALSE) |>
  dplyr::mutate(
    pft = dplyr::case_when(
      pft_key == "woody"  ~ "woody perennial crop",
      pft_key == "annual" ~ "annual crop",
      TRUE ~ pft_key
    )
  )

##### Variable Importance Analysis (by PFT and pool) #####
importance_summary <- purrr::map_dfr(spec_table$spec, function(sp) {
  obj <- downscale_output_list[[sp]]
  importances <- purrr::map(ensemble_ids, function(i) {
    model <- obj[["model"]][[i]]
    vi <- randomForest::importance(model)
    # Prefer %IncMSE if present; otherwise use IncNodePurity
    if ("%IncMSE" %in% colnames(vi)) vi[, "%IncMSE"] else vi[, 1]
  })
  predictors <- rownames(randomForest::importance(obj[["model"]][[1]]))
  imp_df <- purrr::map_dfr(importances, ~ tibble::tibble(importance = .x), .id = "ensemble") |>
    dplyr::group_by(ensemble) |>
    dplyr::mutate(predictor = predictors) |>
    dplyr::ungroup()
  spec_row <- dplyr::filter(spec_table, spec == sp)
  imp_df |>
    dplyr::group_by(predictor) |>
    dplyr::summarize(
      median_importance = median(importance, na.rm = TRUE),
      lcl_importance = stats::quantile(importance, 0.25, na.rm = TRUE),
      ucl_importance = stats::quantile(importance, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      model_output = spec_row$model_output,
      pft_key = spec_row$pft_key,
      pft = spec_row$pft
    )
})

for (i in seq_len(nrow(spec_table))) {
  sp <- spec_table$spec[i]
  pft_label <- spec_table$pft[i]
  pool <- spec_table$model_output[i]
  obj <- downscale_output_list[[sp]]
  model <- obj[["model"]][[1]]

  # Top 2 predictors for this PFT+pool
  top_predictors <- importance_summary |>
    dplyr::filter(model_output == pool, pft == pft_label) |>
    dplyr::arrange(dplyr::desc(median_importance)) |>
    dplyr::slice_head(n = 2) |>
    dplyr::pull(predictor)

  if (length(top_predictors) < 2) {
    PEcAn.logger::logger.warn("Not enough predictors for partial plots:", sp)
    next
  }

  # Set up PNG for three panel plot
  PEcAn.logger::logger.info("Creating importance and partial plots for", sp)
  importance_partial_plot_fig <- here::here(
    "figures",
    paste0(gsub("::", "_", sp), "_importance_partial_plots.png")
  )

  png(
    filename = importance_partial_plot_fig,
    width = 14, height = 6, units = "in", res = 300, bg = "white"
  )
  par(mfrow = c(1, 3))

  # Panel 1: Variable importance plot
  output_importance <- importance_summary |>
    dplyr::filter(model_output == pool, pft == pft_label)
  par(mar = c(5, 10, 4, 2))
  with(
    output_importance,
    dotchart(median_importance,
      labels = reorder(predictor, median_importance),
      xlab = "Median Increase MSE (SD)",
      main = paste("Importance -", pool, "-", pft_label),
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
    ylab = paste("Predicted", pool, "-", pft_label),
    col = "steelblue",
    lwd = 2
  )

  # Panel 3: Partial plot for second predictor
  randomForest::partialPlot(model,
    pred.data = design_covariates,
    x.var = top_predictors[2],
    main = paste("Partial Dependence -", top_predictors[2]),
    xlab = top_predictors[2],
    ylab = paste("Predicted", pool, "-", pft_label),
    col = "steelblue",
    lwd = 2
  )
  dev.off()
  PEcAn.logger::logger.info(
    "Saved importance and partial plots for",
    sp, " to ", importance_partial_plot_fig
  )
}


## ALE Plots
# robust marginal effect estimates even with correlated predictors.
# library(iml)

PEcAn.logger::logger.info("***Starting ALE plots***")
for (i in seq_len(nrow(spec_table))) {
  sp <- spec_table$spec[i]
  pft_label <- spec_table$pft[i]
  pool <- spec_table$model_output[i]
  model <- downscale_output_list[[sp]][["model"]][[1]]

  top_predictors <- importance_summary |>
    dplyr::filter(model_output == pool, pft == pft_label) |>
    dplyr::arrange(dplyr::desc(median_importance)) |>
    dplyr::slice_head(n = 2) |>
    dplyr::pull(predictor)

  predictor_obj <- iml::Predictor$new(
    model = model,
    data = design_covariates,
    y = NULL,
    predict.function = function(m, newdata) stats::predict(m, newdata)
  )

  for (j in seq_along(top_predictors)) {
    pred_var_name <- top_predictors[j]
    PEcAn.logger::logger.info("Starting ALE calculation for", sp, "predictor:", pred_var_name)
    ale <- iml::FeatureEffect$new(predictor_obj, feature = pred_var_name, method = "ale")
    ggplot2::ggsave(
      filename = here::here("figures", paste0(gsub("::", "_", sp), "_ALE_predictor", j, ".png")),
      plot = plot(ale) + ggplot2::ggtitle(paste("ALE for", pred_var_name, "on", pool, "-", pft_label)),
      width = 6, height = 4, units = "in", dpi = 300
    )
  }
}

## ICE Plots
PEcAn.logger::logger.info("Creating ICE plots for top predictors")
for (i in seq_len(nrow(spec_table))) {
  sp <- spec_table$spec[i]
  pft_label <- spec_table$pft[i]
  pool <- spec_table$model_output[i]
  model <- downscale_output_list[[sp]][["model"]][[1]]

  top_predictors <- importance_summary |>
    dplyr::filter(model_output == pool, pft == pft_label) |>
    dplyr::arrange(dplyr::desc(median_importance)) |>
    dplyr::slice_head(n = 2) |>
    dplyr::pull(predictor)

  predictor_obj <- iml::Predictor$new(
    model = model,
    data = design_covariates,
    y = NULL, # y is not used by predict.randomForest
    predict.function = function(m, newdata) stats::predict(m, newdata)
  )

  for (j in seq_along(top_predictors)) {
    pred_var_name <- top_predictors[j]
    ice <- iml::FeatureEffect$new(predictor_obj, feature = pred_var_name, method = "ice")
    ggplot2::ggsave(
      filename = here::here("figures", paste0(gsub("::", "_", sp), "_ICE_predictor", j, ".png")),
      plot = plot(ice) + ggplot2::ggtitle(paste("ICE for", pred_var_name, "on", pool, "-", pft_label)),
      width = 6, height = 4, units = "in", dpi = 300
    )
  }
}
