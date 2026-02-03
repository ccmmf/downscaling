## Downscale analysis using predictions and covariates
source("000-config.R")

# Variable Importance (VI)

preds_csv <- file.path(model_outdir, "downscaled_preds.csv")
meta_json <- file.path(model_outdir, "downscaled_preds_metadata.json")

downscale_preds <- vroom::vroom(
  preds_csv,
  col_types = readr::cols(
    site_id = readr::col_character(),
    pft = readr::col_character(),
    ensemble = readr::col_double(),
    c_density_Mg_ha = readr::col_double(),
    total_c_Mg = readr::col_double(),
    area_ha = readr::col_double(),
    county = readr::col_character(),
    model_output = readr::col_character()
  )
)

meta <- jsonlite::read_json(meta_json, simplifyVector = TRUE)
ensemble_ids <- if (!is.null(meta$ensembles)) meta$ensembles else sort(unique(downscale_preds$ensemble))

covariates_csv <- file.path(data_dir, "site_covariates.csv")

covariates <- readr::read_csv(covariates_csv) |>
  dplyr::select(site_id, where(is.numeric), -climregion_id)
covariate_names <- names(dplyr::select(covariates, where(is.numeric)))
PEcAn.logger::logger.info("Loaded predictions, metadata, and covariates")
PEcAn.logger::logger.info("Rows in predictions:", nrow(downscale_preds), "; unique sites:", dplyr::n_distinct(downscale_preds$site_id))
PEcAn.logger::logger.info("Ensembles detected:", paste(head(ensemble_ids, 10), collapse = ", "), if (length(ensemble_ids) > 10) "..." else "")
PEcAn.logger::logger.info("Number of numeric predictors:", length(covariate_names), "; sample:", paste(utils::head(covariate_names, 6), collapse = ", "))

preds_join <- downscale_preds |>
  dplyr::left_join(covariates, by = "site_id") |>
  tidyr::drop_na(c_density_Mg_ha)

# Optional: load training site metadata
train_sites_csv <- file.path(model_outdir, "training_sites.csv")
train_sites <- NULL
train_sites <- readr::read_csv(train_sites_csv, show_col_types = FALSE)
PEcAn.logger::logger.info("Training site metadata found:", train_sites_csv, "; rows:", nrow(train_sites))
PEcAn.logger::logger.info("Training site columns:", paste(colnames(train_sites), collapse = ", "))

# Build spec table from predictions (includes mixed pft)
spec_table <- preds_join |>
  dplyr::distinct(pft, model_output) |>
  dplyr::arrange(pft, model_output)
PEcAn.logger::logger.info("Data prep complete")

## No surrogate VI: read per-ensemble VI saved by 040 and summarize here
vi_path_for_spec <- function(pft_i, pool) {
  spec_key <- paste0(janitor::make_clean_names(pft_i), "_", janitor::make_clean_names(pool))
  file.path(model_outdir, paste0("vi_", spec_key, "_by_ensemble.csv"))
}

importance_summary <- purrr::pmap_dfr(
  list(spec_table$pft, spec_table$model_output),
  function(pft_i, pool) {
    vi_file <- vi_path_for_spec(pft_i, pool)
    if (!file.exists(vi_file)) {
      PEcAn.logger::logger.warn("VI per-ensemble CSV not found for ", pft_i, "::", pool)
      return(NULL)
    }
    vi_tbl <- readr::read_csv(vi_file, show_col_types = FALSE)
    vi_tbl |>
      dplyr::group_by(pft, model_output, predictor) |>
      dplyr::summarize(
        median_importance = stats::median(importance, na.rm = TRUE),
        lcl_importance = stats::quantile(importance, 0.25, na.rm = TRUE),
        ucl_importance = stats::quantile(importance, 0.75, na.rm = TRUE),
        n_ensembles = dplyr::n(),
        .groups = "drop"
      )
  }
)

## ALE Plots
# Robust marginal effect estimates even with correlated predictors.
# Run ALE/ICE first to prioritize unbiased effects under correlation.
PEcAn.logger::logger.info("Starting ALE and ICE plots for all specs (priority)")
for (row in seq_len(nrow(spec_table))) {
  pft_i <- spec_table$pft[row]
  pool <- spec_table$model_output[row]
  PEcAn.logger::logger.info("Starting ALE/ICE plots for spec:", paste(pft_i, pool, sep = "::"))
  spec_key <- paste0(janitor::make_clean_names(pft_i), "_", janitor::make_clean_names(pool))
  mdl_path <- file.path(cache_dir, "models", paste0(spec_key, "_models.rds"))
  trn_path <- file.path(cache_dir, "training_data", paste0(spec_key, "_training.csv"))
  if (!file.exists(mdl_path) || !file.exists(trn_path)) {
    PEcAn.logger::logger.warn("Saved model/training data missing for ", pft_i, "::", pool, "; skipping ALE/ICE")
    next
  }
  models <- readRDS(mdl_path)
  df_spec <- readr::read_csv(trn_path, show_col_types = FALSE)
  rf <- models[[1]]
  # Use design-point covariates for ALE/ICE as in the original script
  design_covariates <- dplyr::select(df_spec, dplyr::all_of(covariate_names)) |> as.data.frame()
  requireNamespace("randomForest", quietly = TRUE)
  predictor_obj <- iml::Predictor$new(
    model = rf, data = design_covariates, y = NULL,
    predict.function = function(m, newdata) stats::predict(m, newdata)
  )
  top_predictors <- importance_summary |>
    dplyr::filter(model_output == pool, pft == pft_i) |>
    dplyr::arrange(dplyr::desc(median_importance)) |>
    dplyr::slice_head(n = 2) |>
    dplyr::pull(predictor)
  for (j in seq_along(top_predictors)) {
    pred_var_name <- top_predictors[j]
    ale <- iml::FeatureEffect$new(predictor_obj, feature = pred_var_name, method = "ale")
    ggsave_optimized(
      filename = here::here("figures", paste0(janitor::make_clean_names(pft_i), "_", janitor::make_clean_names(pool), "_ALE_predictor", j, ".svg")),
      plot = plot(ale) + ggplot2::ggtitle(paste("ALE for", pred_var_name, "on", pool, "-", pft_i)),
      width = 6, height = 4, units = "in"
    )
    ice <- iml::FeatureEffect$new(predictor_obj, feature = pred_var_name, method = "ice")
    ggsave_optimized(
      filename = here::here("figures", paste0(janitor::make_clean_names(pft_i), "_", janitor::make_clean_names(pool), "_ICE_predictor", j, ".svg")),
      plot = plot(ice) + ggplot2::ggtitle(paste("ICE for", pred_var_name, "on", pool, "-", pft_i)),
      width = 6, height = 4, units = "in"
    )
  }
}

for (row in seq_len(nrow(spec_table))) {
  pft_i <- spec_table$pft[row]
  pool <- spec_table$model_output[row]
  PEcAn.logger::logger.info("Processing plots for spec:", paste(pft_i, pool, sep = "::"))

  spec_key <- paste0(janitor::make_clean_names(pft_i), "_", janitor::make_clean_names(pool))
  mdl_path <- file.path(cache_dir, "models", paste0(spec_key, "_models.rds"))
  trn_path <- file.path(cache_dir, "training_data", paste0(spec_key, "_training.csv"))
  rf <- NULL

  # Here, the cached training CSV corresponds to the design-point covariates used to train the RF.
  design_covariates <- NULL
  if (file.exists(mdl_path) && file.exists(trn_path)) {
    models <- readRDS(mdl_path)
    df_spec <- readr::read_csv(trn_path, show_col_types = FALSE)
    rf <- models[[1]]
    if (inherits(rf, "randomForest")) {
      design_covariates <- dplyr::select(df_spec, dplyr::all_of(covariate_names)) |>
        as.data.frame()
    }
  }
  if (is.null(rf)) {
    PEcAn.logger::logger.warn("Saved model not found for ", pft_i, "::", pool, "; skipping PDP")
    next
  }

  top_predictors <- importance_summary |>
    dplyr::filter(model_output == pool, pft == pft_i) |>
    dplyr::arrange(dplyr::desc(median_importance)) |>
    dplyr::slice_head(n = 2) |>
    dplyr::pull(predictor)

  if (length(top_predictors) < 2) {
    PEcAn.logger::logger.warn("Not enough predictors for partial plots:", paste(pft_i, pool, sep = "::"))
    next
  }

  # Only produce PDPs if features are sufficiently uncorrelated
  corr_threshold <- suppressWarnings(as.numeric(Sys.getenv("PDP_CORR_THRESHOLD", unset = "0.3")))
  if (is.na(corr_threshold)) corr_threshold <- 0.3
  allow_pdp <- FALSE
  if (!is.null(design_covariates) && ncol(design_covariates) > 1) {
    cm <- try(stats::cor(design_covariates, use = "pairwise.complete.obs"), silent = TRUE)
    if (!inherits(cm, "try-error")) {
      # For each top predictor, require low correlation with all other predictors
      max_corr <- vapply(top_predictors, function(v) {
        others <- setdiff(colnames(cm), v)
        if (!v %in% colnames(cm) || length(others) == 0) return(1)
        max(abs(cm[v, others]), na.rm = TRUE)
      }, numeric(1))
      allow_pdp <- all(max_corr <= corr_threshold)
      PEcAn.logger::logger.info(
        sprintf("PDP correlation check (max abs corr per var): %s; threshold=%.2f",
                paste(sprintf("%s=%.2f", top_predictors, max_corr), collapse = ", "), corr_threshold)
      )
    }
  }
  if (!allow_pdp) {
    PEcAn.logger::logger.info("Skipping PDPs due to correlated predictors; rely on ALE/ICE.")
    next
  }

  PEcAn.logger::logger.info("Creating importance and partial plots (PDP) for", paste(pft_i, pool, sep = "::"))
  clean_pft <- janitor::make_clean_names(pft_i)
  clean_pool <- janitor::make_clean_names(pool)
  importance_partial_plot_fig <- here::here(
    "figures",
    paste0(clean_pft, "_", clean_pool, "_importance_partial_plots.png")
  )

  png(filename = importance_partial_plot_fig, width = 14, height = 6, units = "in", res = 300, bg = "white")
  par(mfrow = c(1, 3))

  # Panel 1: Variable importance plot
  output_importance <- importance_summary |>
    dplyr::filter(model_output == pool, pft == pft_i)
  par(mar = c(5, 10, 4, 2))
  with(
    output_importance,
    dotchart(median_importance,
      labels = reorder(predictor, median_importance),
      xlab = "Median Increase MSE (SD)",
      main = paste("Importance -", pool, "-", pft_i),
      pch = 19, col = "steelblue", cex = 1.2
    )
  )
  with(
    output_importance,
    segments(lcl_importance, seq_along(predictor), ucl_importance, seq_along(predictor), col = "gray50")
  )

  # Panel 2 and 3: Partial plots for top predictors
  par(mar = c(5, 5, 4, 2))
  # Set common y-limits from training response
  y_range <- range(rf$y, na.rm = TRUE)
  yl <- c(floor(min(y_range)), ceiling(max(y_range)))

  randomForest::partialPlot(rf,
    pred.data = design_covariates, x.var = top_predictors[1], ylim = yl,
    main = paste("Partial Dependence -", top_predictors[1]),
    xlab = top_predictors[1], ylab = paste("Predicted", pool, "-", pft_i), col = "steelblue", lwd = 2
  )
  randomForest::partialPlot(rf,
    pred.data = design_covariates, x.var = top_predictors[2], ylim = yl,
    main = paste("Partial Dependence -", top_predictors[2]),
    xlab = top_predictors[2], ylab = paste("Predicted", pool, "-", pft_i), col = "steelblue", lwd = 2
  )
  dev.off()
  PEcAn.logger::logger.info("Saved importance/PDP figure:", importance_partial_plot_fig)
}
