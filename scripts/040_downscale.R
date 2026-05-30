# Predict SIPNET ensemble output at every LandIQ field, by scenario,
# PFT, and carbon pool. We fit one Random Forest per ensemble member so
# the ensemble spread carries through to the field-level predictions.
# Writes downscaled_preds.csv (long format: one row per field x
# ensemble x pool) for 041 to roll up to county.
# RF may move to CNN later.
#
# Quick debug knobs:
#   debugonce(PEcAnAssimSequential::ensemble_downscale)
#   PEcAn.logger::logger.setQuitOnSevere(TRUE)


source("000-config.R")
PEcAn.logger::logger.info("***Starting Downscaling and Aggregation***")

# Load ensemble output
ensemble_csv <- file.path(model_outdir, "ensemble_output.csv")
timer_read_ensemble <- step_timer()
ensemble_data <- readr::read_csv(ensemble_csv) |>
  dplyr::rename(
    ensemble = parameter # parameter is EFI std name for ensemble
  )

# detect scenario mode (Phase 3 has scenario column, Phase 2 does not)
has_scenarios <- "scenario" %in% names(ensemble_data)
if (has_scenarios) {
  scenarios <- unique(ensemble_data$scenario)
  PEcAn.logger::logger.info("Scenario mode: ", paste(scenarios, collapse = ", "))
} else {
  scenarios <- "baseline"
  ensemble_data <- ensemble_data |> dplyr::mutate(scenario = "baseline")
}

PEcAn.logger::logger.info(
  "Loaded ensemble data:", nrow(ensemble_data), "rows;",
  dplyr::n_distinct(ensemble_data$site_id), "unique site_ids;",
  dplyr::n_distinct(ensemble_data$ensemble), "ensembles;",
  dplyr::n_distinct(ensemble_data$pft), "PFTs;",
  length(scenarios), "scenarios;",
  dplyr::n_distinct(ensemble_data$variable), "variables (carbon pools) in file; load_time_s=",
  round(step_elapsed(timer_read_ensemble), 2)
)
log_mem("After loading ensemble data :: ")

ensemble_ids <- ensemble_data |>
  dplyr::pull(ensemble) |>
  unique()

start_date <- lubridate::as_date(min(ensemble_data$datetime))
end_date <- lubridate::as_date(max(ensemble_data$datetime))

# Load ca_fields. Stash it in memory so back-to-back interactive
# sessions don't re-read the gpkg.
## TODO: switch to memoise::memoise() or pass it in as an arg.
if (!exists("ca_fields_full")) {
  ca_fields_full <- sf::read_sf(file.path(data_dir, "ca_fields.gpkg"))
}

ca_fields <- ca_fields_full |>
  sf::st_drop_geometry() |>
  dplyr::select(site_id, county, area_ha)

# Normalize reference table to one row per site_id and warn if duplicates exist
dup_counts <- ca_fields |>
  dplyr::count(site_id, name = "n") |>
  dplyr::filter(n > 1)
if (nrow(dup_counts) > 0) {
  PEcAn.logger::logger.warn(
    "ca_fields has duplicate site_id rows: ", nrow(dup_counts),
    "; collapsing to first observed county/area per site_id. Examples: ",
    paste(utils::head(dup_counts$site_id, 5), collapse = ", ")
  )
}
ca_fields <- ca_fields |>
  dplyr::group_by(site_id) |>
  dplyr::summarise(
    county  = dplyr::first(county),
    area_ha = dplyr::first(area_ha),
    .groups = "drop"
  )

ca_field_attributes <- readr::read_csv(file.path(data_dir, "ca_field_attributes.csv"))

# Pick PFTs that show up in both the ensemble data and the field table
ensemble_pfts <- sort(unique(ensemble_data$pft))
field_pfts <- sort(unique(ca_field_attributes$pft))
pfts <- intersect(ensemble_pfts, field_pfts)

if (length(pfts) == 0) {
  PEcAn.logger::logger.error("No overlapping PFTs between ensemble data and field attributes")
} else {
  PEcAn.logger::logger.info("Downscaling will be performed for these PFTs:", paste(pfts, collapse = ", "))
}

# Load site covariates
covariates_csv <- file.path(data_dir, "site_covariates.csv")
timer_read_cov <- step_timer()
covariates <- readr::read_csv(covariates_csv) |>
  dplyr::select(
    site_id, where(is.numeric),
    -climregion_id
  )
PEcAn.logger::logger.info(
  "Loaded covariates:", nrow(covariates), "sites x", ncol(covariates) - 1, "numeric predictors; load_time_s=",
  round(step_elapsed(timer_read_cov), 2)
)
log_mem("After loading covariates :: ")

covariate_names <- names(covariates |>
  dplyr::select(where(is.numeric)))

PEcAn.logger::logger.info(
  "Downscaling will use these covariates:\n\n",
  paste(covariate_names, collapse = ", ")
)

# Variable-importance helpers. Handle randomForest or ranger fits.
safe_sanitize <- function(x) {
  gsub("[^A-Za-z0-9]+", "_", x)
}

extract_vi <- function(model) {
  # Supports randomForest and ranger models
  if (inherits(model, "randomForest")) {
    vi <- tryCatch(randomForest::importance(model), error = function(e) NULL)
    if (is.null(vi)) {
      return(NULL)
    }
    if ("%IncMSE" %in% colnames(vi)) as.numeric(vi[, "%IncMSE"]) else as.numeric(vi[, 1])
  } else if (inherits(model, "ranger")) {
    vi <- tryCatch(model$variable.importance, error = function(e) NULL)
    if (is.null(vi)) {
      return(NULL)
    }
    as.numeric(vi)
  } else {
    NULL
  }
}

extract_vi_names <- function(model) {
  if (inherits(model, "randomForest")) {
    vi <- tryCatch(randomForest::importance(model), error = function(e) NULL)
    if (is.null(vi)) {
      return(NULL)
    }
    rownames(vi)
  } else if (inherits(model, "ranger")) {
    nms <- tryCatch(names(model$variable.importance), error = function(e) NULL)
    nms
  } else {
    NULL
  }
}

extract_oob_r2 <- function(model, y_train = NULL) {
  if (inherits(model, "randomForest")) {
    if (!is.null(model$predicted) && !is.null(model$y)) {
      y <- model$y
      yhat <- model$predicted
      if (length(y) == length(yhat) && stats::var(y) > 0) {
        return(1 - sum((y - yhat)^2) / sum((y - mean(y))^2))
      }
    }
    return(NA_real_)
  }
  if (inherits(model, "ranger")) {
    mse_oob <- tryCatch(model$prediction.error, error = function(e) NA_real_)
    if (!is.na(mse_oob)) {
      if (is.null(y_train)) {
        return(NA_real_)
      }
      v <- stats::var(y_train)
      if (!is.finite(v) || v <= 0) {
        return(NA_real_)
      }
      return(1 - mse_oob / v)
    }
  }
  NA_real_
}

# Convert model predictions from native to reporting units.
#   pools  (kg/m2)   -> Mg/ha
#   fluxes (kg/m2/s) -> kg/ha/yr
# Works on a single model_output string or a vectorized column passed in
# from dplyr::mutate (the if_else branch handles the latter).
convert_to_reporting_units <- function(prediction, model_output) {
  std_vars <- PEcAn.utils::standard_vars

  flux_vars <- std_vars |>
    dplyr::filter(stringr::str_detect(tolower(Category), "flux")) |>
    dplyr::pull(Variable.Name)

  is_flux <- model_output %in% flux_vars

  seconds_per_year <- 365.25 * 86400
  m2_per_ha <- 1e4

  if (length(is_flux) == 1) {
    if (is_flux) {
      prediction * seconds_per_year * m2_per_ha  # kg/m2/s -> kg/ha/yr
    } else {
      PEcAn.utils::ud_convert(prediction, "kg/m2", "Mg/ha")
    }
  } else {
    dplyr::if_else(
      is_flux,
      prediction * seconds_per_year * m2_per_ha,
      prediction * 10  # 1 kg/m2 = 10 Mg/ha
    )
  }
}

# Define design points based on ensemble data.
# TODO: move this sanitization upstream to when ens data is created (030_extract_sipnet_output.R)
# or better ... figure out why we so often run into mis-match!!!
# at least this time the missing site_ids all had matches within a few micrometers (10^-6 m)

# Check for missing design points in covariates and match by proximity if needed
# required_dp_cols <- c("site_id", "lat", "lon", "pft")
# .design_points <- ensemble_data |>
#   dplyr::select(dplyr::any_of(required_dp_cols)) |>
#   dplyr::distinct()

# design_points <- update_design_point_site_ids(
#   .design_points,
#   ca_field_attributes
# )


# Load design points from site_info.csv
site_info <- readr::read_csv(file.path(pecan_outdir, "site_info.csv"))
design_points <- site_info |>
  dplyr::transmute(
    site_id = id,
    lat = lat,
    lon = lon,
    pft = dplyr::case_when(
      site.pft == "annual_crop" ~ "annual crop",
      TRUE ~ site.pft
    )
  )
PEcAn.logger::logger.info("Loaded ", nrow(design_points), " design points from site_info.csv")

stopifnot(all(design_points$site_id %in% covariates$site_id))

# should we ever get here?
if (!all(design_points$site_id %in% ensemble_data$site_id)) {
  ensemble_data2 <- ensemble_data |>
    dplyr::left_join(design_points, by = c("lat", "lon", "pft"), suffix = c("", ".dp")) |>
    dplyr::mutate(site_id = site_id.dp) |>
    dplyr::select(-site_id.dp)
  n_missing <- setdiff(design_points$site_id, ensemble_data2$site_id) |>
    length()

  if (n_missing > 0) {
    PEcAn.logger::logger.error(
      n_missing, "design points still missing from ensemble data after matching",
      "this is already a hack, time to sort it out upstream!"
    )
  }
  ensemble_data <- ensemble_data2
}
stopifnot(any(design_points$site_id %in% ensemble_data$site_id))


# Keep both scaled and unscaled copies. Unscaled feeds prediction;
# scaled is for model diagnostics / plots.
design_covariates_unscaled <- design_points |>
  dplyr::left_join(covariates, by = "site_id") |>
  dplyr::select(site_id, dplyr::all_of(covariate_names)) |>
  as.data.frame()

# Scaled numeric design covariates for model diagnostics/plots
design_covariates <- design_covariates_unscaled |>
  # randomForest needs a plain data.frame, not a tibble
  as.data.frame() |>
  # scale to match what the model sees
  dplyr::mutate(dplyr::across(dplyr::all_of(covariate_names), scale))

# Check again that the design points all have covariate rows
n_not_in_covariates_after <- setdiff(design_points$site_id, covariates$site_id) |>
  length()
if (n_not_in_covariates_after > 0) {
  PEcAn.logger::logger.error(n_not_in_covariates_after, "design points still missing covariate data after matching")
}

all(design_points$site_id %in% covariates$site_id)

# Keep full covariates and perform per-PFT sampling later (dev mode)
covariates_full <- covariates
if (!PRODUCTION) {
  if (!exists(".Random.seed")) set.seed(123)
  PEcAn.logger::logger.info("Development mode: will sample up to 10k prediction sites per PFT")
  # keep ca_field_attributes consistent with available covariates
  ca_field_attributes <- ca_field_attributes |>
    dplyr::filter(site_id %in% covariates_full$site_id)
}

# Build list of site_ids per PFT from field attributes
pft_site_ids <- ca_field_attributes |>
  dplyr::filter(pft %in% pfts) |>
  dplyr::distinct(site_id, pft)

sites_info <- pft_site_ids |>
  dplyr::group_by(pft) |>
  dplyr::summarise(n_sites = dplyr::n(), .groups = "drop")
PEcAn.logger::logger.info(
  "Sites per PFT:",
  paste(paste0(sites_info$pft, "=", sites_info$n_sites), collapse = "; ")
)
log_mem("After computing sites per PFT :: ")

# Downscale one carbon pool given an explicit training set and target sites.
## TODO: refactor and rename to downscale_ensemble_output()
downscale_model_output <- function(date,
                                   model_output,
                                   train_ensemble_data,
                                   train_site_coords = design_points,
                                   pred_covariates = covariates) {
  # Restrict training coords to sites that actually have ensemble data
  ens_sites <- unique(train_ensemble_data$site_id)
  train_site_coords <- train_site_coords[train_site_coords$site_id %in% ens_sites, , drop = FALSE]

  if (nrow(train_site_coords) == 0) {
    PEcAn.logger::logger.warn("No overlapping training sites between site_coords and ensemble data for pool", model_output)
    return(NULL)
  }

  filtered_ens_data <- PEcAnAssimSequential::subset_ensemble(
    ensemble_data = train_ensemble_data,
    site_coords   = train_site_coords,
    date          = date,
    carbon_pool   = model_output
  )
  if (is.null(filtered_ens_data) || nrow(filtered_ens_data) == 0) {
    PEcAn.logger::logger.warn("Filtered ensemble data is empty for pool", model_output)
    return(NULL)
  }

  # Filter out NA values in prediction column (can occur from SIPNET model instability)
  n_before <- nrow(filtered_ens_data)
  filtered_ens_data <- filtered_ens_data |>
    dplyr::filter(!is.na(prediction))
  n_after <- nrow(filtered_ens_data)
  if (n_before > n_after) {
    PEcAn.logger::logger.warn(
      "Removed ", n_before - n_after, " rows with NA predictions for pool ", model_output,
      " (", round(100 * (n_before - n_after) / n_before, 1), "% of data)"
    )
  }
  if (nrow(filtered_ens_data) == 0) {
    PEcAn.logger::logger.warn("All data is NA for pool ", model_output, " - skipping")
    return(NULL)
  }

  # Update train_site_coords to only include sites that remain after NA filtering
  valid_sites <- unique(filtered_ens_data$site_id)
  train_site_coords <- train_site_coords[train_site_coords$site_id %in% valid_sites, , drop = FALSE]
  if (nrow(train_site_coords) == 0) {
    PEcAn.logger::logger.warn("No valid training sites remain after NA filtering for pool ", model_output)
    return(NULL)
  }

  ## BEGIN HACK FOR SMALL-N
  # n_unique_sites <- dplyr::n_distinct(train_site_coords$site_id)
  # if (n_unique_sites <= 12) {
  #   source(here::here("R", "temporary_hack_fit_predict_small_n.R"))
  #   PEcAn.logger::logger.info("Small-N mode: training per-ensemble RF with nodesize=1 and no test split")
  #   downscale_output <- fit_predict_small_n(
  #     filtered_ens = filtered_ens_data,
  #     pred_covariates = pred_covariates,
  #     covariate_names = covariate_names, # already defined earlier in 040
  #     nodesize = 1,
  #     ntree = 1000
  #   )
  # } else {
  ## END HACK FOR SMALL-N
  downscale_output <-
    PEcAnAssimSequential::ensemble_downscale(
      ensemble_data = filtered_ens_data,
      site_coords   = train_site_coords,
      covariates    = pred_covariates
    )
  # } ## REMOVE WITH HACK FOR SMALL-N
  if (is.null(downscale_output)) {
    return(NULL)
  }

  # Attach the site_ids used for prediction to keep mapping explicit
  downscale_output$site_ids <- pred_covariates$site_id
  return(downscale_output)
}

# Loop over scenario / pool / PFT one at a time. furrr is already used
# inside ensemble_downscale, so don't nest it out here.

downscale_output_list <- list()
delta_output_records <- list()
training_sites_records <- list()
combo_total <- length(scenarios) * length(outputs_to_extract) * length(pfts)
combo_index <- 0L
loop_global_timer <- step_timer()

for (scenario_i in scenarios) {
  PEcAn.logger::logger.info("Processing scenario: ", scenario_i)

  for (pool in outputs_to_extract) {
    for (pft_i in pfts) {
      combo_index <- combo_index + 1L
      iter_timer <- step_timer()
      PEcAn.logger::logger.info(
        sprintf(
          "[Progress %d/%d] %s / %s / %s at %s",
          combo_index, combo_total, scenario_i, pool, pft_i, ts_now()
        )
      )

      train_ens <- ensemble_data |>
        dplyr::filter(scenario == scenario_i, pft == pft_i, variable == pool)

      # Skip empty slices early
      if (nrow(train_ens) == 0) {
        PEcAn.logger::logger.warn("No data for ", scenario_i, "::", pft_i, "::", pool, " - skipping")
        next
      }

      # Determine per-slice end date and warn if ensembles disagree
      slice_end_date <- as.Date(max(train_ens$datetime))
      end_by_ens <- train_ens |>
        dplyr::group_by(ensemble) |>
        dplyr::summarise(last_date = max(lubridate::as_date(datetime)), .groups = "drop")
      if (dplyr::n_distinct(end_by_ens$last_date) > 1) {
        PEcAn.logger::logger.warn(
          "End dates vary across ensembles for ", scenario_i, "::", pft_i, "::", pool
        )
      }

      train_pts <- train_ens |>
        dplyr::select(site_id, lat, lon, pft) |>
        dplyr::distinct()

      # Diagnostic: overlapping site counts
      n_train_ens_sites <- length(unique(train_ens$site_id))
      n_train_pts <- nrow(train_pts)
      PEcAn.logger::logger.info("Training sites: ", n_train_ens_sites, "; coords: ", n_train_pts)
      training_sites_records[[paste0(scenario_i, "::", pft_i, "::", pool)]] <-
        tibble::tibble(site_id = unique(train_pts$site_id), scenario = scenario_i, pft = pft_i, model_output = pool)

    # Ensure design point covariates are included for training join
    dp_pft <- design_covariates_unscaled |>
      dplyr::filter(site_id %in% (design_points |>
        dplyr::filter(pft == pft_i) |>
        dplyr::pull(site_id)))
    # prediction covariates: either full set for that PFT (prod) or sampled subset (dev)
    if (PRODUCTION) {
      pred_covs <- covariates_full |>
        dplyr::filter(site_id %in% (pft_site_ids |> dplyr::filter(pft == pft_i) |> dplyr::pull(site_id))) |>
        dplyr::bind_rows(dp_pft) |>
        dplyr::distinct(site_id, .keep_all = TRUE)
    } else {
      sample_pool <- covariates_full |>
        dplyr::filter(site_id %in% pft_site_ids[[pft_i]]) |>
        dplyr::anti_join(dp_pft, by = "site_id")

      n_sample <- min(10000, nrow(sample_pool))
      sampled <- if (n_sample > 0) {
        sample_pool |>
          dplyr::slice_sample(n = n_sample)
      } else {
        sample_pool
      }

      pred_covs <- dplyr::bind_rows(sampled, dp_pft)
    }

    # Guard for empty prediction covariates (both development mode and production)
    if (nrow(pred_covs) == 0) {
      PEcAn.logger::logger.warn("No prediction covariates for PFT:", pft_i, " pool:", pool, " -- skipping")
      next
    }

    # Sanity: ensure all training site_ids exist in prediction covariates
    missing_train_cov <- setdiff(unique(train_pts$site_id), pred_covs$site_id)
    if (length(missing_train_cov) > 0) {
      PEcAn.logger::logger.error(
        "Missing covariates for training site_ids (", length(missing_train_cov), ") in PFT ", pft_i,
        ": ", paste(utils::head(missing_train_cov, 10), collapse = ", "),
        if (length(missing_train_cov) > 10) " ..." else ""
      )
    }

    call_timer <- step_timer()
    result <- downscale_model_output(
      date = slice_end_date,
      model_output = pool,
      train_ensemble_data = train_ens,
      train_site_coords = train_pts,
      pred_covariates = pred_covs
    )
    PEcAn.logger::logger.info(
      "Completed downscaling for", pool, "(", pft_i, ") in",
      round(step_elapsed(call_timer), 2), "s; n_pred_sites=", nrow(pred_covs),
      " n_train_sites=", n_train_pts
    )
    log_mem(paste0("After downscaling ", pool, " (", pft_i, ") :: "))
    # Also compute start-date predictions to enable delta maps
    start_obj <- downscale_model_output(
      date = start_date,
      model_output = pool,
      train_ensemble_data = train_ens,
      train_site_coords = train_pts,
      pred_covariates = pred_covs
    )

    # Save models and VI metrics if available from downscale outputs
    if (!is.null(result) && !is.null(result$model)) {
      models_dir <- file.path(cache_dir, "models")
      train_dir <- file.path(cache_dir, "training_data")
      if (!dir.exists(models_dir)) dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
      if (!dir.exists(train_dir)) dir.create(train_dir, recursive = TRUE, showWarnings = FALSE)

      spec_key <- paste0(janitor::make_clean_names(pft_i), "_", janitor::make_clean_names(pool))
      saveRDS(result$model, file = file.path(models_dir, paste0(spec_key, "_models.rds")))

      # Write the explicit training covariate matrix for the sites used to fit the model
      tr_covs <- dp_pft |>
        dplyr::semi_join(train_pts, by = "site_id") |>
        dplyr::select(site_id, dplyr::all_of(covariate_names))
      tr_path <- file.path(train_dir, paste0(spec_key, "_training.csv"))
      readr::write_csv(tr_covs, tr_path)

      ens_labels <- names(result$predictions)
      if (is.null(ens_labels)) ens_labels <- as.character(seq_along(result$predictions))
      vi_rows <- list()
      for (mi in seq_along(result$model)) {
        mdl <- result$model[[mi]]
        vi_vals <- extract_vi(mdl)
        vi_nms <- extract_vi_names(mdl)
        if (is.null(vi_vals) || is.null(vi_nms)) next
        y_train <- NULL
        if (!is.null(result$data) && !is.null(result$data$training)) {
          tr <- result$data$training
          if ("ensemble" %in% names(tr) && "prediction" %in% names(tr)) {
            y_train <- tr$prediction[tr$ensemble == ens_labels[mi]]
          }
        } else if (!is.null(mdl$y)) {
          y_train <- mdl$y
        }
        # TODO OOB r2 is within-training only (bootstrap from same sites)
        # Add spatial cross-validation (e.g., leave-one-site-out or spatial
        # blocking).
        r2_oob <- extract_oob_r2(mdl, y_train)
        vi_rows[[length(vi_rows) + 1L]] <- tibble::tibble(
          pft = pft_i,
          model_output = pool,
          ensemble = ens_labels[mi],
          predictor = vi_nms,
          importance = as.numeric(vi_vals[seq_along(vi_nms)]),
          oob_r2 = r2_oob
        )
      }
      if (length(vi_rows) > 0) {
        vi_tbl <- dplyr::bind_rows(vi_rows)
        out_vi_per_ens <- file.path(model_outdir, paste0("vi_", spec_key, "_by_ensemble.csv"))
        readr::write_csv(vi_tbl, out_vi_per_ens)
      }
    }

    downscale_output_list[[paste0(scenario_i, "::", pft_i, "::", pool)]] <- result
    if (!is.null(result) && !is.null(start_obj)) {
      end_df <- purrr::map(
        result$predictions,
        ~ tibble::tibble(site_id = result$site_ids, prediction = .x)
      ) |>
        dplyr::bind_rows(.id = "ensemble") |>
        dplyr::rename(end_pred = prediction)
      start_df <- purrr::map(
        start_obj$predictions,
        ~ tibble::tibble(site_id = start_obj$site_ids, prediction = .x)
      ) |>
        dplyr::bind_rows(.id = "ensemble") |>
        dplyr::rename(start_pred = prediction)
      delta_df <- end_df |>
        dplyr::inner_join(start_df, by = c("site_id", "ensemble")) |>
        dplyr::mutate(delta_pred = end_pred - start_pred) |>
        dplyr::left_join(ca_fields, by = "site_id") |>
        dplyr::mutate(
          pft = pft_i,
          model_output = pool,
          delta_density_per_ha = convert_to_reporting_units(delta_pred, pool),
          delta_total = delta_density_per_ha * area_ha
        ) |>
        dplyr::select(site_id, pft, ensemble, delta_density_per_ha, delta_total, area_ha, county, model_output)
      delta_output_records[[paste0(scenario_i, "::", pft_i, "::", pool)]] <- delta_df
    }

    # Incremental checkpoint (so production runs can be resumed)
    if (!is.null(result)) {
      tryCatch(
        {
          saveRDS(downscale_output_list, file = file.path(cache_dir, "downscale_partial.rds"))
        },
        error = function(e) {
          PEcAn.logger::logger.warn("Failed to write partial checkpoint: ", conditionMessage(e))
        }
      )
    }
    PEcAn.logger::logger.info(
      sprintf(
        "[Progress %d/%d] Finished %s (%s); iter_time_s=%.2f; elapsed_total_s=%.2f",
        combo_index, combo_total, pool, pft_i,
        step_elapsed(iter_timer), step_elapsed(loop_global_timer)
      )
    )
    } # end pft_i loop
  } # end pool loop
} # end scenario_i loop

PEcAn.logger::logger.info(
  "Downscaling loop complete; total_elapsed_s=",
  round(step_elapsed(loop_global_timer), 2)
)

if (length(downscale_output_list) == 0) {
  PEcAn.logger::logger.severe("No downscale outputs produced")
}

log_mem("Post primary downscaling loop :: ")

PEcAn.logger::logger.info("Finished downscaling.")

# Per-ensemble downscaling metrics

PEcAn.logger::logger.info("Downscaling model results for each ensemble member:")
metrics_timer <- step_timer()
metrics <- lapply(downscale_output_list, PEcAnAssimSequential::downscale_metrics)

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
PEcAn.logger::logger.info(
  "Computed median metrics in", round(step_elapsed(metrics_timer), 2), "s"
)

if (!PRODUCTION) {
  # For testing, use a subset of fields
  # could be even faster if we queried from gpkg:
  #    sf::read_sf(..., sql = "SELECT * FROM ca_fields WHERE site_id IN (...)")
  ca_fields <- ca_fields |>
    dplyr::right_join(covariates, by = "site_id")
}

# Tidy one downscale object into site_id x ensemble x prediction,
# joined to ca_fields for area/county.
get_downscale_preds <- function(downscale_obj) {
  purrr::map(
    downscale_obj$predictions,
    ~ tibble::tibble(site_id = downscale_obj$site_ids, prediction = .x)
  ) |>
    dplyr::bind_rows(.id = "ensemble") |>
    dplyr::left_join(ca_fields, by = "site_id")
}

# Assemble predictions; carry scenario/PFT label by parsing element name: "{scenario}::{pft}::{pool}"
downscale_preds <- purrr::map(downscale_output_list, get_downscale_preds) |>
  dplyr::bind_rows(.id = "spec") |>
  tidyr::separate(
    col = "spec",
    into = c("scenario", "pft", "model_output"),
    sep = "::",
    remove = TRUE
  ) |>
  dplyr::mutate(density_per_ha = convert_to_reporting_units(prediction, model_output)) |>
  dplyr::mutate(total_per_field = density_per_ha * area_ha) |>
  dplyr::select(site_id, scenario, pft, ensemble, density_per_ha, total_per_field, area_ha, county, model_output, -prediction)

dp <- downscale_preds |>
  dplyr::select(
    site_id, scenario, pft, ensemble,
    density_per_ha, total_per_field,
    area_ha, county, model_output
  )

## Mixed scenario: orchard overlap with 50% grass on woody fields
# Goal: add an additional PFT record "woody + annual" computed as:
#   combined = woody_value + f_annual * (annual_end - annual_start)
# where values are in kg/m2 and f_annual = 0.5.

# Identify labels used for woody and annual PFTs
# TODO use lookup table to map crops --> pfts and get labels
woody_label <- pfts[grepl("woody", pfts, ignore.case = TRUE)]
annual_label <- pfts[grepl("annual", pfts, ignore.case = TRUE)]

if (length(woody_label) == 0 || length(annual_label) == 0) {
  PEcAn.logger::logger.warn("Cannot build mixed scenario: missing woody or annual PFT")
} else {
  PEcAn.logger::logger.info(
    "Building mixed scenario 'woody + annual' using overlap (incremental) with 50% annual cover"
  )
  
  # Helper to tidy a downscale object to site_id/ensemble/prediction (kg/m2)
  tidy_downscale <- function(ds) {
    purrr::map(
      ds$predictions,
      ~ tibble::tibble(site_id = ds$site_ids, prediction = .x)
    ) |>
      dplyr::bind_rows(.id = "ensemble")
  }
  
  # Determine target woody sites that exist in current dp
  target_woody_sites <- dp |>
    dplyr::filter(pft == woody_label) |>
    dplyr::distinct(site_id) |>
    dplyr::pull(site_id)
  
  if (length(target_woody_sites) == 0) {
    PEcAn.logger::logger.warn("No woody sites found in downscaled predictions; skipping mixed scenario")
  } else {
    # Build covariates for predicting annual onto woody sites, ensuring
    # design-point covariates for the annual PFT are available for training join
    dp_annual <- design_covariates_unscaled |>
      dplyr::filter(site_id %in% (design_points |>
                                    dplyr::filter(pft == annual_label) |>
                                    dplyr::pull(site_id)))
    
    pred_cov_mixed <- covariates_full |>
      dplyr::filter(site_id %in% target_woody_sites) |>
      dplyr::bind_rows(dp_annual) |>
      dplyr::distinct(site_id, .keep_all = TRUE)
    
    mixed_records <- list()
    
    for (pool in outputs_to_extract) {
      # Training data for annual
      train_ens_annual <- ensemble_data |>
        dplyr::filter(pft == annual_label & variable == pool)
      
      if (nrow(train_ens_annual) == 0) {
        PEcAn.logger::logger.warn("No annual ensemble data for pool ", pool, "; skipping mixed for this pool")
        next
      }
      
      train_pts_annual <- train_ens_annual |>
        dplyr::select(site_id, lat, lon, pft) |>
        dplyr::distinct()
      
      # Annual predictions at start and end dates on woody sites
      annual_start_obj <- downscale_model_output(
        date = start_date,
        model_output = pool,
        train_ensemble_data = train_ens_annual,
        train_site_coords = train_pts_annual,
        pred_covariates = pred_cov_mixed
      )
      annual_end_obj <- downscale_model_output(
        date = end_date,
        model_output = pool,
        train_ensemble_data = train_ens_annual,
        train_site_coords = train_pts_annual,
        pred_covariates = pred_cov_mixed
      )
      
      # Get woody predictions at end date from existing results
      woody_key <- paste0(woody_label, "::", pool)
      woody_obj <- downscale_output_list[[woody_key]]
      
      if (is.null(annual_start_obj) || is.null(annual_end_obj) || is.null(woody_obj)) {
        PEcAn.logger::logger.warn("Missing components for mixed scenario in pool ", pool, "; skipping")
        next
      }
      
      woody_df <- tidy_downscale(woody_obj) |>
        dplyr::filter(site_id %in% target_woody_sites) |>
        dplyr::rename(woody_pred = prediction)
      
      ann_start_df <- tidy_downscale(annual_start_obj) |>
        dplyr::filter(site_id %in% target_woody_sites) |>
        dplyr::rename(annual_start = prediction)
      
      ann_end_df <- tidy_downscale(annual_end_obj) |>
        dplyr::filter(site_id %in% target_woody_sites) |>
        dplyr::rename(annual_end = prediction)
      
      # Join by site_id and ensemble to align predictions (include annual_start for SOC)
      mix_df <- woody_df |>
        dplyr::inner_join(ann_end_df,  by = c("site_id", "ensemble")) |>
        dplyr::inner_join(ann_start_df, by = c("site_id", "ensemble"))
      
      if (nrow(mix_df) == 0) {
        PEcAn.logger::logger.warn("No overlapping site/ensemble rows for mixed scenario in pool ", pool)
        next
      }
      
      f_annual <- 0.5 # TODO: will come from monitoring / scenario data later
      mix_df <- mix_df |>
        dplyr::mutate(
          mixed_pred = combine_mixed_crops(
            woody_value = .data$woody_pred,
            annual_value = .data$annual_end,
            annual_init = if (pool == "AGB") 0 else .data$annual_start,
            annual_cover = f_annual,
            woody_cover = 1.0,
            method = "incremental"
          )
        ) |>
        # add area/county for totals
        dplyr::left_join(ca_fields, by = "site_id") |>
        dplyr::mutate(
          pft = "woody + annual",
          model_output = pool,
          density_per_ha = convert_to_reporting_units(mixed_pred, pool),
          total_per_field = density_per_ha * area_ha
        ) |>
        dplyr::select(site_id, pft, ensemble, density_per_ha, total_per_field, area_ha, county, model_output)
      
      mixed_records[[pool]] <- mix_df
      
      # Also save per-site treatment scenarios on woody fields for comparisons
      woody_scn <- woody_df |>
        dplyr::left_join(ca_fields, by = "site_id") |>
        dplyr::mutate(
          pft = pft_i,
          model_output = pool,
          scenario = "woody_100",
          density_per_ha = convert_to_reporting_units(woody_pred, pool),
          total_per_field = density_per_ha * area_ha
        ) |>
        dplyr::select(site_id, pft, ensemble, scenario, density_per_ha, total_per_field, area_ha, county, model_output)

      annual_scn <- ann_end_df |>
        dplyr::left_join(ca_fields, by = "site_id") |>
        dplyr::mutate(
          pft = pft_i,
          model_output = pool,
          scenario = "annual_100",
          density_per_ha = convert_to_reporting_units(annual_end, pool),
          total_per_field = density_per_ha * area_ha
        ) |>
        dplyr::select(site_id, pft, ensemble, scenario, density_per_ha, total_per_field, area_ha, county, model_output)

      mixed_scn <- mix_df |>
        dplyr::mutate(scenario = "woody_50_annual_50") |>
        dplyr::select(site_id, pft, ensemble, scenario, density_per_ha, total_per_field, area_ha, county, model_output)
      
      # accumulate
      if (!exists("treatment_records", inherits = FALSE)) treatment_records <- list()
      treatment_records[[length(treatment_records) + 1L]] <- dplyr::bind_rows(woody_scn, annual_scn, mixed_scn)
    }
    
    if (length(mixed_records) > 0) {
      mixed_df_all <- dplyr::bind_rows(mixed_records, .id = "pool") |>
        dplyr::select(-pool)
      dp <- dplyr::bind_rows(dp, mixed_df_all)
    }
  }
}


## Write out downscaled predictions

# Helper function to write CSV and optionally parquet
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

write_output(
  dp,
  file.path(model_outdir, "downscaled_preds"),
  "Downscaled predictions"
)

# Training sites
if (length(training_sites_records) > 0) {
  train_sites_df <- dplyr::bind_rows(training_sites_records) |>
    dplyr::distinct()
  write_output(
    train_sites_df,
    file.path(model_outdir, "training_sites"),
    "Training site list"
  )
}
metadata <- list(
  title = "Downscaled SIPNET Outputs",
  description = "SIPNET model outputs downscaled to field level using Random Forest",
  created = Sys.time(),
  ensembles = sort(unique(as.integer(ensemble_ids))),
  pfts = pfts,
  scenarios = scenarios,
  outputs_to_extract = outputs_to_extract,
  start_date = as.character(start_date),
  end_date = as.character(end_date),
  mixed_cover_fraction = if (length(scenarios) == 1) 0.5 else NA,
  columns = list(
    site_id = "Unique identifier for each field from LandIQ",
    pft = "Plant functional type",
    scenario = "Management scenario (baseline, compost, etc.)",
    ensemble = "Ensemble member identifier",
    density_per_ha = "Predicted density per hectare: Mg/ha for pools, kg/ha/yr for fluxes",
    total_per_field = "Predicted total per field: density_per_ha * area_ha",
    area_ha = "Field area in hectares",
    county = "California county name where the field is located",
    model_output = "Type of SIPNET model output (e.g., AGB, TotSoilCarb, N2O_flux, CH4_flux)"
  )
)

metadata |>
  jsonlite::write_json(
    file.path(model_outdir, "downscaled_preds_metadata.json"),
    pretty = TRUE, auto_unbox = TRUE
  )

# Delta predictions
if (length(delta_output_records) > 0) {
  delta_dp <- dplyr::bind_rows(delta_output_records, .id = "spec") |>
    tidyr::separate(col = "spec", into = c("scenario", "pft", "model_output"), sep = "::", remove = TRUE)
  write_output(
    delta_dp,
    file.path(model_outdir, "downscaled_deltas"),
    "Delta predictions"
  )
}

# Treatment comparisons
if (exists("treatment_records") && length(treatment_records) > 0) {
  treatments_df <- dplyr::bind_rows(treatment_records)
  write_output(
    treatments_df,
    file.path(model_outdir, "treatments_woody_sites"),
    "Treatment scenarios"
  )
}

PEcAn.logger::logger.info(
  "Total script elapsed time (s):",
  round(step_elapsed(overall_timer), 2)
)
log_mem("End of script :: ")
