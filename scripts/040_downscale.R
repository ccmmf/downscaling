# This workflow does the following:
#
# - Use environmental covariates to predict SIPNET estimated SOC for each field in the LandIQ dataset
#   - Uses Random Forest [may change to CNN later] trained on site-scale model runs.
#   - Build a model for each ensemble member
# - Write out a table with predicted biomass and SOC to maintain ensemble structure, ensuring correct error propagation and spatial covariance.
# - Aggregates County-level biomass and SOC inventories
#
## ----debugging--------------------------------------------------------------------
# debugonce(PEcAnAssimSequential::ensemble_downscale)
# PEcAn.logger::logger.setQuitOnSevere(TRUE)
# ----setup--------------------------------------------------------------------


source("000-config.R")
PEcAn.logger::logger.info("***Starting Downscaling and Aggregation***")

#----- load ensemble data ----------------------------------------------------
ensemble_csv <- file.path(model_outdir, "ensemble_output.csv")
ensemble_data <- readr::read_csv(ensemble_csv) |>
  dplyr::rename(
    ensemble = parameter # parameter is EFI std name for ensemble
  )

ensemble_ids <- ensemble_data |>
  dplyr::pull(ensemble) |>
  unique()

start_date <- lubridate::as_date(min(ensemble_data$datetime))
end_date <- lubridate::as_date(max(ensemble_data$datetime))

#--- load ca_fields ------------------------------------------------
# this is a convenience time saver for development
if (!exists("ca_fields_full")) {
  ca_fields_full <- sf::read_sf(file.path(data_dir, "ca_fields.gpkg"))
}

ca_fields <- ca_fields_full |>
  dplyr::select(site_id, county, area_ha)

ca_field_attributes <- readr::read_csv(file.path(data_dir, "ca_field_attributes.csv"))

# Determine PFTs and map ensemble keys (e.g. 'woody') to field labels
ensemble_pfts <- sort(unique(ensemble_data$pft))
field_pfts <- sort(unique(ca_field_attributes$pft))

# map each ensemble key to itself (each key acts as its own label)
pfts <- intersect(ensemble_pfts, field_pfts)

if (length(pfts) == 0) {
  PEcAn.logger::logger.error("No overlapping PFTs between ensemble data and field attributes")
} else {
  PEcAn.logger::logger.info("Downscaling will be performed for these PFTs:", paste(pfts, collapse = ", "))
}

#--- load site covariates
covariates_csv <- file.path(data_dir, "site_covariates_plus_dp_patch.csv")
# covariates_csv <- file.path(data_dir, "site_covariates.csv")
covariates <- readr::read_csv(covariates_csv) |>
  dplyr::select(
    site_id, where(is.numeric),
    -climregion_id
  )

covariate_names <- names(covariates |>
  dplyr::select(where(is.numeric)))

PEcAn.logger::logger.info(
  "Downscaling will use these covariates:\n\n",
  paste(covariate_names, collapse = ", ")
)

# ----define design points based on ensemble data-------------------------------
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

# TODO: Need to put a canonical design_points CSV in repository
### FOR NOW, just use hard coded design points
design_points <- structure(list(site_id = c(
  "3a84c0268e1655a3", "3a84c0268e1655a3",
  "d523652b399a8f6e", "d523652b399a8f6e", "275102c035b15f5e", "275102c035b15f5e",
  "26ff9e8246f7c8f4", "26ff9e8246f7c8f4", "47cd11223bb49112", "47cd11223bb49112",
  "9a4c7e47fc0297bb", "9a4c7e47fc0297bb", "e5bb4dca46bd5041", "e5bb4dca46bd5041",
  "abd5a71d492e92e1", "abd5a71d492e92e1", "7fe5bb855fb36cdb", "7fe5bb855fb36cdb",
  "7bb77bae6ac3c147", "7bb77bae6ac3c147"
), lat = c(
  34.91295, 34.91295,
  34.38596, 34.38596, 34.47244, 34.47244, 33.86884, 33.86884, 34.29708,
  34.29708, 33.96727, 33.96727, 33.35306, 33.35306, 34.37258, 34.37258,
  33.90119, 33.90119, 33.57847, 33.57847
), lon = c(
  -120.40345,
  -120.40345, -118.81446, -118.81446, -119.22015, -119.22015, -117.40838,
  -117.40838, -119.06014, -119.06014, -117.34049, -117.34049, -117.19182,
  -117.19182, -119.03318, -119.03318, -117.40624, -117.40624, -116.03157,
  -116.03157
), pft = c(
  "woody perennial crop", "annual crop", "woody perennial crop", "annual crop", "woody perennial crop",
  "annual crop", "woody perennial crop", "annual crop", "woody perennial crop", "annual crop", "woody perennial crop", "annual crop",
  "woody perennial crop", "annual crop", "woody perennial crop", "annual crop", "woody perennial crop", "annual crop", "woody perennial crop",
  "annual crop"
)), row.names = c(NA, -20L), class = c(
  "tbl_df", "tbl",
  "data.frame"
))

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


# Scaled numeric design covariates for model diagnostics/plots
# Keep an unscaled copy of design covariates for prediction inputs
design_covariates_unscaled <- design_points |>
  dplyr::left_join(covariates, by = "site_id") |>
  dplyr::select(site_id, dplyr::all_of(covariate_names)) |>
  as.data.frame()

# Scaled numeric design covariates for model diagnostics/plots
design_covariates <- design_covariates_unscaled |>
  # randomForest pkg requires data frame
  as.data.frame() |>
  # scale covariates as for consistency with model
  dplyr::mutate(dplyr::across(dplyr::all_of(covariate_names), scale))

# Check again to ensure we've resolved the issue
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

sites_info <- lapply(pft_site_ids, length)
PEcAn.logger::logger.info("Sites per PFT:", paste(sites_info, collapse = ", "))

#### Target sites: per-PFT site lists built above (pft_site_ids)

## Wrapper to downscale a single carbon pool with explicit training set and target sites
# TODO refactor to to downscale_ensemble_output()
downscale_model_output <- function(date,
                                   model_output,
                                   train_ensemble_data,
                                   train_site_coords = design_points,
                                   pred_covariates = covariates) {
  # Ensure training site coords only include sites present in the ensemble slice
  # Restrict training coordinates to those present in the ensemble data
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
  # Downscale the data
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

# not using furrr b/c it is used inside downscale
# We downscale each carbon pool for both woody and annual PFTs,
# predicting to the same target set for that PFT
downscale_output_list <- list()
for (pool in outputs_to_extract) {
  for (pft_i in pfts) {
    PEcAn.logger::logger.info("Starting downscaling for", pool, "(", pft_i, ")")

    # train_ens: ensemble data filtered by PFT
    train_ens <- ensemble_data |>
      dplyr::filter(pft == pft_i & variable == pool)

    # Skip empty slices early
    if (nrow(train_ens) == 0) {
      PEcAn.logger::logger.warn("No ensemble rows for ", pft_i, "::", pool, " <U+2014> skipping")
      next
    }

    # Determine per-slice end date and warn if ensembles disagree
    slice_end_date <- as.Date(max(train_ens$datetime))
    end_by_ens <- train_ens |>
      dplyr::group_by(ensemble) |>
      dplyr::summarise(last_date = max(lubridate::as_date(datetime)), .groups = "drop")
    if (dplyr::n_distinct(end_by_ens$last_date) > 1) {
      PEcAn.logger::logger.warn(
        "End dates vary across ensembles for ", pft_i, "::", pool,
        "; using slice_end_date=", as.character(slice_end_date)
      )
    } else {
      PEcAn.logger::logger.info(
        "Using slice_end_date=", as.character(slice_end_date), " for ", pft_i, "::", pool
      )
    }
    # train_pts: design points filtered by PFT
    # should be the same as
    train_pts <- train_ens |>
      dplyr::select(site_id, lat, lon, pft) |>
      dplyr::distinct()
    # train_pts <- design_points |>
    #  dplyr::filter(pft == pft_i)

    # Quick diagnostic: overlapping site counts
    n_train_ens_sites <- length(unique(train_ens$site_id))
    n_train_pts <- nrow(train_pts)
    PEcAn.logger::logger.info("Training sites: ensemble has", n_train_ens_sites, "site_ids; using", n_train_pts, "coords")

    # NOTE: We skip strict pre-check; wrapper will attempt subset_ensemble then manual fallback.

    # prediction covariates: either full set for that PFT or sampled subset (dev)
    if (PRODUCTION) {
      # Ensure design point covariates are included for training join
      dp_pft <- design_covariates_unscaled |>
        dplyr::filter(site_id %in% (design_points |>
          dplyr::filter(pft == pft_i) |>
          dplyr::pull(site_id)))

      pred_covs <- covariates_full |>
        dplyr::filter(site_id %in% (pft_site_ids |> dplyr::filter(pft == pft_i) |> dplyr::pull(site_id))) |>
        dplyr::bind_rows(dp_pft) |>
        dplyr::distinct(site_id, .keep_all = TRUE)
    } else {
      dp_pft <- design_covariates_unscaled |>
        dplyr::filter(site_id %in% (design_points |>
          dplyr::filter(pft == pft_i) |>
          dplyr::pull(site_id)))

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

      # ensure prediction covariates have at least one site (development mode)
      if (nrow(pred_covs) == 0) {
        PEcAn.logger::logger.warn("No prediction covariates for PFT:", pft_i, " pool:", pool, " <U+2014> skipping")
        next
      }
    }

    # Guard for empty prediction covariates (both development mode and production)
    if (nrow(pred_covs) == 0) {
      PEcAn.logger::logger.warn("No prediction covariates for PFT:", pft_i, " pool:", pool, " <U+2014> skipping")
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

    result <- downscale_model_output(
      date = slice_end_date,
      model_output = pool,
      train_ensemble_data = train_ens,
      train_site_coords = train_pts,
      pred_covariates = pred_covs
    )

    PEcAn.logger::logger.info("Downscaling complete for", pool, "(", pft_i, ")")

    # store using pft::pool names (e.g. "woody::AGB").
    downscale_output_list[[paste0(pft_i, "::", pool)]] <- result
  }
}

if (length(downscale_output_list) == 0) {
  PEcAn.logger::logger.severe("No downscale outputs produced")
}
PEcAn.logger::logger.info("Downscaling complete for all model outputs")

## Save to make it easier to restart
#### ---Create checkpoint for downstream analysis---####
checkpoint_file <- file.path(cache_dir, "downscaling_output.RData")
start_end <- system.time(
  save(
    downscale_output_list,
    covariates,
    design_points,
    design_covariates,
    ensemble_ids,
    pft_site_ids,
    file = checkpoint_file,
    compress = FALSE
  )
)
PEcAn.logger::logger.info(
  "Downscaling output objects saved to", checkpoint_file,
  "\nIt took", round(start_end[3] / 60, 2), "minutes"
)

PEcAn.logger::logger.info(
  paste0("<U+0001F31F><U+0001F31F><U+0001F31F> Finished downscaling <U+0001F31F><U+0001F31F><U+0001F31F>"),
  "\n\nCongratulations! You are almost there!\n\n",
  rep("<U+0001F680>", 10)
)

### --- Print Metrics for Each Ensemble Member ---####

PEcAn.logger::logger.info("Downscaling model results for each ensemble member:")
metrics <- lapply(downscale_output_list, PEcAnAssimSequential::downscale_metrics) # nolint

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
