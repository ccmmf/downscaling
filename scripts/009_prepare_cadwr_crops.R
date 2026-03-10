#' Prepare CADWR crops data for downscaling
#'
#' Reads harmonized LandIQ v4.1 crops parquet, standardizes to the downscaling
#' format, and creates temporal features (EOF scores, one-hot PFT encoding) for
#' downstream clustering in 020_cluster_and_select_design_points.R.
#'
#' Outputs (written to data_dir):
#'   - cadwr_crops_sites.gpkg (for 010, 011, 020)
#'   - cadwr_crops_attributes.csv
#'   - cadwr_crops_site_summary.csv (for 010, 020)
#'   - cadwr_crops_features.csv (eof scores for 020)

source("000-config.R")
PEcAn.logger::logger.info("Preparing CADWR crops data")

# -- input paths --
landiq_dir <- file.path(ccmmf_dir, "LandIQ-harmonized-v4.1")
crops_parq <- file.path(landiq_dir, "crops_all_years.parq")
parcels_gpkg <- file.path(landiq_dir, "parcels.gpkg")
pft_mapping_csv <- file.path(raw_data_dir, "cadwr_land_use", "CARB_PFTs_table.csv")

for (f in c(crops_parq, parcels_gpkg, pft_mapping_csv)) {
  if (!file.exists(f)) {
    PEcAn.logger::logger.severe("Required file not found: ", f)
  }
}

# -- standardize crops --
cadwr_data <- standardize_cadwr_crops(
  input_parquet = crops_parq,
  parcels_gpkg = parcels_gpkg,
  output_dir = data_dir,
  pft_mapping_csv = pft_mapping_csv,
  write_outputs = TRUE
)

# -- summarize PFTs --
agricultural_pfts <- cadwr_data$site_summary |>
  dplyr::filter(!is.na(dominant_pft)) |>
  dplyr::pull(dominant_pft) |>
  unique()

PEcAn.logger::logger.info("PFTs found: ", paste(agricultural_pfts, collapse = ", "))

sites_ag <- cadwr_data$site_summary |>
  dplyr::filter(dominant_pft %in% agricultural_pfts)

PEcAn.logger::logger.info(
  "Agricultural fields: ", format(nrow(sites_ag), big.mark = ","),
  " of ", format(nrow(cadwr_data$site_summary), big.mark = ",")
)

# -- crop history matrix for EOF decomposition --
# season 2 = primary crop record in v4.1 (season 1 has CLASS=NA for most fields)
PEcAn.logger::logger.info("Creating crop history matrix...")

crop_history <- cadwr_data$attributes |>
  dplyr::filter(
    site_id %in% sites_ag$site_id,
    season == 2,
    !is.na(class)
  ) |>
  dplyr::slice_max(pcnt, n = 1, with_ties = FALSE, by = c(site_id, year)) |>
  dplyr::select(site_id, year, class)

years <- sort(unique(crop_history$year))
classes <- sort(unique(crop_history$class))

PEcAn.logger::logger.info(length(years), " years, ", length(classes), " crop classes")

# -- one-hot encode crop history (class x year matrix) --
PEcAn.logger::logger.info("One-hot encoding crop histories...")

# binary indicator 1 if crop class was present in that year
crop_binary_list <- lapply(years, function(yr) {
  yr_data <- crop_history |>
    dplyr::filter(year == yr) |>
    dplyr::select(site_id, class)

  if (nrow(yr_data) == 0) return(NULL)

  onehot <- model.matrix(~ class - 1, data = yr_data)
  colnames(onehot) <- paste0("y", yr, "_", gsub("class", "", colnames(onehot)))

  tibble::tibble(site_id = yr_data$site_id) |>
    dplyr::bind_cols(dplyr::as_tibble(onehot))
})

crop_binary <- purrr::reduce(crop_binary_list, dplyr::full_join, by = "site_id")
crop_binary[is.na(crop_binary)] <- 0

# need >= 3 years of data for stable EOF decomposition
sites_with_history <- crop_history |>
  dplyr::count(site_id) |>
  dplyr::filter(n >= 3) |>
  dplyr::pull(site_id)

crop_binary_filtered <- crop_binary |>
  dplyr::filter(site_id %in% sites_with_history)

PEcAn.logger::logger.info(
  format(nrow(crop_binary_filtered), big.mark = ","), " sites with 3+ years history"
)

# -- EOF decomposition (PCA on crop history) --
# continuous features capturing cropping system patterns
PEcAn.logger::logger.info("Performing EOF decomposition...")

numeric_cols <- setdiff(names(crop_binary_filtered), "site_id")
crop_matrix <- as.matrix(crop_binary_filtered[, numeric_cols])

# PCA on centered (not scaled) binary data
eof_result <- prcomp(crop_matrix, center = TRUE, scale. = FALSE)

var_explained <- eof_result$sdev^2 / sum(eof_result$sdev^2)
cumvar <- cumsum(var_explained)

# keep components explaining >= 80% variance, max 10
n_components <- min(which(cumvar >= 0.80)[1], 10, na.rm = TRUE)
if (is.na(n_components)) n_components <- min(10, ncol(eof_result$x))

PEcAn.logger::logger.info(
  n_components, " EOF components explain ",
  round(cumvar[n_components] * 100, 1), "% of variance"
)

eof_scores <- eof_result$x[, seq_len(n_components), drop = FALSE] |>
  dplyr::as_tibble() |>
  dplyr::rename_with(~ paste0("eof_", seq_along(.))) |>
  dplyr::mutate(site_id = crop_binary_filtered$site_id)

# -- PFT one-hot encoding --
PEcAn.logger::logger.info("Creating PFT one-hot covariates...")

pft_onehot <- sites_ag |>
  dplyr::select(site_id, dominant_pft) |>
  dplyr::mutate(value = 1L) |>
  tidyr::pivot_wider(
    names_from = dominant_pft,
    names_glue = "pft_{dominant_pft}",
    values_from = value,
    values_fill = 0L
  ) |>
  dplyr::rename_with(~ gsub(" ", "_", .))

# crop class by year (wide format for downstream analysis)
crop_by_year <- crop_history |>
  tidyr::pivot_wider(
    names_from = year,
    names_prefix = "class_",
    values_from = class
  )

# -- assemble site features --
PEcAn.logger::logger.info("Assembling site features...")

site_features <- sites_ag |>
  dplyr::left_join(pft_onehot, by = "site_id") |>
  dplyr::left_join(eof_scores, by = "site_id") |>
  dplyr::left_join(crop_by_year, by = "site_id") |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("eof_"), ~ tidyr::replace_na(.x, 0)))

features_csv <- file.path(data_dir, "cadwr_crops_features.csv")
readr::write_csv(site_features, features_csv)

PEcAn.logger::logger.info("Saved:", basename(features_csv))
PEcAn.logger::logger.info("CADWR crops preparation complete")
