#' Prepare CADWR Crops Data for Downscaling
#'
#' Converts harmonized crops_all_years.csv to standardized format and creates
#' temporal features (EOF scores, one-hot encoding) for downstream clustering.
#'
#' Outputs:
#'   - cadwr_crops_sites.gpkg (for 010_prepare_covariates.R)
#'   - cadwr_crops_attributes.csv
#'   - cadwr_crops_site_summary.csv (for 010_prepare_covariates.R)
#'   - cadwr_crops_features.csv (with EOF scores for 020_cluster_and_select_design_points.R)
#'

source("000-config.R")
PEcAn.logger::logger.info("*** Preparing CADWR Crops Data ***")

# Input files
crops_csv <- file.path(raw_data_dir, "cadwr_land_use", "crops_all_years.csv")
pft_mapping_csv <- file.path(raw_data_dir, "cadwr_land_use", "CARB_PFTs_table.csv")

if (!file.exists(crops_csv)) {
  PEcAn.logger::logger.severe("File not found: ", crops_csv)
}

cadwr_data <- standardize_cadwr_crops(
  input_csv = crops_csv,
  output_dir = data_dir,
  pft_mapping_csv = pft_mapping_csv,
  write_outputs = TRUE
)

# Get PFTs from the data
agricultural_pfts <- cadwr_data$site_summary |>
  dplyr::filter(!is.na(dominant_pft)) |>
  dplyr::pull(dominant_pft) |>
  unique()

PEcAn.logger::logger.info("PFTs found: ", paste(agricultural_pfts, collapse = ", "))

# Filter to agricultural sites
sites_ag <- cadwr_data$site_summary |>
  dplyr::filter(dominant_pft %in% agricultural_pfts)

n_ag <- nrow(sites_ag)
n_total <- nrow(cadwr_data$site_summary)

PEcAn.logger::logger.info("Agricultural fields: ", n_ag, " of ", n_total)


# Create crop history for EOF decomposition
PEcAn.logger::logger.info("Creating crop history matrix...")

# Extract crop class by year (season 2 = main growing season)
crop_history <- cadwr_data$attributes |>
  dplyr::filter(site_id %in% sites_ag$site_id, season == 2, !is.na(class)) |>
  dplyr::group_by(site_id, year) |>
  dplyr::slice_max(pcnt, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(site_id, year, class)

years <- sort(unique(crop_history$year))
classes <- sort(unique(crop_history$class))

PEcAn.logger::logger.info(length(years), " years, ", length(classes), " crop classes")


# One-hot encode crop history (class * year matrix)
PEcAn.logger::logger.info("One-hot encoding crop histories...")

# Create binary matrix -> 1 if crop class present in that year
crop_binary_list <- lapply(years, function(yr) {
  yr_data <- crop_history |>
    dplyr::filter(year == yr) |>
    dplyr::select(site_id, class)
  
  if (nrow(yr_data) == 0) return(NULL)
  
  # One-hot encode
  onehot <- model.matrix(~ class - 1, data = yr_data)
  colnames(onehot) <- paste0("y", yr, "_", gsub("class", "", colnames(onehot)))
  
  tibble::tibble(site_id = yr_data$site_id) |>
    dplyr::bind_cols(dplyr::as_tibble(onehot))
})

# Merge all years
crop_binary <- purrr::reduce(crop_binary_list, dplyr::full_join, by = "site_id")
crop_binary[is.na(crop_binary)] <- 0

# Keep only sites with at least 3 years of data for stable EOF
sites_with_history <- crop_history |>
  dplyr::count(site_id) |>
  dplyr::filter(n >= 3) |>
  dplyr::pull(site_id)

crop_binary_filtered <- crop_binary |>
  dplyr::filter(site_id %in% sites_with_history)

PEcAn.logger::logger.info(nrow(crop_binary_filtered), " sites with 3+ years history")

# EOF decomposition (PCA on crop history)
# This creates continuous features that capture cropping system patterns
# These features will be used by 020_cluster_and_select_design_points.R for site selection
PEcAn.logger::logger.info("Performing EOF decomposition...")

numeric_cols <- setdiff(names(crop_binary_filtered), "site_id")
crop_matrix <- as.matrix(crop_binary_filtered[, numeric_cols])

# PCA on centered (not scaled) binary data
eof_result <- prcomp(crop_matrix, center = TRUE, scale. = FALSE)

# Variance explained
var_explained <- eof_result$sdev^2 / sum(eof_result$sdev^2)
cumvar <- cumsum(var_explained)

# Keep components explaining 80% variance, max 10
n_components <- min(which(cumvar >= 0.80)[1], 10, na.rm = TRUE)
if (is.na(n_components)) n_components <- min(10, ncol(eof_result$x))

PEcAn.logger::logger.info(
  "  ", n_components, " EOF components explain ",
  round(cumvar[n_components] * 100, 1), "% of variance"
)

# Extract EOF scores
eof_scores <- eof_result$x[, 1:n_components, drop = FALSE] |>
  dplyr::as_tibble() |>
  dplyr::rename_with(~ paste0("eof_", seq_along(.))) |>
  dplyr::mutate(site_id = crop_binary_filtered$site_id)


# One-hot encoding for PFT
PEcAn.logger::logger.info("Creating one-hot encoded PFT covariates...")

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

# Create wide-format crop class by year
# For downstream analysis (what crop was grown each year)
crop_by_year <- crop_history |>
  tidyr::pivot_wider(
    names_from = year,
    names_prefix = "class_",
    values_from = class
  )

PEcAn.logger::logger.info("Assembling site features...")

site_features <- sites_ag |>
  dplyr::left_join(pft_onehot, by = "site_id") |>
  dplyr::left_join(eof_scores, by = "site_id") |>
  dplyr::left_join(crop_by_year, by = "site_id")

# Fill NA EOF scores with 0 for sites without enough history
site_features <- site_features |>
  dplyr::mutate(dplyr::across(starts_with("eof_"), ~ tidyr::replace_na(.x, 0)))

features_csv <- file.path(data_dir, "cadwr_crops_features.csv")
readr::write_csv(site_features, features_csv)

PEcAn.logger::logger.info("Saved: ", basename(features_csv))
PEcAn.logger::logger.info("*** Complete ***")