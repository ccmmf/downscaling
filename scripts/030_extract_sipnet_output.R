# This file processess the output from SIPNET ensemble runs and generates
# A long format CSV (time, site, ensemble, variable) 
# that follows the Ecological Forecasting Initiative (EFI) forecast standard

# Helper functions in R/efi_long_to_arrays.R will convert this to
# 1. A 4-D array (time, site, ensemble, variable)
# 2. A NetCDF file (time, site, ensemble, variable)
# TODO: write out EML metadata in order to be fully EFI compliant


## First, uncompress the model output
# tar --use-compress-program="pigz -d" -xf ccmmf_phase_2a_DRAFT_output_20250516.tgz
# tar --use-compress-program="pigz -d" -xf ccmmf_phase_2a_DRAFT_output_20250516.tgz --wildcards '*.nc'

## Second, make sure ccmmf_dir and pecan_outdir are defined in the config file
source("000-config.R")
PEcAn.logger::logger.info("***Starting SIPNET output extraction***")
# Read settings file and extract run information
settings <- PEcAn.settings::read.settings(file.path(pecan_outdir, "pecan.CONFIGS.xml"))

ensemble_size <- settings$ensemble$size |>
    as.numeric()
start_date <- settings$run$settings.1$start.date
start_year <- lubridate::year(start_date)
end_date <- settings$run$settings.1$end.date
end_year <- lubridate::year(end_date)


design_points_csv <- "https://raw.githubusercontent.com/ccmmf/workflows/refs/tags/v0.2.0/data/design_points.csv"
design_points <- readr::read_csv(design_points_csv)

# Read the runs.txt file to get ensemble and site and directory information
runs_file <- file.path(pecan_outdir, "run", "runs.txt")
## Expected outputs based on runs.txt
expected_ens_dirs <- tibble::tibble(
    dir = readr::read_lines(runs_file)
) |>
    tidyr::separate(
        col = "dir",
        into = c("prefix", "ens", "site_id"),
        sep = "-",
        remove = FALSE
    ) |>
    dplyr::mutate(
        # base ID has no suffix; used to match design_points
        base_site_id = sub("_grass$", "", site_id),
        # PFT determined by presence of the suffix in the actual site_id
        pft = dplyr::if_else(
            grepl("_grass$", site_id),
            "annual crop",
            "woody perennial crop"
        )
    ) |>
    dplyr::select(-prefix)

## Check that all expected are present
actual_ens_dirs <- dir(model_outdir)
dirs_present <- all(expected_ens_dirs$dir %in% actual_ens_dirs)
if (!dirs_present) {
    missing_dirs <- setdiff(expected_ens_dirs$dir, actual_ens_dirs)
    PEcAn.logger::logger.severe(
        "Not all expected ensemble directories are present in the model output directory.",
        "Missing: ", paste(missing_dirs, collapse = ", ")
    )
}
ens_dirs <- expected_ens_dirs

# Map each actual site_id (with/without suffix) to lat/lon via base_site_id
site_meta <- ens_dirs |>
    dplyr::distinct(site_id, base_site_id, pft) |>
    dplyr::left_join(
        design_points |> dplyr::select(site_id, lat, lon),
        by = dplyr::join_by(base_site_id == site_id)
    )

site_ids <- site_meta |>
    dplyr::pull(site_id) |>
    unique()

ens_ids <- 1:ensemble_size

variables <- outputs_to_extract # TODO standardize this name; variables is ambiguous
                                # but is used by the PEcAn read.output function

if (!PRODUCTION) {
    ## -----TESTING SUBSET----##
    site_ids <- site_ids[1:10]
    ens_ids <- ens_ids[1:10]
    start_year <- end_year - 1
}

## Create dataframe of directories to process, filtered to include only
##   existing directories (ens_dirs) that have site_ids and ensemble IDs
ens_ids_str <- PEcAn.utils::left.pad.zeros(ens_ids)
dirs_to_process <- ens_dirs |>
    dplyr::filter(
        site_id %in% site_ids,
        ens %in% ens_ids_str
    )

ens_dirs_subset <- ens_dirs |>
    dplyr::filter(
        base_site_id %in% site_ids, # use base_site_id (site_id may include _grass)
        as.numeric(ens) %in% ens_ids
    ) |>
    dplyr::mutate(dir = file.path(model_outdir, dir)) |>
    dplyr::select(ens, site_id, dir, pft, base_site_id) # keep both ids

# extract output via PEcAn.utils::read.output
# temporarily suppress logging or else it will print a lot of file names
logger_level <- PEcAn.logger::logger.setLevel("OFF")
ens_results_raw <- furrr::future_pmap_dfr(
    ens_dirs_subset,
    function(ens, site_id, dir, pft, base_site_id) {
        out_df <- PEcAn.utils::read.output(
            runid = paste(ens, site_id, sep = "-"),
            outdir = dir,
            start.year = start_year,
            end.year = end_year,
            variables = variables,
            dataframe = TRUE,
            verbose = FALSE
        ) |>
            dplyr::mutate(
                site_id = .env$site_id,
                parameter = as.numeric(.env$ens),
                pft = .env$pft,
                base_site_id = .env$base_site_id
            ) |>
            dplyr::rename(time = posix)
    },
    # Avoids warning "future unexpectedly generated random numbers",
    # .Bug report: https://github.com/r-quantities/units/issues/409
    # Fixed in units version >= 0.8.7
    .options = furrr::furrr_options(seed = TRUE)
) 

ens_results <- ens_results_raw |>
    dplyr::group_by(parameter, base_site_id, pft, year) |>
    dplyr::ungroup() |>
    dplyr::arrange(parameter, base_site_id, pft, year) |>
    tidyr::pivot_longer(
        cols = tidyr::all_of(variables),
        names_to = "variable",
        values_to = "prediction"
    ) |>
    dplyr::rename(datetime = time) |>
    dplyr::left_join(site_meta,
        by = c("site_id", "base_site_id", "pft")
    ) |>
    dplyr::select(-site_id) |>
    dplyr::rename(site_id = base_site_id) |>
    dplyr::select(datetime, site_id, lat, lon, pft, parameter, variable, prediction)

# Classify variables as 'pool' (state) vs 'flux' (rate) using PEcAn standard_vars.
std_vars <- PEcAn.utils::standard_vars

pool_vars <- std_vars |>
        dplyr::filter(stringr::str_detect(tolower(Category), "pool")) |>
        dplyr::pull(Variable.Name) 

flux_vars <- std_vars |>
        dplyr::filter(stringr::str_detect(tolower(Category), "flux")) |>
        dplyr::pull(Variable.Name)

ens_results <- ens_results |>
    dplyr::mutate(
        datetime = lubridate::floor_date(datetime, unit = "month"),
        variable_type = dplyr::case_when(
            variable %in% pool_vars ~ "pool",
            variable %in% flux_vars ~ "flux",
            TRUE ~ "unknown"
        )
    ) |>
    dplyr::group_by(datetime, site_id, lat, lon, pft, parameter, variable, variable_type) |>
    dplyr::summarise(
        prediction = mean(prediction, na.rm = TRUE),
        .groups = "drop"
    )

# Warn if flux variables are present because users may need to treat them differently.
if (any(ens_results$variable_type == "flux")) {
    PEcAn.logger::logger.severe(
        "Flux variables detected in ensemble output. Note: averaging flux (rate) variables",
        "across ensembles/sites or over time can be misleading. Consider computing cumulative",
        "fluxes over simulation period",
    )
}

# After extraction, ens_results$prediction is in kg C m-2 for both AGB and TotSoilCarb

# restore logging
logger_level <- PEcAn.logger::logger.setLevel(logger_level)
 
ensemble_output_csv <- file.path(model_outdir, "ensemble_output.csv")
readr::write_csv(ens_results, ensemble_output_csv)
PEcAn.logger::logger.info(
    "\nEnsemble output extraction complete.",
    "\nResults saved to ", ensemble_output_csv
)
PEcAn.logger::logger.setLevel(logger_level)
