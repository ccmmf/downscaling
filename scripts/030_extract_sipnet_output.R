# Read SIPNET ensemble output for every management scenario, glue the
# scenarios together, and write a long-format CSV (time, site, ensemble,
# variable) that follows the Ecological Forecasting Initiative (EFI)
# standard.
#
# Helpers in R/efi_long_to_arrays.R turn this CSV into:
#   1. a 4-D array (time, site, ensemble, variable)
#   2. a NetCDF file (time, site, ensemble, variable)
#
# TODO: write out EML metadata so we are fully EFI compliant
# TODO: extend to multi-PFT scenarios once woody crop runs are added

source("000-config.R")
PEcAn.logger::logger.info("***Starting SIPNET output extraction***")

# Site lat/lon for all 100 annual_crop sites
site_info <- readr::read_csv(file.path(pecan_outdir, "site_info.csv"))

variables <- outputs_to_extract

# Tag variables as pool (state) or flux (rate) from PEcAn's standard_vars
# table. Done once up front so every scenario reuses the same lookup.
std_vars <- PEcAn.utils::standard_vars

pool_vars <- std_vars |>
    dplyr::filter(stringr::str_detect(tolower(Category), "pool")) |>
    dplyr::pull(Variable.Name)

flux_vars <- std_vars |>
    dplyr::filter(stringr::str_detect(tolower(Category), "flux")) |>
    dplyr::pull(Variable.Name)

all_scenario_results <- list()

for (scenario in management_scenarios) {
    PEcAn.logger::logger.info("Processing scenario: ", scenario)

    scenario_outdir <- file.path(pecan_outdir, paste0("output_", scenario))
    scenario_model_outdir <- file.path(scenario_outdir, "out")

    # Pull the run window straight from the scenario's CONFIGS file
    settings <- PEcAn.settings::read.settings(
        file.path(scenario_outdir, "pecan.CONFIGS.xml")
    )
    first_run <- settings$run[[1]]
    start_year <- lubridate::year(first_run$start.date)
    end_year <- lubridate::year(first_run$end.date)

    # runs.txt holds the full <prefix>-<ens>-<site_id> directory list
    # (e.g. ENS-00001-8773c58306e8d9a0)
    runs_file <- file.path(scenario_outdir, "run", "runs.txt")
    ens_dirs <- tibble::tibble(
        dir = readr::read_lines(runs_file)
    ) |>
        tidyr::separate(
            col = "dir",
            into = c("prefix", "ens", "site_id"),
            sep = "-",
            remove = FALSE
        ) |>
        dplyr::select(-prefix)

    ensemble_size <- dplyr::n_distinct(ens_dirs$ens)

    # Catch half-finished runs early
    actual_ens_dirs <- dir(scenario_model_outdir)
    if (!all(ens_dirs$dir %in% actual_ens_dirs)) {
        missing_dirs <- setdiff(ens_dirs$dir, actual_ens_dirs)
        PEcAn.logger::logger.severe(
            "Missing ensemble directories in ", scenario_model_outdir, ": ",
            paste(missing_dirs, collapse = ", ")
        )
    }

    # Attach lat/lon to each site_id
    site_meta <- ens_dirs |>
        dplyr::distinct(site_id) |>
        dplyr::left_join(
            site_info |> dplyr::select(id, lat, lon),
            by = c("site_id" = "id")
        )

    site_ids <- unique(site_meta$site_id)
    ens_ids <- 1:ensemble_size

    if (!PRODUCTION) {
        # Dev mode: trim to a small slice so iterations stay quick
        site_ids <- site_ids[1:10]
        ens_ids <- ens_ids[1:10]
        start_year <- end_year - 1
    }

    ens_dirs_subset <- ens_dirs |>
        dplyr::filter(
            site_id %in% site_ids,
            as.numeric(ens) %in% ens_ids
        ) |>
        dplyr::mutate(dir = file.path(scenario_model_outdir, dir))

    # PEcAn.utils::read.output prints one log line per file. Mute the
    # logger for the duration of the read, then restore it.
    logger_level <- PEcAn.logger::logger.setLevel("OFF")
    ens_results_raw <- furrr::future_pmap_dfr(
        ens_dirs_subset,
        function(dir, ens, site_id) {
            PEcAn.utils::read.output(
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
                    parameter = as.numeric(.env$ens)
                ) |>
                dplyr::rename(time = posix)
        },
        .options = furrr::furrr_options(seed = TRUE)
    )
    PEcAn.logger::logger.setLevel(logger_level)

    ens_results <- ens_results_raw |>
        dplyr::arrange(parameter, site_id, year) |>
        tidyr::pivot_longer(
            cols = tidyr::all_of(variables),
            names_to = "variable",
            values_to = "prediction"
        ) |>
        dplyr::rename(datetime = time) |>
        dplyr::left_join(site_meta, by = "site_id") |>
        dplyr::mutate(
            scenario = .env$scenario,
            pft = "annual crop"
        )
    rm(ens_results_raw)

    # Roll up to monthly inside the loop; holding raw sub-monthly data
    # for every scenario blows the memory budget.
    n_raw <- nrow(ens_results)
    ens_results <- ens_results |>
        dplyr::mutate(
            datetime = lubridate::floor_date(datetime, unit = "month"),
            variable_type = dplyr::case_when(
                variable %in% pool_vars ~ "pool",
                variable %in% flux_vars ~ "flux",
                TRUE ~ "unknown"
            )
        ) |>
        dplyr::summarise(
            prediction = mean(prediction, na.rm = TRUE),
            .by = c("scenario", "datetime", "site_id", "lat", "lon", "pft",
                    "parameter", "variable", "variable_type")
        ) |>
        dplyr::select(scenario, datetime, site_id, lat, lon, pft,
                      parameter, variable, variable_type, prediction)

    all_scenario_results[[scenario]] <- ens_results
    PEcAn.logger::logger.info(
        "Scenario '", scenario, "' done: ",
        n_raw, " raw rows -> ", nrow(ens_results), " monthly rows"
    )
}

ens_results <- dplyr::bind_rows(all_scenario_results)
rm(all_scenario_results)

# Note: monthly means of instantaneous flux rates (kg m-2 s-1) are valid
# mean rates. Downstream scripts handle the rate -> reporting-unit
# conversion (e.g. kg ha-1 yr-1).
if (any(ens_results$variable_type == "flux")) {
    PEcAn.logger::logger.info(
        "Flux variables present; downstream scripts handle the ",
        "rate -> reporting-unit conversion."
    )
}

ensemble_output_csv <- file.path(pecan_outdir, "ensemble_output.csv")
readr::write_csv(ens_results, ensemble_output_csv)
PEcAn.logger::logger.info(
    "Extraction complete. ",
    length(management_scenarios), " scenarios, ",
    nrow(ens_results), " total rows. ",
    "Saved to ", ensemble_output_csv
)
