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

#----------Management scenario extraction-------------------------------
# TODO: extend to support multi-PFT scenarios when woody crop management scenarios are added
if (exists("USE_PHASE_3_SCENARIOS") && USE_PHASE_3_SCENARIOS) {
    PEcAn.logger::logger.info("Loading pre-extracted scenario data")
    
    rdata_files <- list.files(
        pecan_outdir,
        pattern = "^results_monthly_.*\\.Rdata$",
        full.names = TRUE
    )
    
    if (length(rdata_files) == 0) {
        PEcAn.logger::logger.severe("No pre-extracted .Rdata files found in ", pecan_outdir)
    }
    
    # load site info for lat/lon
    site_info <- readr::read_csv(file.path(pecan_outdir, "site_info.csv"))
    
    # process each variable file
    all_results <- list()
    for (rdata_file in rdata_files) {
        var_name <- stringr::str_extract(basename(rdata_file), "(?<=results_monthly_)[^.]+")
        
        if (!var_name %in% outputs_to_extract) {
            PEcAn.logger::logger.info("Skipping ", var_name, " (not in outputs_to_extract)")
            next
        }
        
        PEcAn.logger::logger.info("Loading ", var_name, " from ", basename(rdata_file))
        load(rdata_file)  # loads 'results' dataframe
        
        # convert to EFI format
        var_results <- results |>
            dplyr::rename(
                site_id = site,
                parameter = ens_num,
                datetime = posix
            ) |>
            dplyr::left_join(
                site_info |> dplyr::select(id, lat, lon),
                by = c("site_id" = "id")
            ) |>
            tidyr::pivot_longer(
                cols = tidyr::all_of(var_name),
                names_to = "variable",
                values_to = "prediction"
            ) |>
            dplyr::mutate(
                pft = "annual crop",
                datetime = lubridate::floor_date(datetime, unit = "month"),
                variable_type = "pool"
            ) |>
            dplyr::group_by(scenario, datetime, site_id, lat, lon, pft, parameter, variable, variable_type) |>
            dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop")
        
        all_results[[var_name]] <- var_results
    }
    
    ens_results <- dplyr::bind_rows(all_results)
    
    # write output
    ensemble_output_csv <- file.path(model_outdir, "ensemble_output.csv")
    readr::write_csv(ens_results, ensemble_output_csv)
    PEcAn.logger::logger.info(
        "\nPhase 3 extraction complete.",
        "\nScenarios: ", paste(unique(ens_results$scenario), collapse = ", "),
        "\nSites: ", dplyr::n_distinct(ens_results$site_id),
        "\nEnsembles: ", dplyr::n_distinct(ens_results$parameter),
        "\nVariables: ", paste(unique(ens_results$variable), collapse = ", "),
        "\nSaved to: ", ensemble_output_csv
    )
    
} else {

    #---------- netCDF extraction -------------------------------
    
    # check for merged netCDF files
    merged_nc_dir <- file.path(model_outdir, "merged_nc")
    use_merged_nc <- dir.exists(merged_nc_dir) && 
                     length(list.files(merged_nc_dir, pattern = "^\\d{4}\\.nc$")) > 0
    
    if (use_merged_nc) {
        PEcAn.logger::logger.info("Found merged netCDF files in: ", merged_nc_dir)
        
        # list yearly NC files
        nc_files <- list.files(merged_nc_dir, pattern = "^\\d{4}\\.nc$", full.names = TRUE)
        years <- as.numeric(tools::file_path_sans_ext(basename(nc_files)))
        
        # open first file to get metadata
        nc_sample <- ncdf4::nc_open(nc_files[1])
        site_ids_nc <- tryCatch(
            ncdf4::ncvar_get(nc_sample, "site_id"),
            error = function(e) as.character(nc_sample$dim$site$vals)
        )
        ensemble_size <- nc_sample$dim$ensemble$len
        available_vars <- names(nc_sample$var)
        ncdf4::nc_close(nc_sample)
        
        PEcAn.logger::logger.info(
            "Merged NC metadata: ", length(site_ids_nc), " sites, ",
            ensemble_size, " ensembles, ", length(years), " years"
        )
        
        # load design points for lat/lon
        design_points_csv <- "https://raw.githubusercontent.com/ccmmf/workflows/refs/tags/v0.2.0/data/design_points.csv"
        design_points <- readr::read_csv(design_points_csv)
        
        # build site metadata
        site_ids <- site_ids_nc
        site_meta <- tibble::tibble(
            site_id = site_ids,
            base_site_id = sub("_grass$", "", site_ids),
            pft = dplyr::if_else(
                grepl("_grass$", site_ids),
                "annual crop",
                "woody perennial crop"
            )
        ) |>
            dplyr::left_join(
                design_points |> dplyr::select(site_id, lat, lon),
                by = c("base_site_id" = "site_id")
            )
        
        if (!PRODUCTION) {
            site_ids <- site_ids[1:10]
            ens_ids <- 1:min(10, ensemble_size)
            nc_files <- nc_files[years >= max(years) - 1]
        } else {
            ens_ids <- 1:ensemble_size
        }
        
        # extract variables from merged files
        variables <- intersect(outputs_to_extract, available_vars)
        if (length(variables) == 0) {
            PEcAn.logger::logger.severe(
                "None of requested variables found in merged NC. ",
                "Requested: ", paste(outputs_to_extract, collapse = ", "),
                "Available: ", paste(available_vars, collapse = ", ")
            )
        }
        
        logger_level <- PEcAn.logger::logger.setLevel("OFF")
        
        ens_results_raw <- purrr::map_dfr(nc_files, function(nc_path) {
            nc <- ncdf4::nc_open(nc_path)
            
            time_vals <- nc$dim$time$vals
            time_units <- nc$dim$time$units
            origin <- strsplit(time_units, "since ")[[1]][2]
            datetimes <- as.POSIXct(time_vals * 3600 * 24, origin = origin, tz = "UTC")
            
            # extract each variable
            var_results <- purrr::map_dfr(variables, function(var) {
                # nc dimensions are [site, ensemble, time]
                var_data <- ncdf4::ncvar_get(nc, var)
                
                # get indices for requested sites/ensembles
                site_indices <- which(site_ids_nc %in% site_ids)
                
                # build result dataframe
                tidyr::expand_grid(
                    site_idx = site_indices,
                    ens = ens_ids,
                    time_idx = seq_along(datetimes)
                ) |>
                    dplyr::mutate(
                        site_id = site_ids_nc[site_idx],
                        datetime = datetimes[time_idx],
                        parameter = ens,
                        variable = var,
                        prediction = purrr::pmap_dbl(
                            list(site_idx, ens, time_idx),
                            function(s, e, t) var_data[s, e, t]
                        )
                    ) |>
                    dplyr::select(datetime, site_id, parameter, variable, prediction)
            })
            
            ncdf4::nc_close(nc)
            var_results
        })
        
        PEcAn.logger::logger.setLevel(logger_level)
        
        # Add metadata and aggregate to monthly
        ens_results <- ens_results_raw |>
            dplyr::left_join(
                site_meta |> dplyr::select(site_id, lat, lon, pft),
                by = "site_id"
            ) |>
            dplyr::mutate(scenario = "baseline")
        
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
            dplyr::group_by(scenario, datetime, site_id, lat, lon, pft, parameter, variable, variable_type) |>
            dplyr::summarise(prediction = mean(prediction, na.rm = TRUE), .groups = "drop")
        
        if (any(ens_results$variable_type == "flux")) {
            PEcAn.logger::logger.warn(
                "Flux variables detected. Averaging fluxes over time can be misleading."
            )
        }
        
        ensemble_output_csv <- file.path(model_outdir, "ensemble_output.csv")
        readr::write_csv(ens_results, ensemble_output_csv)
        PEcAn.logger::logger.info(
            "\nMerged NC extraction complete.",
            "\nResults saved to ", ensemble_output_csv
        )
        
    } else {
        #---------- individual file extraction ----------
        
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
            dplyr::mutate(scenario = "baseline") |>  # Add scenario column for consistency
            dplyr::select(scenario, datetime, site_id, lat, lon, pft, parameter, variable, prediction)
        
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
            dplyr::group_by(scenario, datetime, site_id, lat, lon, pft, parameter, variable, variable_type) |>
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
    }
}