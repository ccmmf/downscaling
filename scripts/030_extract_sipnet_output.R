# This file processess the output from SIPNET ensemble runs and generates
# three different data formats that comply with Ecological Forecasting Initiative 
# (EFI) standard:
# 1. A 4-D array (time, site, ensemble, variable)
# 2. A long format data frame (time, site, ensemble, variable)
# 3. A NetCDF file (time, site, ensemble, variable)
# This code can be moved to PEcAn.utils as one or more functions
# I did this so that I could determine which format is easiest to work with
# For now, I am planning to work with the CSV format
# TODO: write out EML metadata in order to be fully EFI compliant

## use pkg::fn syntax instead of library(pkg)
## Except furrr which loads .future.R
# library(PEcAn.logger)
# library(lubridate)
# library(dplyr)
# library(ncdf4)
library(furrr)
# library(stringr)


## First, uncompress the model output
# tar --use-compress-program="pigz -d" -xf ccmmf_phase_2a_DRAFT_output_20250516.tgz
# tar --use-compress-program="pigz -d" -xf ccmmf_phase_2a_DRAFT_output_20250516.tgz --wildcards '*.nc'

## Second, make sure ccmmf_dir and pecan_outdir are defined in the config file
source("000-config.R")

# Read settings file and extract run information
settings <- PEcAn.settings::read.settings(file.path(pecan_outdir, "pecan.CONFIGS.xml"))

ensemble_size <- settings$ensemble$size |>
    as.numeric()
start_date <- settings$run$settings.1$start.date
start_year <- lubridate::year(start_date)
end_date <- settings$run$settings.1$end.date
end_year <- lubridate::year(end_date)

# Site Information
# design points for 1b
# data/design_points.csv
design_pt_csv <- "data/design_points.csv"
design_points <- readr::read_csv(design_pt_csv) |>
    dplyr::distinct()

# Variables to extract
variables <- c("AGB", "TotSoilCarb")

site_ids <- design_points |>
    dplyr::pull(site_id) |>
    unique()
ens_ids <- 1:ensemble_size

if(!PRODUCTION) {
  ##-----TESTING SUBSET----##
  site_ids   <- site_ids[1:5]
  ens_ids    <- ens_ids[1:5]
  start_year <- end_year - 1
}

ens_dirs <- expand.grid(
    ens = PEcAn.utils::left.pad.zeros(ens_ids),
    site_id = site_ids,
    stringsAsFactors = FALSE
) |>
    dplyr::mutate(dir = as.character(file.path(model_outdir, paste("ENS", ens, site_id, sep = "-"))))

### Check that all ens dirs exist

existing_dirs <- dir.exists(ens_dirs$dir)
if (!all(existing_dirs)) {
    missing_dirs <- ens_dirs[!existing_dirs]
    PEcAn.logger::logger.warn("Missing expected ensemble directories: ", paste(missing_dirs, collapse = ", "))
} else {
    PEcAn.logger::logger.info("Found all expected ensemble directories.")
}

### Check that each ensemble directory has expected files 
years <- start_year:end_year
expected_files <- dplyr::bind_rows(
    tidyr::expand_grid(year = years, ext = c(".nc", ".nc.var")) |>
        dplyr::mutate(file = paste0(year, ext)),
    tibble::tibble(file = c("logfile.txt", "README.txt", "sipnet.out"))
) |>
    dplyr::pull(file)

check_files <- expand.grid(dir = ens_dirs$dir, file = expected_files) |>
    dplyr::mutate(full_file = file.path(dir, file)) |>
    dplyr::rowwise() |>
    dplyr::mutate( # check if file exists
        exists = file.exists(full_file)
    ) |>
    dplyr::arrange(dir, file)
if(any(!check_files$exists)) {
    missing_files <- check_files |>
        filter(!exists)

    percent_missing <- check_files |>
        dplyr::group_by(dir) |>
        dplyr::summarise(
            percent_missing = sum(!exists) / dplyr::n() * 100
        ) |>
        dplyr::mutate(
            dir = basename(as.character(dir))
        )

    PEcAn.logger::logger.warn(
        "Missing ", nrow(missing_files), "expected files\n",
        "these directories have missing files:\n"
    )
    knitr::kable(
        percent_missing  |> 
          dplyr::filter(percent_missing > 0),
        digits = 0,
        col.names = c("Directory", "Percent Missing")
    )
} else {
    PEcAn.logger::logger.info("Found all expected PEcAn output files.")
}


# extract output via PEcAn.utils::read.output
# temporarily suppress logging or else it will print a lot of file names
logger_level <- PEcAn.logger::logger.setLevel("OFF")
ens_results <- furrr::future_pmap_dfr(
    ens_dirs,
    function(ens, site_id, dir) {
        out_df <- PEcAn.utils::read.output(
            runid = paste(ens, site_id, sep = "-"),
            outdir = dir,
            start.year = start_year,
            end.year = end_year,
            variables = variables,
            dataframe = TRUE,
            verbose = FALSE
        ) |> 
        dplyr::mutate(site_id = .env$site_id, ensemble = as.numeric(.env$ens)) |>
        dplyr::rename(time = posix)
    },
    # Avoids warning "future unexpectedly generated random numbers",
    # Fixed in units v 0.9.0 
    #.Bug report: https://github.com/r-quantities/units/issues/409
    .options = furrr::furrr_options(seed = TRUE)
) |>
    dplyr::group_by(ensemble, site_id, year) |>
    #filter(year <= end_year) |> # not sure why this was necessary; should be taken care of by read.output
    dplyr::filter(time == max(time)) |> # only take last value
    dplyr::ungroup() |>
    dplyr::arrange(ensemble, site_id, year)  |> 
    tidyr::pivot_longer(cols = all_of(variables), names_to = "variable", values_to = "prediction")

# After extraction, ens_results$prediction is in kg C m-2 for both AGB and TotSoilCarb

# restore logging
logger_level <- PEcAn.logger::logger.setLevel(logger_level)

## Create Ensemble Output For Downscaling
## Below, three different output formats are created:
## 1. 4-D array (time, site, ensemble, variable)
## 2. long format data frame (time, site, ensemble, variable)
## 3. NetCDF file (time, site, ensemble, variable)

# --- 1. Create 4-D array ---
# Add a time dimension (even if of length 1) so that dimensions are: [time, site, ensemble, variable]
unique_times <- sort(unique(ens_results$time))
if(length(unique_times) != length(start_year:end_year)){
    # --> if code above including group_by(.., year) is changed,
    # this check may fail if we are using > one time point per year
    PEcAn.logger::logger.warn( 
        "there should only be one unique time per year",
        "unless we are doing a time series with multiple time points per year"
    )
}

# Create a list to hold one 3-D array per variable
ens_arrays <- list()
for (var in variables) {
    # Preallocate 3-D array for time, site, ensemble for each variable
    arr <- array(NA,
        dim = c(length(unique_times), length(site_ids), length(ens_ids)),
        dimnames = list(
            datetime = as.character(unique_times),
            site_id = site_ids,
            ensemble = as.character(ens_ids)
        )
    )
    
    # Get rows corresponding to the current variable
    subset_idx <- which(ens_results$variable == var)
    if (length(subset_idx) > 0) {
        i_time <- match(ens_results$time[subset_idx], unique_times)
        i_site <- match(ens_results$site_id[subset_idx], site_ids)
        i_ens <- match(ens_results$ensemble[subset_idx], ens_ids)
        arr[cbind(i_time, i_site, i_ens)] <- ens_results$prediction[subset_idx]
    }
    
    ens_arrays[[var]] <- arr
}

# ens_arrays: three dimensions [time, site, ensemble]
saveRDS(ens_arrays, file = file.path(cache_dir, "ensemble_output.rds"))
