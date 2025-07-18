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
# TODO turn this into a function nc_to_efi_long 
#      that returns a data frame and optionally writes to a file

## Except furrr which loads .future.R
library(furrr)

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

## Running into issues with the design points
## TODO Fix on next run
# Read the runs.txt file to get ensemble and site and directory information
actual_ens_dirs <- tibble::tibble(dir = readr::read_lines(
    file.path(pecan_outdir, "run", "runs.txt")
)) |>
    tidyr::separate(
        col = "dir",
        into = c("prefix", "ens", "site_id"),
        sep = "-",
        remove = FALSE
    ) |>
    dplyr::select(-prefix)

implied_design_points <- actual_ens_dirs |>
    dplyr::select(site_id) |>
    dplyr::distinct()

field_attributes_csv <- file.path(data_dir, "ca_field_attributes.csv")
field_attributes <- readr::read_csv(field_attributes_csv) |>
    dplyr::select(site_id, lat, lon, pft)

implied_design_points_lat_lon <- implied_design_points |>
    dplyr::left_join(
        field_attributes,
        by = "site_id"
    )
### TODO: Design points should be from clustering step
### But ... what actually matters is where the model was run
### See tests / assertions at bottom of this script
design_pt_csv <- file.path(data_dir, "design_points.csv")
readr::write_csv(
    implied_design_points_lat_lon,
    design_pt_csv
)

design_points <- readr::read_csv(design_pt_csv) |>
    dplyr::filter(pft == "annual crop")

site_ids <- design_points |>
    dplyr::pull(site_id) |>
    unique()

ens_ids <- 1:ensemble_size

variables <- outputs_to_extract # TODO standardize this name; variables is ambiguous
                                # but lots of downstream code uses it

if (!PRODUCTION) {
    ## -----TESTING SUBSET----##
    site_ids <- site_ids[1:10]
    ens_ids <- ens_ids[1:10]
    start_year <- end_year - 1
    variables <- variables[1]
}

expected_out_dirs <- expand.grid(
    ens = PEcAn.utils::left.pad.zeros(ens_ids),
    site_id = site_ids,
    stringsAsFactors = FALSE
) |>
    dplyr::mutate(dir = file.path(model_outdir, paste("ENS", ens, site_id, sep = "-")))

# extract output via PEcAn.utils::read.output
# temporarily suppress logging or else it will print a lot of file names
logger_level <- PEcAn.logger::logger.setLevel("OFF")
ens_results <- furrr::future_pmap_dfr(
    expected_out_dirs,
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
            dplyr::mutate(
                site_id = .env$site_id,
                parameter = as.numeric(.env$ens)
            ) |>
            dplyr::rename(time = posix)
    },
    # Avoids warning "future unexpectedly generated random numbers",
    # Fixed in units v 0.9.0
    # .Bug report: https://github.com/r-quantities/units/issues/409
    .options = furrr::furrr_options(seed = TRUE)
) |>
    dplyr::group_by(parameter, site_id, year) |>
    # filter(year <= end_year) |> # not sure why this was necessary;
    # should be taken care of by read.output
    dplyr::filter(time == max(time)) |> # only take last value
    dplyr::ungroup() |>
    dplyr::arrange(parameter, site_id, year) |>
    tidyr::pivot_longer(
        cols = all_of(variables),
        names_to = "variable",
        values_to = "prediction"
    ) |>
    dplyr::rename(datetime = time) |>
    dplyr::left_join(design_points, by = "site_id") |> 
    dplyr::select(datetime, site_id, lat, lon, pft, parameter, variable, prediction)


# After extraction, ens_results$prediction is in kg C m-2 for both AGB and TotSoilCarb

# restore logging
logger_level <- PEcAn.logger::logger.setLevel(logger_level)
 
ensemble_output_csv <- file.path(model_outdir, "ensemble_output.csv")
readr::write_csv(ens_results, ensemble_output_csv)
PEcAn.logger::logger.info(
    "\nEnsemble output extraction complete.",
    "\nResults saved to ", ensemble_output_csv
)



####---------DIRECTORY AND FILE CHECKS---------####
#### TODO: TURNED INTO A FUNCTION, Convert to PEcAn tests, or otherwise 
####       THEY SHOULDN"T BE REQUIRED IN THIS SCRIPT
####       IF IT SOUNDS LIKE I AM YELLING, YES THIS IS FRUSTRATING TO KEEP TRACK OF!
### Check that all expected ensemble directories are present
### Some of these tests like identical(runs.txt, actual_dirs) could be done
### when files are written out
# check_dirs_and_files <- FALSE
# if (check_dirs_and_files) {
#     # directories based on ensemble and site_id
#     ens_run_dirs <- sort(ens_run_dirs$dir)
#     expected_dirs <- sort(expected_out_dirs$dir)
#     actual_dirs <- sort(dir(model_outdir, pattern = "^ENS-\\d+-"))

#     ## Check that runs.txt matches the expected ensemble directories
#     if (identical(ens_run_dirs, actual_dirs)) {
#         PEcAn.logger::logger.info("Success: Exact match between runs.txt and output directories")
#     } else {
#         PEcAn.logger::logger.warn(
#             "Mismatch between output directories and in runs.txt.\n"
#             # next time this messes up, add more informative diagnostics, like what is missing
#         )
#     }
#     if (PRODUCTION) {
#         if (identical(expected_dirs, actual_dirs)) {
#             PEcAn.logger::logger.info("Success: model output directory matches expanded based on ensemble x site_id")
#         } else {
#             PEcAn.logger::logger.warn(
#                 "Mismatch between expected ensemble directories and actual directories."
#             )
#         }
#     } else {
#         # expect that expected_dirs is a subset of actual_dirs
#         if (all(expected_dirs %in% model_runs)) {
#             PEcAn.logger::logger.info("Success: All expected ensemble directories found in model output directory")
#         } else {
#             PEcAn.logger::logger.warn(
#                 "Some expected ensemble directories are missing from model output directory."
#             )
#         }
#     }

#     if (!all(actual_dirs)) {
#         # identify missing basenames and reconstruct full paths for warning
#         missing_basenames <- expected_basenames[!actual_dirs]
#         missing_dirs <- file.path(model_outdir, missing_basenames)
#         PEcAn.logger::logger.warn(
#             "Missing ", length(missing_dirs), " of ", nrow(expected_dirs),
#             " expected ensemble directories:\n  ",
#             paste(missing_basenames, collapse = ", ")
#         )
#     } else {
#         PEcAn.logger::logger.info("Success: Found all expected ensemble directories.")
#     }

#     ### Check that each ensemble directory has expected files
#     years <- start_year:end_year
#     expected_files <- dplyr::bind_rows(
#         tidyr::expand_grid(year = years, ext = c(".nc", ".nc.var")) |>
#             dplyr::mutate(file = paste0(year, ext)),
#         tibble::tibble(file = c("logfile.txt", "README.txt", "sipnet.out"))
#     ) |>
#         dplyr::pull(file)

#     check_files <- expand.grid(dir = actual_dirs, file = expected_files) |>
#         dplyr::mutate(full_file = file.path(dir, file)) |>
#         dplyr::rowwise() |>
#         dplyr::mutate( # check if file exists
#             exists = file.exists(full_file)
#         ) |>
#         dplyr::arrange(dir, file)

#     if (any(!check_files$exists)) {
#         missing_files <- check_files |>
#             filter(!exists)

#         percent_missing <- check_files |>
#             dplyr::group_by(dir) |>
#             dplyr::summarise(
#                 percent_missing = sum(!exists) / dplyr::n() * 100
#             ) |>
#             dplyr::mutate(
#                 dir = basename(as.character(dir))
#             )

#         PEcAn.logger::logger.warn(
#             "Missing ", nrow(missing_files), "expected files\n",
#             "these directories have missing files:\n"
#         )
#         knitr::kable(
#             percent_missing |>
#                 dplyr::filter(percent_missing > 0),
#             digits = 0,
#             col.names = c("Directory", "Percent Missing")
#         )
#     } else {
#         PEcAn.logger::logger.info("Found all expected PEcAn output files.")
#     }
# }

# # Debugging: Check if all site_ids are present in SIPNET outputs
# missing_site_ids <- setdiff(site_ids, ens_run_dirs$site_id)
# if (length(missing_site_ids) > 0) {
#     PEcAn.logger::logger.warn(
#         "Missing site IDs in SIPNET outputs:\n",
#         paste(missing_site_ids, collapse = ", ")
#     )
# } else {
#     PEcAn.logger::logger.info("All site IDs are present in SIPNET outputs.")
# }