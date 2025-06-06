## Create Ensemble Output For Downscaling
## Below, three different output formats are created:
## 1. 4-D array (time, site, ensemble, variable)
## 2. long format data frame (time, site, ensemble, variable)
## 3. NetCDF file (time, site, ensemble, variable)

source("000-config.R")
PEcAn.logger::logger.info("***Starting Write Ensemble Outputs***")

cached_output_file <- file.path(cache_dir, "ensemble_results.csv")
ens_arrays <- readr::read_csv(cached_output_file)

output_format <- "long" # "netcdf", "rds"

# --- 1. Create 4-D array ---
if ("rds" %in% output_format) {
    # Add a time dimension (even if of length 1) so that dimensions are: [time, site, ensemble, variable]
    unique_times <- sort(unique(ens_results$time))
    if (length(unique_times) != length(start_year:end_year)) {
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
    rds_file <- file.path(cache_dir, "ensemble_output.rds")
    saveRDS(ens_arrays, file = rds_file)
    PEcAn.logger::logger.info("Ensemble output arrays written to model_outdir.")
}

if ("long" %in% output_format) {
    # --- 2. Create EFI Standard v1.0 long format data frame ---
    efi_long <- ens_results |>
        dplyr::rename(datetime = time) |>
        dplyr::select(datetime, site_id, ensemble, variable, prediction)
    readr::write_csv(efi_long, file.path(model_outdir, "ensemble_output.csv"))

    # Debugging: Check if all site_ids are included in ensemble_output.csv
    missing_site_ids <- setdiff(site_ids, unique(efi_long$site_id))
    if (length(missing_site_ids) > 0) {
        PEcAn.logger::logger.warn(
            "Missing site IDs in ensemble_output.csv:\n",
            paste(missing_site_ids, collapse = ", ")
        )
    } else {
        PEcAn.logger::logger.info("All site IDs are included in ensemble_output.csv.")
    }
}

if ("netcdf" %in% output_format) {
    # --- 3. Create EFI Standard v1.0 NetCDF files ---
    PEcAn.logger::logger.info("Creating EFI-compliant NetCDF file for ensemble output...")
    # Make sure all required variables are defined before this block:
    # unique_times, site_ids, design_points, ens_ids, settings, model_outdir

    # Get dimension names / site IDs
    time_char <- unique_times

    lat <- design_points |>
        dplyr::filter(site_id %in% site_ids) |> # only required when testing w/ subset
        dplyr::pull(lat)
    lon <- design_points |>
        dplyr::filter(site_id %in% site_ids) |>
        dplyr::pull(lon)

    # Convert time to CF-compliant values using PEcAn.utils::datetime2cf
    time_units <- "days since 1970-01-01 00:00:00"
    cf_time <- PEcAn.utils::datetime2cf(time_char, unit = time_units)

    # TODO: could accept start year as an argument to the to_ncdim function if variable = 'time'? Or set default?
    #       Otherwise this returns an invalid dimension
    # time_dim <- PEcAn.utils::to_ncdim("time", cf_time)
    time_dim <- ncdf4::ncdim_def(
        name = "ntime",
        longname = "Time middle averaging period",
        units = time_units,
        vals = cf_time,
        calendar = "standard",
        unlim = FALSE
    )
    site_dim     <- ncdim_def("site", "", vals = seq_along(site_ids), longname = "Site ID", unlim = FALSE)
    ensemble_dim <- ncdim_def("ensemble", "", vals = ens_ids, longname = "ensemble member", unlim = FALSE)
    site_dim     <- ncdf4::ncdim_def("site", "", vals = seq_along(site_ids), longname = "Site ID", unlim = FALSE)
    ensemble_dim <- ncdf4::ncdim_def("ensemble", "", vals = ens_ids, longname = "ensemble member", unlim = FALSE)
    dims         <- list(time_dim, site_dim, ensemble_dim)

    # Define forecast variables:
    agb_ncvar <- ncdf4::ncvar_def(
        name = "AGB",
        units = "kg C m-2", # correct units
        dim = dims,
        longname = "Total aboveground biomass"
    )
    soc_ncvar <- ncdf4::ncvar_def(
        name = "TotSoilCarb",
        units = "kg C m-2", # correct units
        dim = dims,
        longname = "Total Soil Carbon"
    )
    time_var <- ncdf4::ncvar_def(
        name = "time",
        units = "days since 1970-01-01 00:00:00",
        dim = time_dim,
        longname = "Time dimension"
    )
    lat_var <- ncdf4::ncvar_def(
        name = "lat",
        units = "degrees_north",
        dim = site_dim,
        longname = "Latitude"
    )

    lon_var <- ncdf4::ncvar_def(
        name = "lon",
        units = "degrees_east",
        dim = site_dim,
        longname = "Longitude"
    )

    nc_vars <- list(
        time = time_var,
        lat = lat_var,
        lon = lon_var,
        AGB = agb_ncvar,
        TotSoilCarb = soc_ncvar
    )

    nc_file <- file.path(model_outdir, "ensemble_output.nc")

    if (file.exists(nc_file)) {
        file.remove(nc_file)
    }

    nc_out <- ncdf4::nc_create(nc_file, nc_vars)
    # Add attributes to coordinate variables for clarity
    # ncdf4::ncatt_put(nc_out, "time", "bounds", "time_bounds", prec = NA)
    # ncdf4::ncatt_put(nc_out, "time", "axis", "T", prec = NA)
    # ncdf4::ncatt_put(nc_out, "site", "axis", "Y", prec = NA)
    # ncdf4::ncatt_put(nc_out, "ensemble", "axis", "E", prec = NA)

    # Write data into the netCDF file.
    ncdf4::ncvar_put(nc_out, time_var, cf_time)
    ncdf4::ncvar_put(nc_out, lat_var, lat)
    ncdf4::ncvar_put(nc_out, time_var, cf_time)
    ncdf4::ncvar_put(nc_out, lat_var, lat)
    ncdf4::ncvar_put(nc_out, lon_var, lon)
    ncdf4::ncvar_put(nc_out, agb_ncvar, ens_arrays[["AGB"]])
    ncdf4::ncvar_put(nc_out, soc_ncvar, ens_arrays[["TotSoilCarb"]])

    # Get Run metadata from STATUS file
    forecast_time <- readr::read_tsv(
        file.path(model_outdir, "output", "STATUS"),
        col_names = FALSE
    ) |>
        dplyr::filter(X1 == "FINISHED") |>
        dplyr::pull(X3)

    forecast_iteration_id <- as.numeric(forecast_time) # or is run_id available?
    obs_flag <- 0

    ncdf4::ncatt_put(nc_out, 0, "model_name", settings$model$type)
    ncdf4::ncatt_put(nc_out, 0, "model_version", settings$model$revision)
    ncdf4::ncatt_put(nc_out, 0, "iteration_id", forecast_iteration_id)
    ncdf4::ncatt_put(nc_out, 0, "forecast_time", forecast_time)
    ncdf4::ncatt_put(nc_out, 0, "obs_flag", obs_flag)
    ncdf4::ncatt_put(nc_out, 0, "creation_date", format(Sys.time(), "%Y-%m-%d"))
    # Close the netCDF file.
    ncdf4::nc_close(nc_out)

    PEcAn.logger::logger.info("EFI-compliant netCDF file 'ensemble_output.nc' created.")
    PEcAn.logger::logger.info("All ensemble outputs (RDS, CSV, NetCDF) are in kg C m-2 for AGB and TotSoilCarb.")
}
