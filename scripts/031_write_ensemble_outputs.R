source("000-config.R")
PEcAn.logger::logger.info("***Starting Write Ensemble Outputs***")

cached_output_file <- file.path(cache_dir, "ensemble_output.rds")
ens_arrays <- readRDS(cached_output_file)

output_format <- "long" # "netcdf", "rds"
if ("rds" %in% output_format) {
    file.copy(
        cached_output_file,
        file.path(model_outdir, "ensemble_output.rds"),
        overwrite = TRUE
    )
    PEcAn.logger::logger.info("Ensemble output RDS file copied to model_outdir.")
}

if ("long" %in% output_format) {
    # --- 2. Create EFI Standard v1.0 long format data frame ---
    efi_long <- ens_results |>
        dplyr::rename(datetime = time) |>
        dplyr::select(datetime, site_id, ensemble, variable, prediction)
    readr::write_csv(efi_long, file.path(model_outdir, "ensemble_output.csv"))
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
