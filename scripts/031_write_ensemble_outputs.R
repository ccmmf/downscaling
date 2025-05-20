source("000-config.R")
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
    # NOTE: prediction is in kg C m-2

    readr::write_csv(efi_long, file.path(model_outdir, "ensemble_output.csv"))
    # Consider: write a README or add a comment about units in the CSV
}

if ("netcdf" %in% output_format) {
    # --- 3. Create EFI Standard v1.0 NetCDF files ---
    #### --- 3. Create EFI Standard v1.0 NetCDF files
    library(ncdf4)
    # Assume these objects already exist (created above):
    #   unique_times: vector of unique datetime strings
    #   design_points: data frame with columns lat, lon, and id (site_ids)
    #   ens_ids: vector of ensemble member numbers (numeric)
    #   ens_arrays: list with elements "AGB" and "TotSoilCarb" that are arrays
    #       with dimensions: datetime, site, ensemble

    # Get dimension names / site IDs
    time_char <- unique_times

    lat <- design_points |>
        filter(site_id %in% site_ids) |> # only required when testing w/ subset
        dplyr::pull(lat)
    lon <- design_points |>
        filter(site_id %in% site_ids) |>
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
    site_dim <- ncdim_def("site", "", vals = seq_along(site_ids), longname = "Site ID", unlim = FALSE)
    ensemble_dim <- ncdim_def("ensemble", "", vals = ens_ids, longname = "ensemble member", unlim = FALSE)

    # Use dims in reversed order so that the unlimited (time) dimension ends up as the record dimension:
    dims <- list(time_dim, site_dim, ensemble_dim)

    # Define forecast variables:
    agb_ncvar <- ncvar_def(
        name = "AGB",
        units = "kg C m-2", # correct units
        dim = dims,
        longname = "Total aboveground biomass"
    )
    soc_ncvar <- ncvar_def(
        name = "TotSoilCarb",
        units = "kg C m-2", # correct units
        dim = dims,
        longname = "Total Soil Carbon"
    )
    time_var <- ncvar_def(
        name = "time",
        units = "days since 1970-01-01 00:00:00",
        dim = time_dim,
        longname = "Time dimension"
    )
    lat_var <- ncvar_def(
        name = "lat",
        units = "degrees_north",
        dim = site_dim,
        longname = "Latitude"
    )

    lon_var <- ncvar_def(
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
    ncvar_put(nc_out, time_var, cf_time)
    ncvar_put(nc_out, lat_var, lat)
    ncvar_put(nc_out, lon_var, lon)
    ncvar_put(nc_out, agb_ncvar, ens_arrays[["AGB"]])
    ncvar_put(nc_out, soc_ncvar, ens_arrays[["TotSoilCarb"]])

    ## Add global attributes per EFI standards.

    # Get Run metadata from log filename
    # ??? is there a more reliable way to do this?
    forecast_time <- readr::read_tsv(
        file.path(model_outdir, "output", "STATUS"),
        col_names = FALSE
    ) |>
        filter(X1 == "FINISHED") |>
        pull(X3)
    forecast_iteration_id <- as.numeric(forecast_time) # or is run_id available?
    obs_flag <- 0

    ncatt_put(nc_out, 0, "model_name", settings$model$type)
    ncatt_put(nc_out, 0, "model_version", settings$model$revision)
    ncatt_put(nc_out, 0, "iteration_id", forecast_iteration_id)
    ncatt_put(nc_out, 0, "forecast_time", forecast_time)
    ncatt_put(nc_out, 0, "obs_flag", obs_flag)
    ncatt_put(nc_out, 0, "creation_date", format(Sys.time(), "%Y-%m-%d"))
    # Close the netCDF file.
    nc_close(nc_out)

    PEcAn.logger::logger.info("EFI-compliant netCDF file 'ensemble_output.nc' created.")
    PEcAn.logger::logger.info("All ensemble outputs (RDS, CSV, NetCDF) are in kg C m-2 for AGB and TotSoilCarb.")
}