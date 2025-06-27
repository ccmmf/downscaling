efi_long_to_array <- function(ensemble_output, output_file = NULL, save = FALSE) {

    if(is.null(output_file) && save) {
        PEcAn.logger::logger.error("Must provide output_file if save is TRUE")
    }
    # Extract unique times, site IDs, and ensemble IDs from ensemble_output
    unique_times <- sort(unique(ensemble_output$datetime))
    site_ids <- unique(ensemble_output$site_id)
    ens_ids <- unique(ensemble_output$ensemble)
    variables <- unique(ensemble_output$variable)

    # Create a list to hold one 3-D array per variable
    ens_arrays <- list()
    for (var in variables) {
        arr <- array(NA,
            dim = c(length(unique_times), length(site_ids), length(ens_ids)),
            dimnames = list(
                datetime = as.character(unique_times),
                site_id = site_ids,
                ensemble = as.character(ens_ids)
            )
        )
        subset_idx <- which(ensemble_output$variable == var)
        if (length(subset_idx) > 0) {
            i_time <- match(ensemble_output$datetime[subset_idx], unique_times)
            i_site <- match(ensemble_output$site_id[subset_idx], site_ids)
            i_ens <- match(ensemble_output$ensemble[subset_idx], ens_ids)
            arr[cbind(i_time, i_site, i_ens)] <- ensemble_output$prediction[subset_idx]
        }
        ens_arrays[[var]] <- arr
    }
    if(write){
        saveRDS(ens_arrays, output_file)
        PEcAn.logger::logger.info("Ensemble arrays saved to:", output_file)
    }
    return(ens_arrays)
}

efi_long_to_netcdf <- function(ensemble_output, output_file) {
    # Extract required information from ensemble_output
    unique_times <- sort(unique(ensemble_output$datetime))
    site_ids <- unique(ensemble_output$site_id)
    ens_ids <- unique(ensemble_output$ensemble)
    design_points <- ensemble_output |>
        dplyr::select(site_id, lat, lon) |>
        dplyr::distinct()

    lat <- design_points$lat
    lon <- design_points$lon

    # Convert time to CF-compliant values
    time_units <- "days since 1970-01-01 00:00:00"
    cf_time <- PEcAn.utils::datetime2cf(unique_times, unit = time_units)

    # Define dimensions
    time_dim <- ncdf4::ncdim_def("time", "Time middle averaging period", time_units, cf_time, unlim = FALSE)
    site_dim <- ncdf4::ncdim_def("site", "Site ID", "", seq_along(site_ids), unlim = FALSE)
    ensemble_dim <- ncdf4::ncdim_def("ensemble", "Ensemble member", "", ens_ids, unlim = FALSE)
    dims <- list(time_dim, site_dim, ensemble_dim)

    # Define forecast variables
    variables <- unique(ensemble_output$variable)
    nc_vars <- lapply(variables, function(var) {
        ncdf4::ncvar_def(
            name = var,
            units = "kg C m-2",
            dim = dims,
            longname = paste("Variable:", var)
        )
    })

    # Create NetCDF file
    nc_out <- ncdf4::nc_create(output_file, nc_vars)
    ncdf4::ncvar_put(nc_out, time_dim, cf_time)
    ncdf4::ncvar_put(nc_out, site_dim, lat)
    ncdf4::ncvar_put(nc_out, site_dim, lon)

    for (var in variables) {
        ncdf4::ncvar_put(nc_out, nc_vars[[var]], efi_long_to_array(ensemble_output)[[var]])
    }

    ncdf4::nc_close(nc_out)
    PEcAn.logger::logger.info("EFI-compliant NetCDF file created:", output_file)
}
