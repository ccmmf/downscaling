## Create county-level plots
source("000-config.R")
PEcAn.logger::logger.info("Creating county-level plots")
county_boundaries <- sf::st_read(file.path(data_dir, "ca_counties.gpkg"))
county_summaries <- readr::read_csv(file = file.path(model_outdir, "county_summaries.csv"))

co_preds_to_plot <- county_summaries |>
  dplyr::right_join(county_boundaries, by = "county") |>
  dplyr::arrange(county, model_output) |>
  tidyr::pivot_longer(
    cols = c(mean_total_c_Tg, sd_total_c_Tg, mean_c_density_Mg_ha, sd_c_density_Mg_ha),
    names_to = "stat",
    values_to = "value"
  ) |>
  dplyr::mutate(units = dplyr::case_when(
    stringr::str_detect(stat, "total_c") ~ "Carbon Stock (Tg)",
    stringr::str_detect(stat, "c_density") ~ "Carbon Density (Mg/ha)"
  ), stat = dplyr::case_when(
    stringr::str_detect(stat, "mean") ~ "Mean",
    stringr::str_detect(stat, "sd") ~ "SD"
  ))

units <- rep(unique(co_preds_to_plot$units), each = length(outputs_to_extract))
pool_x_units <- co_preds_to_plot |>
  dplyr::select(model_output, units) |>
  dplyr::distinct() |>
  # remove na
  dplyr::filter(!is.na(model_output)) |> # why is one field in SF county NA?
  dplyr::arrange(model_output, units) |>
  dplyr::filter(!is.na(model_output))

p <- purrr::map2(pool_x_units$model_output, pool_x_units$units, function(pool, unit) {
  .p <- ggplot2::ggplot(
    dplyr::filter(co_preds_to_plot, model_output == pool & units == unit),
    ggplot2::aes(geometry = geom, fill = value)
  ) +
    ggplot2::geom_sf(data = county_boundaries, fill = "lightgrey", color = "black") +
    ggplot2::geom_sf() +
    ggplot2::scale_fill_viridis_c(option = "plasma") +
    ggplot2::theme_minimal() +
    ggplot2::facet_grid(model_output ~ stat) +
    ggplot2::labs(
      title = paste("County-Level Predictions for", pool, unit),
      fill = "Value"
    )

  unit <- ifelse(unit == "Carbon Stock (Tg)", "stock",
    ifelse(unit == "Carbon Density (Mg/ha)", "density", NA)
  )

  plotfile <- here::here("figures", paste0("county_", pool, "_carbon_", unit, ".png"))
  PEcAn.logger::logger.info("Creating county-level plot for", pool, unit)
  ggplot2::ggsave(
    plot = .p,
    filename = plotfile,
    width = 10, height = 5,
    bg = "white"
  )
  return(.p)
})
