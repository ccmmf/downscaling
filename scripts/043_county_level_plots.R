## Create county-level plots
##
## Usage:
##   Rscript scripts/043_county_level_plots.R
##
## Inputs (from 000-config.R paths):
##   - model_outdir/out/county_summaries.csv (from 041_aggregate_to_county.R)
##   - model_outdir/out/downscaled_preds.csv (for mixed-scenario diffs and deltas)
##   - model_outdir/out/downscaled_deltas.csv (optional; for delta maps)
##   - data/ca_counties.gpkg (county boundaries)
##
## Outputs:
##   - figures/county_<pft>_<pool>_carbon_{density,stock}.webp
##   - figures/county_diff_woody_plus_annual_minus_woody_<pool>_carbon_{density,stock}.webp
##   - figures/county_delta_<pft>_<pool>_carbon_{density,stock}.webp
##   - Additional poster-grade exports for selected maps (PDF/SVG/PNG)
##
## Notes:
##   - Requires R/ggsave_optimized.R to be sourced via 000-config.R
##   - Uses facet columns for Mean and SD in each output
source("000-config.R")
PEcAn.logger::logger.info("Creating county-level plots")
county_boundaries <- sf::st_read(file.path(data_dir, "ca_counties.gpkg"))
county_summaries <- readr::read_csv(file = file.path(model_outdir, "county_summaries.csv"))

co_preds_to_plot <- county_summaries |>
  dplyr::right_join(county_boundaries, by = "county") |>
  dplyr::arrange(county, model_output, pft) |>
  tidyr::pivot_longer(
    cols = c(mean_total_c_Tg, sd_total_c_Tg, mean_c_density_Mg_ha, sd_c_density_Mg_ha),
    names_to = "stat",
    values_to = "value"
  ) |>
  dplyr::mutate(
    units = dplyr::case_when(
      stringr::str_detect(stat, "total_c") ~ "Carbon Stock (Tg)",
      stringr::str_detect(stat, "c_density") ~ "Carbon Density (Mg/ha)"
    ),
    stat = dplyr::case_when(
      stringr::str_detect(stat, "mean") ~ "Mean",
      stringr::str_detect(stat, "sd") ~ "SD"
    )
  )

combos <- co_preds_to_plot |>
  dplyr::filter(!is.na(model_output), !is.na(pft)) |>
  dplyr::distinct(pft, model_output, units) |>
  dplyr::arrange(pft, model_output, units)

# Optional filters via env vars to generate a subset only
pft_filter   <- Sys.getenv("PFT_FILTER",   unset = "")
pool_filter  <- Sys.getenv("POOL_FILTER",  unset = "")
units_filter <- Sys.getenv("UNITS_FILTER", unset = "")
if (nzchar(pft_filter))   combos <- dplyr::filter(combos, pft == !!pft_filter)
if (nzchar(pool_filter))  combos <- dplyr::filter(combos, model_output == !!pool_filter)
if (nzchar(units_filter)) combos <- dplyr::filter(combos, units == !!units_filter)

# Color scale controls
# Priority: COLOR_SCALE if set -> POSTER_PALETTE if TRUE -> default (plasma)
use_poster_palette <- isTRUE(as.logical(Sys.getenv("POSTER_PALETTE", "FALSE")))
color_scale_opt <- tolower(Sys.getenv("COLOR_SCALE", unset = ""))
custom_colors <- Sys.getenv("CUSTOM_COLORS", unset = "")  # comma-separated hex
custom_values <- Sys.getenv("CUSTOM_VALUES", unset = "")  # comma-separated numerics in [0,1]

fill_scale <- NULL
if (nzchar(color_scale_opt)) {
  if (color_scale_opt %in% c("viridis","plasma","magma","inferno","cividis")) {
    fill_scale <- ggplot2::scale_fill_viridis_c(option = color_scale_opt, na.value = "white")
  } else if (color_scale_opt == "custom" && nzchar(custom_colors)) {
    cols <- trimws(strsplit(custom_colors, ",", fixed = TRUE)[[1]])
    vals <- if (nzchar(custom_values)) as.numeric(trimws(strsplit(custom_values, ",", fixed = TRUE)[[1]])) else NULL
    if (is.null(vals)) {
      fill_scale <- ggplot2::scale_fill_gradientn(colors = cols, na.value = "white")
    } else {
      fill_scale <- ggplot2::scale_fill_gradientn(colors = cols, values = vals, na.value = "white")
    }
  }
}
if (is.null(fill_scale)) {
  if (use_poster_palette) {
    # Poster palette requested earlier: light green -> poster green -> poster purple
    fill_scale <- ggplot2::scale_fill_gradientn(
      colors = c("#d7f2d3", "#62b874", "#4a2f86"),
      values = c(0, 0.6, 1),
      na.value = "white"
    )
  } else {
    fill_scale <- ggplot2::scale_fill_viridis_c(option = "plasma", na.value = "white")
  }
}

p <- purrr::pmap(
  list(combos$pft, combos$model_output, combos$units),
  function(.pft, pool, unit) {
    dat <- dplyr::filter(co_preds_to_plot, pft == .pft, model_output == pool, units == unit) |>
      dplyr::mutate(value_plot = value)
    .plt <- ggplot2::ggplot(
      dat,
      ggplot2::aes(geometry = geom, fill = value_plot)
    ) +
      ggplot2::geom_sf(data = county_boundaries, mapping = ggplot2::aes(geometry = geom), fill = "white", color = "black", inherit.aes = FALSE) +
      ggplot2::geom_sf() +
      fill_scale +
      ggplot2::theme_minimal() +
      ggplot2::facet_grid(model_output ~ stat) +
      ggplot2::labs(
        title = paste("County-Level", pool, "-", .pft),
        fill = unit
      ) +
      ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = "top"))

    unit_key <- ifelse(unit == "Carbon Stock (Tg)", "stock",
      ifelse(unit == "Carbon Density (Mg/ha)", "density", NA)
    )
    pft_key <- stringr::str_replace_all(.pft, "[^A-Za-z0-9]+", "_")
    plotfile <- here::here("figures", paste0("county_", pft_key, "_", pool, "_carbon_", unit_key, ".webp"))
    PEcAn.logger::logger.info("Creating county-level plot for", pool, unit, "PFT:", .pft)
    ggsave_optimized(
      filename = plotfile,
      plot = .plt,
      width = 10, height = 5, units = "in", dpi = 96,
      bg = "white"
    )
    # Also export poster-grade formats for the AGB density map of woody perennials
    if (.pft == "woody perennial crop" && pool == "AGB" && unit_key == "density") {
      basefile <- here::here("figures", paste0("county_", pft_key, "_", pool, "_carbon_", unit_key))
      PEcAn.logger::logger.info("Exporting poster-grade versions (28.36x14.18 in):", paste0(basename(basefile), "{.pdf,.svg,.png}"))
      # Vector formats (preferred for print) at 14.18 in tall (2:1 aspect)
      ggsave_optimized(filename = paste0(basefile, ".pdf"), plot = .plt, width = 28.36, height = 14.18, units = "in", bg = "white")
      ggsave_optimized(filename = paste0(basefile, ".svg"), plot = .plt, width = 28.36, height = 14.18, units = "in", bg = "white")
      # High-resolution raster as fallback
      ggsave_optimized(filename = paste0(basefile, ".png"), plot = .plt, width = 28.36, height = 14.18, units = "in", dpi = 300, bg = "white")
    }
    # High-res PNG for woody perennials SOC stock (12" tall for print)
    if (.pft == "woody perennial crop" && pool == "TotSoilCarb" && unit_key == "stock") {
      basefile_soc_stock <- here::here("figures", paste0("county_", pft_key, "_", pool, "_carbon_", unit_key))
      # Maintain 2:1 aspect (width:height) used above: width=24in, height=12in
      PEcAn.logger::logger.info("Exporting 12in-tall high-res PNG:", paste0(basename(basefile_soc_stock), ".png"))
      ggsave_optimized(filename = paste0(basefile_soc_stock, ".png"), plot = .plt,
                       width = 28.36, height = 14.18, units = "in", dpi = 300, bg = "white")
    }
    # High-res exports for annual crop SOC stock
    if (.pft == "annual crop" && pool == "TotSoilCarb" && unit_key == "stock") {
      basefile_ann_soc_stock <- here::here("figures", paste0("county_", pft_key, "_", pool, "_carbon_", unit_key))
      PEcAn.logger::logger.info("Exporting high-res annual SOC stock (28.36x14.18 in):", paste0(basename(basefile_ann_soc_stock), "{.pdf,.png}"))
      ggsave_optimized(filename = paste0(basefile_ann_soc_stock, ".pdf"), plot = .plt,
                       width = 28.36, height = 14.18, units = "in", bg = "white")
      ggsave_optimized(filename = paste0(basefile_ann_soc_stock, ".png"), plot = .plt,
                       width = 28.36, height = 14.18, units = "in", dpi = 300, bg = "white")
    }
    # Export high-res PDF/PNG for County-level woody+herbaceous (mixed) SOC maps
    if (pool == "TotSoilCarb" && (grepl("woody", .pft, ignore.case = TRUE)) &&
        (grepl("annual", .pft, ignore.case = TRUE) || grepl("herb", .pft, ignore.case = TRUE))) {
      basefile_soc <- here::here("figures", paste0("county_", pft_key, "_", pool, "_carbon_", unit_key))
      PEcAn.logger::logger.info("Exporting high-res SOC versions (28.36x14.18 in):", paste0(basename(basefile_soc), "{.pdf,.png}"))
      ggsave_optimized(filename = paste0(basefile_soc, ".pdf"), plot = .plt, width = 28.36, height = 14.18, units = "in", bg = "white")
      ggsave_optimized(filename = paste0(basefile_soc, ".png"), plot = .plt, width = 28.36, height = 14.18, units = "in", dpi = 300, bg = "white")
    }
    return(.plt)
  }
)

## --- County-level differences from matched fields: (woody + annual) minus (woody perennial crop) --- ##
dp <- vroom::vroom(
  file.path(model_outdir, "downscaled_preds.csv"),
  col_types = readr::cols(
    site_id = readr::col_character(),
    pft = readr::col_character(),
    ensemble = readr::col_double(),
    c_density_Mg_ha = readr::col_double(),
    total_c_Mg = readr::col_double(),
    area_ha = readr::col_double(),
    county = readr::col_character(),
    model_output = readr::col_character()
  )
)
mix <- dp |>
  dplyr::filter(pft == "woody + annual") |>
  dplyr::select(site_id, ensemble, model_output, county, area_ha_mix = area_ha, total_c_Mg_mix = total_c_Mg)
wood <- dp |>
  dplyr::filter(pft == "woody perennial crop") |>
  dplyr::select(site_id, ensemble, model_output, county, area_ha_woody = area_ha, total_c_Mg_woody = total_c_Mg)

# Log if duplicate keys remain (should be rare after upstream normalization)
mix_dups <- mix |>
  dplyr::count(site_id, ensemble, model_output, county) |>
  dplyr::filter(n > 1)
wood_dups <- wood |>
  dplyr::count(site_id, ensemble, model_output, county) |>
  dplyr::filter(n > 1)
if (nrow(mix_dups) > 0 || nrow(wood_dups) > 0) {
  PEcAn.logger::logger.severe(paste0(
    "Duplicate keys detected prior to join (mix=", nrow(mix_dups), ", wood=", nrow(wood_dups), "). ",
    "Fix upstream duplication; refusing to silently aggregate."
  ))
}

diff_county <- mix |>
  dplyr::inner_join(wood, by = c("site_id", "ensemble", "model_output", "county")) |>
  dplyr::mutate(diff_total_Mg = total_c_Mg_mix - total_c_Mg_woody, area_ha = dplyr::coalesce(area_ha_woody, area_ha_mix)) |>
  dplyr::group_by(county, model_output, ensemble) |>
  dplyr::summarise(diff_total_Mg = sum(diff_total_Mg), total_ha = sum(area_ha), .groups = "drop") |>
  dplyr::mutate(diff_total_Tg = PEcAn.utils::ud_convert(diff_total_Mg, "Mg", "Tg"), diff_density_Mg_ha = diff_total_Mg / total_ha) |>
  dplyr::group_by(county, model_output) |>
  dplyr::summarise(mean_diff_total_Tg = mean(diff_total_Tg), mean_diff_density_Mg_ha = mean(diff_density_Mg_ha), .groups = "drop") |>
  dplyr::right_join(county_boundaries, by = "county")

for (pool in unique(stats::na.omit(diff_county$model_output))) {
  dat_pool <- dplyr::filter(diff_county, model_output == pool)

  # Density difference map (Mg/ha)
  p_density <- ggplot2::ggplot(dat_pool, ggplot2::aes(geometry = geom, fill = mean_diff_density_Mg_ha)) +
    ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "black") +
    ggplot2::geom_sf() +
    ggplot2::scale_fill_gradient2(low = "royalblue", mid = "white", high = "firebrick", midpoint = 0, na.value = "white") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Difference in Carbon Density (Mg/ha): (woody + annual) - (woody)", pool),
      fill = "Delta (Mg/ha)"
    )
  ggsave_optimized(
    filename = here::here("figures", paste0("county_diff_woody_plus_annual_minus_woody_", pool, "_carbon_density.webp")),
    plot = p_density,
    width = 10, height = 5, units = "in", dpi = 96, bg = "white"
  )

  # Stock difference map (Tg)
  p_stock <- ggplot2::ggplot(dat_pool, ggplot2::aes(geometry = geom, fill = mean_diff_total_Tg)) +
    ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "black") +
    ggplot2::geom_sf() +
    ggplot2::scale_fill_gradient2(low = "royalblue", mid = "white", high = "firebrick", midpoint = 0, na.value = "white") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Difference in Carbon Stock (Tg): (woody + annual) - (woody)", pool),
      fill = "Delta (Tg)"
    )
  ggsave_optimized(
    filename = here::here("figures", paste0("county_diff_woody_plus_annual_minus_woody_", pool, "_carbon_stock.webp")),
    plot = p_stock,
    width = 10, height = 5, units = "in", dpi = 96, bg = "white"
  )
}

## --- County-level deltas: start->end for woody and annual --- ##
delta_csv <- file.path(model_outdir, "downscaled_deltas.csv")
if (file.exists(delta_csv)) {
  deltas <- vroom::vroom(
    delta_csv,
    col_types = readr::cols(
      site_id = readr::col_character(),
      pft = readr::col_character(),
      ensemble = readr::col_double(),
      delta_c_density_Mg_ha = readr::col_double(),
      delta_total_c_Mg = readr::col_double(),
      area_ha = readr::col_double(),
      county = readr::col_character(),
      model_output = readr::col_character()
    )
  )

  delta_county <- deltas |>
    dplyr::group_by(model_output, pft, county, ensemble) |>
    dplyr::summarize(total_delta_Mg = sum(delta_total_c_Mg), total_ha = sum(area_ha), .groups = "drop") |>
    dplyr::mutate(
      delta_density_Mg_ha = total_delta_Mg / total_ha,
      delta_total_Tg = PEcAn.utils::ud_convert(total_delta_Mg, "Mg", "Tg")
    ) |>
    dplyr::group_by(model_output, pft, county) |>
    dplyr::summarize(
      mean_delta_density_Mg_ha = mean(delta_density_Mg_ha),
      mean_delta_total_Tg = mean(delta_total_Tg),
      .groups = "drop"
    ) |>
    dplyr::right_join(county_boundaries, by = "county")

  combos_delta <- delta_county |>
    dplyr::filter(!is.na(model_output), !is.na(pft)) |>
    dplyr::distinct(pft, model_output)

  purrr::pwalk(
    combos_delta,
    function(pft, model_output) {
      datp <- dplyr::filter(delta_county, pft == !!pft, model_output == !!model_output)
      pft_key <- stringr::str_replace_all(pft, "[^A-Za-z0-9]+", "_")
      # density
      p_den <- ggplot2::ggplot(datp, ggplot2::aes(geometry = geom, fill = mean_delta_density_Mg_ha)) +
        ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "black") +
        ggplot2::geom_sf() +
        ggplot2::scale_fill_gradient2(low = "royalblue", mid = "white", high = "firebrick", midpoint = 0, na.value = "white") +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste("Delta Density (start->end)", model_output, "-", pft),
          fill = "Delta (Mg/ha)"
        )
      ggsave_optimized(
        filename = here::here("figures", paste0("county_delta_", pft_key, "_", model_output, "_carbon_density.webp")),
        plot = p_den, width = 10, height = 5, units = "in", dpi = 96, bg = "white"
      )
      # stock
      p_stk <- ggplot2::ggplot(datp, ggplot2::aes(geometry = geom, fill = mean_delta_total_Tg)) +
        ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "black") +
        ggplot2::geom_sf() +
        ggplot2::scale_fill_gradient2(low = "royalblue", mid = "white", high = "firebrick", midpoint = 0, na.value = "white") +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste("Delta Stock (start->end)", model_output, "-", pft),
          fill = "Delta (Tg)"
        )
      ggsave_optimized(filename = here::here("figures", paste0("county_delta_", pft_key, "_", model_output, "_carbon_stock.webp")), plot = p_stk, width = 10, height = 5, units = "in", dpi = 96, bg = "white")
    }
  )
} else {
  PEcAn.logger::logger.warn("downscaled_deltas.csv not found; skipping delta maps")
}
