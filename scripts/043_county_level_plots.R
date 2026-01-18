## Create county-level and field-level plots
source("000-config.R")
PEcAn.logger::logger.info("Creating county and field level plots")

# ---- Load data ----
county_boundaries <- sf::st_read(file.path(data_dir, "ca_counties.gpkg"))

county_summaries <- readr::read_csv(
  file.path(model_outdir, "county_aggregated_preds.csv"),
  show_col_types = FALSE
)

# backward compatibility for scenario column
if (!"scenario" %in% names(county_summaries)) {
  county_summaries <- county_summaries |>
    dplyr::mutate(scenario = "baseline")
  PEcAn.logger::logger.info("No scenario column found; using 'baseline'")
}

scenarios <- unique(county_summaries$scenario)
PEcAn.logger::logger.info("Scenarios to plot: ", paste(scenarios, collapse = ", "))

# ----------County-level STOCK maps----------
# NOTE: Per CARB request, county-level density choropleth maps are no longer 
# produced. Density is shown at field level instead.

co_stocks <- county_summaries |>
  dplyr::right_join(county_boundaries, by = "county") |>
  dplyr::filter(!is.na(scenario), !is.na(model_output), !is.na(pft))

combos_stock <- co_stocks |>
  dplyr::distinct(scenario, pft, model_output) |>
  dplyr::arrange(scenario, pft, model_output)

PEcAn.logger::logger.info("Creating ", nrow(combos_stock), " county stock maps")

purrr::pwalk(
  combos_stock,
  function(scenario, pft, model_output) {
    dat <- dplyr::filter(
      co_stocks,
      scenario == !!scenario,
      pft == !!pft,
      model_output == !!model_output
    )
    
    p_stock <- ggplot2::ggplot(dat, ggplot2::aes(geometry = geom, fill = mean_total_c_Tg)) +
      ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "black") +
      ggplot2::geom_sf() +
      ggplot2::scale_fill_viridis_c(option = "plasma", na.value = "white") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = paste("County Carbon Stock:", model_output, "-", pft),
        subtitle = paste("Scenario:", scenario),
        fill = "Carbon\nStock (Tg)"
      )
    
    pft_key <- stringr::str_replace_all(pft, "[^A-Za-z0-9]+", "_")
    scenario_key <- stringr::str_replace_all(scenario, "[^A-Za-z0-9]+", "_")
    
    ggsave_optimized(
      filename = here::here("figures", paste0(
        "county_", scenario_key, "_", pft_key, "_", model_output, "_carbon_stock.webp"
      )),
      plot = p_stock,
      width = 10, height = 6, units = "in", dpi = 96, bg = "white"
    )
    PEcAn.logger::logger.info("Created stock map: ", scenario, "/", pft, "/", model_output)
  }
)


# ----------Field-level density maps----------
PEcAn.logger::logger.info("Creating field-level density maps")

# Load field-level predictions
dp <- vroom::vroom(
  file.path(model_outdir, "downscaled_preds.csv"),
  col_types = readr::cols(
    scenario = readr::col_character(),
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

if (!"scenario" %in% names(dp)) {
  dp <- dp |> dplyr::mutate(scenario = "baseline")
}

# Load field centroids for point maps
ca_fields <- sf::st_read(file.path(data_dir, "ca_fields.gpkg"))
field_centroids <- ca_fields |>
  sf::st_centroid() |>
  dplyr::select(site_id, geom)

# Compute mean density per field (across ensembles)
field_mean_density <- dp |>
  dplyr::group_by(scenario, model_output, pft, site_id) |>
  dplyr::summarize(
    mean_c_density_Mg_ha = mean(c_density_Mg_ha, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::inner_join(
    sf::st_drop_geometry(field_centroids) |> dplyr::select(site_id),
    by = "site_id"
  )

# Join with geometry
field_mean_density_sf <- field_mean_density |>
  dplyr::inner_join(field_centroids, by = "site_id") |>
  sf::st_as_sf()

combos_field <- field_mean_density |>
  dplyr::distinct(scenario, pft, model_output) |>
  dplyr::arrange(scenario, pft, model_output)

PEcAn.logger::logger.info("Creating ", nrow(combos_field), " field density maps")

purrr::pwalk(
  combos_field,
  function(scenario, pft, model_output) {
    dat <- dplyr::filter(
      field_mean_density_sf,
      scenario == !!scenario,
      pft == !!pft,
      model_output == !!model_output
    )
    
    p_field <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = county_boundaries, fill = "gray95", color = "gray70", linewidth = 0.3) +
      ggplot2::geom_sf(data = dat, ggplot2::aes(color = mean_c_density_Mg_ha), size = 0.4, alpha = 0.7) +
      ggplot2::scale_color_viridis_c(option = "plasma", na.value = "gray50") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = paste("Field Carbon Density:", model_output, "-", pft),
        subtitle = paste("Scenario:", scenario),
        color = "Density\n(Mg/ha)"
      )
    
    pft_key <- stringr::str_replace_all(pft, "[^A-Za-z0-9]+", "_")
    scenario_key <- stringr::str_replace_all(scenario, "[^A-Za-z0-9]+", "_")
    
    ggsave_optimized(
      filename = here::here("figures", paste0(
        "field_", scenario_key, "_", pft_key, "_", model_output, "_carbon_density.webp"
      )),
      plot = p_field,
      width = 10, height = 6, units = "in", dpi = 96, bg = "white"
    )
    PEcAn.logger::logger.info("Created field density map: ", scenario, "/", pft, "/", model_output)
  }
)

# ----------PFT difference maps (woody + annual) minus (woody)----------
# NOTE: This section only applies to multi-PFT mode, skipped for scenario mode
mix <- dp |>
  dplyr::filter(pft == "woody + annual")
wood <- dp |>
  dplyr::filter(pft == "woody perennial crop")

# Only create diff maps if both PFTs exist
if (nrow(mix) > 0 && nrow(wood) > 0) {
  PEcAn.logger::logger.info("Creating PFT difference maps (woody+annual - woody)")
  
  mix_sel <- mix |>
    dplyr::select(site_id, ensemble, model_output, county, area_ha_mix = area_ha, total_c_Mg_mix = total_c_Mg)
  wood_sel <- wood |>
    dplyr::select(site_id, ensemble, model_output, county, area_ha_woody = area_ha, total_c_Mg_woody = total_c_Mg)
  
  # Check for duplicates
 mix_dups <- mix_sel |>
    dplyr::count(site_id, ensemble, model_output, county) |>
    dplyr::filter(n > 1)
  wood_dups <- wood_sel |>
    dplyr::count(site_id, ensemble, model_output, county) |>
    dplyr::filter(n > 1)
  
  if (nrow(mix_dups) > 0 || nrow(wood_dups) > 0) {
    PEcAn.logger::logger.warn(
      "Duplicate keys detected (mix=", nrow(mix_dups), ", wood=", nrow(wood_dups), "); skipping diff maps"
    )
  } else {
    diff_county <- mix_sel |>
      dplyr::inner_join(wood_sel, by = c("site_id", "ensemble", "model_output", "county")) |>
      dplyr::mutate(
        diff_total_Mg = total_c_Mg_mix - total_c_Mg_woody,
        area_ha = dplyr::coalesce(area_ha_woody, area_ha_mix)
      ) |>
      dplyr::group_by(county, model_output, ensemble) |>
      dplyr::summarise(diff_total_Mg = sum(diff_total_Mg), total_ha = sum(area_ha), .groups = "drop") |>
      dplyr::mutate(
        diff_total_Tg = PEcAn.utils::ud_convert(diff_total_Mg, "Mg", "Tg"),
        diff_density_Mg_ha = diff_total_Mg / total_ha
      ) |>
      dplyr::group_by(county, model_output) |>
      dplyr::summarise(
        mean_diff_total_Tg = mean(diff_total_Tg),
        mean_diff_density_Mg_ha = mean(diff_density_Mg_ha),
        .groups = "drop"
      ) |>
      dplyr::right_join(county_boundaries, by = "county")
    
    for (pool in unique(stats::na.omit(diff_county$model_output))) {
      dat_pool <- dplyr::filter(diff_county, model_output == pool)
      
      # Stock difference map only (density removed per CARB)
      p_stock <- ggplot2::ggplot(dat_pool, ggplot2::aes(geometry = geom, fill = mean_diff_total_Tg)) +
        ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "black") +
        ggplot2::geom_sf() +
        ggplot2::scale_fill_gradient2(
          low = "royalblue", mid = "white", high = "firebrick",
          midpoint = 0, na.value = "white"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste("Difference in Carbon Stock: (woody + annual) - (woody)"),
          subtitle = pool,
          fill = "Delta (Tg)"
        )
      
      ggsave_optimized(
        filename = here::here("figures", paste0(
          "county_diff_woody_plus_annual_minus_woody_", pool, "_carbon_stock.webp"
        )),
        plot = p_stock,
        width = 10, height = 6, units = "in", dpi = 96, bg = "white"
      )
    }
  }
} else {
  PEcAn.logger::logger.info("Skipping PFT diff maps (woody + annual or woody perennial not present)")
}


## --- County-level deltas: start->end carbon change for all PFTs and scenarios --- ##
delta_csv <- file.path(model_outdir, "downscaled_deltas.csv")
if (file.exists(delta_csv)) {
  PEcAn.logger::logger.info("Creating delta maps")
  
  deltas <- vroom::vroom(
    delta_csv,
    col_types = readr::cols(
      scenario = readr::col_character(),
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
  
  if (!"scenario" %in% names(deltas)) {
    deltas <- deltas |> dplyr::mutate(scenario = "baseline")
  }
  
  delta_county <- deltas |>
    dplyr::group_by(scenario, model_output, pft, county, ensemble) |>
    dplyr::summarize(
      total_delta_Mg = sum(delta_total_c_Mg),
      total_ha = sum(area_ha),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      delta_density_Mg_ha = total_delta_Mg / total_ha,
      delta_total_Tg = PEcAn.utils::ud_convert(total_delta_Mg, "Mg", "Tg")
    ) |>
    dplyr::group_by(scenario, model_output, pft, county) |>
    dplyr::summarize(
      mean_delta_density_Mg_ha = mean(delta_density_Mg_ha),
      mean_delta_total_Tg = mean(delta_total_Tg),
      .groups = "drop"
    ) |>
    dplyr::right_join(county_boundaries, by = "county")
  
  combos_delta <- delta_county |>
    dplyr::filter(!is.na(scenario), !is.na(model_output), !is.na(pft)) |>
    dplyr::distinct(scenario, pft, model_output)
  
  purrr::pwalk(
    combos_delta,
    function(scenario, pft, model_output) {
      datp <- dplyr::filter(
        delta_county,
        scenario == !!scenario,
        pft == !!pft,
        model_output == !!model_output
      )
      
      pft_key <- stringr::str_replace_all(pft, "[^A-Za-z0-9]+", "_")
      scenario_key <- stringr::str_replace_all(scenario, "[^A-Za-z0-9]+", "_")
      
      # Stock delta map only (per CARB - no county density)
      p_stk <- ggplot2::ggplot(datp, ggplot2::aes(geometry = geom, fill = mean_delta_total_Tg)) +
        ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "black") +
        ggplot2::geom_sf() +
        ggplot2::scale_fill_gradient2(
          low = "royalblue", mid = "white", high = "firebrick",
          midpoint = 0, na.value = "white"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          title = paste("Delta Stock (start→end):", model_output, "-", pft),
          subtitle = paste("Scenario:", scenario),
          fill = "Delta (Tg)"
        )
      
      ggsave_optimized(
        filename = here::here("figures", paste0(
          "county_delta_", scenario_key, "_", pft_key, "_", model_output, "_carbon_stock.webp"
        )),
        plot = p_stk,
        width = 10, height = 6, units = "in", dpi = 96, bg = "white"
      )
      PEcAn.logger::logger.info("Created delta map: ", scenario, "/", pft, "/", model_output)
    }
  )
} else {
  PEcAn.logger::logger.warn("downscaled_deltas.csv not found; skipping delta maps")
}

PEcAn.logger::logger.info("Finished creating all plots")