## county-level and field-level plots.

source("000-config.R")
PEcAn.logger::logger.info("county and field level plots")

# plotting helpers.

# map theme.
theme_ccmmf_map <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = base_size + 2, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = base_size, color = "grey30"),
      legend.title = ggplot2::element_text(size = base_size - 1),
      legend.text = ggplot2::element_text(size = base_size - 2),
      legend.key.height = ggplot2::unit(1.2, "cm"),
      strip.text = ggplot2::element_text(size = base_size, face = "bold"),
      axis.text = ggplot2::element_text(size = base_size - 3),
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}

# display labels.
format_variable_label <- function(var_name) {
  labels <- c(
    TotSoilCarb = "Total Soil Carbon",
    AGB = "Aboveground Biomass",
    N2O_flux = "N2O Flux",
    CH4_flux = "CH4 Flux"
  )
  unname(ifelse(var_name %in% names(labels), labels[var_name], var_name))
}

format_scenario_label <- function(scenario) {
  labels <- c(
    baseline = "Baseline",
    compost = "Compost",
    reduced_till = "Reduced Till",
    zero_till = "Zero Till",
    reduced_irrig_drip = "Reduced Irrig. (Drip)",
    stacked = "Stacked"
  )
  unname(ifelse(scenario %in% names(labels), labels[scenario], scenario))
}

safe_key <- function(x) stringr::str_replace_all(x, "[^A-Za-z0-9]+", "_")

# legend unit labels.
# pools: Mg C or Mg C/ha (from 040, kg/m2 -> Mg/ha).
# fluxes: kg gas/yr or kg gas/ha/yr (from 040, kg/m2/s -> kg/ha/yr).
format_unit_label <- function(model_output,
                              map_type = c("county_total", "density",
                                           "diff_total", "diff_density")) {
  map_type <- match.arg(map_type)

  gas_label <- dplyr::case_match(
    model_output,
    "N2O_flux" ~ "N\u2082O",
    "CH4_flux" ~ "CH\u2084",
    .default = NA_character_
  )
  is_flux <- !is.na(gas_label)

  switch(map_type,
    county_total = if (is_flux) {
      paste0("Total\n(Gg ", gas_label, " yr\u207b\u00b9)")
    } else {
      "Total\n(Gg C)"
    },
    density = if (is_flux) {
      paste0("Density\n(kg ", gas_label, " ha\u207b\u00b9 yr\u207b\u00b9)")
    } else {
      "Density\n(Mg C ha\u207b\u00b9)"
    },
    diff_total = if (is_flux) {
      paste0("Difference\n(Gg ", gas_label, " yr\u207b\u00b9)")
    } else {
      "Difference\n(Gg C)"
    },
    diff_density = if (is_flux) {
      paste0("Difference\n(kg ", gas_label, " ha\u207b\u00b9 yr\u207b\u00b9)")
    } else {
      "Difference\n(Mg C ha\u207b\u00b9)"
    }
  )
}

# load data.
county_boundaries <- sf::st_read(file.path(data_dir, "ca_counties.gpkg"))

county_summaries <- readr::read_csv(
  file.path(model_outdir, "county_aggregated_preds.csv"),
  show_col_types = FALSE
)

# report GHG fluxes as mass of gas, not element.
# SIPNET outputs CH4 as kg C and N2O as kg N; convert to kg CH4 and kg N2O.
# then Mg -> Gg for stocks, kg gas -> Gg gas for fluxes.
county_summaries <- county_summaries |>
  dplyr::mutate(
    .gas_factor = dplyr::case_match(model_output,
      "N2O_flux" ~ 44 / 28,
      "CH4_flux" ~ 16 / 12,
      .default = 1
    ),
    mean_total_per_county = mean_total_per_county * .gas_factor,
    mean_density_per_ha = mean_density_per_ha * .gas_factor,
    sd_total_per_county = sd_total_per_county * .gas_factor,
    sd_density_per_ha = sd_density_per_ha * .gas_factor,
    .total_divisor = dplyr::case_match(model_output,
      c("TotSoilCarb", "AGB") ~ 1e3,
      c("N2O_flux", "CH4_flux") ~ 1e6,
      .default = 1
    ),
    mean_total_per_county = mean_total_per_county / .total_divisor,
    sd_total_per_county = sd_total_per_county / .total_divisor
  ) |>
  dplyr::select(-.gas_factor, -.total_divisor)

# older outputs have no scenario column.
if (!"scenario" %in% names(county_summaries)) {
  county_summaries <- county_summaries |>
    dplyr::mutate(scenario = "baseline")
  PEcAn.logger::logger.info("no scenario column; using 'baseline'")
}

scenarios <- unique(county_summaries$scenario)
PEcAn.logger::logger.info("scenarios: ", paste(scenarios, collapse = ", "))

# fixed scenario ordering.
scenario_levels <- intersect(management_scenarios, scenarios)
county_summaries <- county_summaries |>
  dplyr::mutate(scenario = factor(scenario, levels = scenario_levels))

# county data joined with geometry.
co_stocks <- county_summaries |>
  dplyr::right_join(county_boundaries, by = "county") |>
  dplyr::filter(!is.na(scenario), !is.na(model_output), !is.na(pft))

# per-(pft, model_output) fill limits, shared across scenarios.
county_stock_limits <- county_summaries |>
  dplyr::group_by(pft, model_output) |>
  dplyr::summarize(
    lo = min(mean_total_per_county, na.rm = TRUE),
    hi = max(mean_total_per_county, na.rm = TRUE),
    .groups = "drop"
  )

PEcAn.logger::logger.info(
  "stock limits for ",
  nrow(county_stock_limits), " (pft, variable) combinations"
)

combos <- co_stocks |>
  dplyr::distinct(pft, model_output) |>
  dplyr::arrange(pft, model_output)


# section 1 - county stock maps.
PEcAn.logger::logger.info("county stock maps")

for (row_i in seq_len(nrow(combos))) {
  pft_i <- combos$pft[row_i]
  mo_i <- combos$model_output[row_i]

  lims <- county_stock_limits |>
    dplyr::filter(pft == pft_i, model_output == mo_i)

  for (scn in scenario_levels) {
    dat <- dplyr::filter(
      co_stocks,
      as.character(scenario) == scn,
      pft == pft_i,
      model_output == mo_i
    )
    if (nrow(dat) == 0) next

    p <- ggplot2::ggplot(dat, ggplot2::aes(geometry = geom, fill = mean_total_per_county)) +
      ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey60") +
      ggplot2::geom_sf() +
      ggplot2::scale_fill_viridis_c(
        option = "viridis", na.value = "white",
        limits = c(lims$lo, lims$hi),
        labels = scales::label_comma()
      ) +
      theme_ccmmf_map() +
      ggplot2::labs(
        title = paste("County Total:", format_variable_label(mo_i)),
        subtitle = paste("Scenario:", format_scenario_label(scn), " --  PFT:", pft_i),
        fill = format_unit_label(mo_i, "county_total")
      )

    ggsave_optimized(
      filename = here::here("figures", paste0(
        "county_", safe_key(scn), "_", safe_key(pft_i), "_", mo_i, "_carbon_stock.webp"
      )),
      plot = p, width = 10, height = 8, units = "in", dpi = 150, bg = "white"
    )
  }
  PEcAn.logger::logger.info("county stock: ", pft_i, "/", mo_i)
}


# section 2 - county stock faceted comparison.
PEcAn.logger::logger.info("faceted county stock plots")

for (row_i in seq_len(nrow(combos))) {
  pft_i <- combos$pft[row_i]
  mo_i <- combos$model_output[row_i]

  lims <- county_stock_limits |>
    dplyr::filter(pft == pft_i, model_output == mo_i)

  dat_facet <- co_stocks |>
    dplyr::filter(pft == pft_i, model_output == mo_i) |>
    dplyr::mutate(scenario_label = factor(
      format_scenario_label(as.character(scenario)),
      levels = format_scenario_label(scenario_levels)
    ))

  if (nrow(dat_facet) == 0) next

  p_facet <- ggplot2::ggplot(dat_facet, ggplot2::aes(geometry = geom, fill = mean_total_per_county)) +
    ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey60", linewidth = 0.2) +
    ggplot2::geom_sf(linewidth = 0.15) +
    ggplot2::scale_fill_viridis_c(
      option = "viridis", na.value = "white",
      limits = c(lims$lo, lims$hi),
      labels = scales::label_comma()
    ) +
    ggplot2::facet_wrap(~scenario_label, ncol = 3) +
    theme_ccmmf_map(base_size = 12) +
    ggplot2::labs(
      title = paste("County Total:", format_variable_label(mo_i), "--", pft_i),
      subtitle = "All scenarios on consistent scale",
      fill = format_unit_label(mo_i, "county_total")
    )

  ggsave_optimized(
    filename = here::here("figures", paste0(
      "facet_county_", safe_key(pft_i), "_", mo_i, "_stock_comparison.webp"
    )),
    plot = p_facet, width = 18, height = 12, units = "in", dpi = 150, bg = "white"
  )
  PEcAn.logger::logger.info("faceted county stock: ", pft_i, "/", mo_i)
}


# section 3 - county-level scenario minus baseline diffs.
non_baseline <- setdiff(as.character(scenario_levels), "baseline")

if (length(non_baseline) > 0) {
  PEcAn.logger::logger.info("county diff maps")

  baseline_county <- county_summaries |>
    dplyr::filter(as.character(scenario) == "baseline") |>
    dplyr::select(model_output, pft, county,
      baseline_total = mean_total_per_county,
      baseline_density = mean_density_per_ha
    )

  county_diffs <- county_summaries |>
    dplyr::filter(as.character(scenario) != "baseline") |>
    dplyr::inner_join(baseline_county, by = c("model_output", "pft", "county")) |>
    dplyr::mutate(
      diff_total = mean_total_per_county - baseline_total,
      diff_density = mean_density_per_ha - baseline_density
    ) |>
    dplyr::right_join(county_boundaries, by = "county") |>
    dplyr::filter(!is.na(scenario), !is.na(model_output), !is.na(pft))

  # diverging limits symmetric around 0.
  diff_county_limits <- county_diffs |>
    sf::st_drop_geometry() |>
    dplyr::group_by(pft, model_output) |>
    dplyr::summarize(
      max_abs = max(abs(diff_total), na.rm = TRUE),
      .groups = "drop"
    )

  # statewide pct change per (scenario, pft, model_output) for facet labels.
  statewide_pct <- county_diffs |>
    sf::st_drop_geometry() |>
    dplyr::group_by(scenario, pft, model_output) |>
    dplyr::summarize(
      total_scenario = sum(mean_total_per_county, na.rm = TRUE),
      total_baseline = sum(baseline_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pct_change = (total_scenario - total_baseline) / total_baseline * 100,
      pct_label = paste0(ifelse(pct_change >= 0, "+", ""),
                         round(pct_change, 1), "%")
    )

  # individual diff maps.
  for (row_i in seq_len(nrow(combos))) {
    pft_i <- combos$pft[row_i]
    mo_i <- combos$model_output[row_i]

    dlims <- diff_county_limits |>
      dplyr::filter(pft == pft_i, model_output == mo_i)
    if (nrow(dlims) == 0 || is.na(dlims$max_abs)) next

    for (scn in non_baseline) {
      dat_diff <- dplyr::filter(
        county_diffs,
        as.character(scenario) == scn,
        pft == pft_i,
        model_output == mo_i
      )
      if (nrow(dat_diff) == 0) next

      pct_row <- statewide_pct |>
        dplyr::filter(as.character(scenario) == scn, pft == pft_i, model_output == mo_i)
      pct_txt <- if (nrow(pct_row) == 1) paste0(" (", pct_row$pct_label, ")") else ""

      p_diff <- ggplot2::ggplot(dat_diff, ggplot2::aes(geometry = geom, fill = diff_total)) +
        ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey60") +
        ggplot2::geom_sf() +
        ggplot2::scale_fill_gradient2(
          low = "#542788", mid = "#F7F7F7", high = "#E66101",
          midpoint = 0, na.value = "white",
          limits = c(-dlims$max_abs, dlims$max_abs),
          labels = scales::label_comma()
        ) +
        theme_ccmmf_map() +
        ggplot2::labs(
          title = paste(
            format_variable_label(mo_i), ":",
            format_scenario_label(scn), "- Baseline", pct_txt
          ),
          subtitle = paste("PFT:", pft_i, " -- County-level total difference"),
          fill = format_unit_label(mo_i, "diff_total")
        )

      ggsave_optimized(
        filename = here::here("figures", paste0(
          "diff_county_", safe_key(scn), "_", safe_key(pft_i), "_", mo_i, "_vs_baseline.webp"
        )),
        plot = p_diff, width = 10, height = 8, units = "in", dpi = 150, bg = "white"
      )
    }
    PEcAn.logger::logger.info("county diff: ", pft_i, "/", mo_i)
  }

  # faceted diff maps.
  PEcAn.logger::logger.info("faceted county diff plots")

  for (row_i in seq_len(nrow(combos))) {
    pft_i <- combos$pft[row_i]
    mo_i <- combos$model_output[row_i]

    dlims <- diff_county_limits |>
      dplyr::filter(pft == pft_i, model_output == mo_i)
    if (nrow(dlims) == 0 || is.na(dlims$max_abs)) next

    dat_facet_diff <- county_diffs |>
      dplyr::filter(pft == pft_i, model_output == mo_i)

    # pct in facet labels.
    pct_lookup <- statewide_pct |>
      dplyr::filter(pft == pft_i, model_output == mo_i) |>
      dplyr::mutate(
        scenario_label = paste0(
          format_scenario_label(as.character(scenario)), " (", pct_label, ")"
        )
      ) |>
      dplyr::select(scenario, scenario_label)

    dat_facet_diff <- dat_facet_diff |>
      dplyr::left_join(sf::st_drop_geometry(pct_lookup), by = "scenario") |>
      dplyr::mutate(scenario_label = factor(
        scenario_label,
        levels = pct_lookup$scenario_label
      ))

    if (nrow(dat_facet_diff) == 0) next

    p_fdiff <- ggplot2::ggplot(dat_facet_diff, ggplot2::aes(geometry = geom, fill = diff_total)) +
      ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey60", linewidth = 0.2) +
      ggplot2::geom_sf(linewidth = 0.15) +
      ggplot2::scale_fill_gradient2(
        low = "#542788", mid = "#F7F7F7", high = "#E66101",
        midpoint = 0, na.value = "white",
        limits = c(-dlims$max_abs, dlims$max_abs),
        labels = scales::label_comma()
      ) +
      ggplot2::facet_wrap(~scenario_label, ncol = 3) +
      theme_ccmmf_map(base_size = 12) +
      ggplot2::labs(
        title = paste(
          format_variable_label(mo_i),
          ": Difference from Baseline (County Total)"
        ),
        subtitle = paste("PFT:", pft_i, " -- Difference from baseline"),
        fill = format_unit_label(mo_i, "diff_total")
      )

    ggsave_optimized(
      filename = here::here("figures", paste0(
        "facet_diff_county_", safe_key(pft_i), "_", mo_i, "_vs_baseline.webp"
      )),
      plot = p_fdiff, width = 18, height = 12, units = "in", dpi = 150, bg = "white"
    )
    PEcAn.logger::logger.info("faceted county diff: ", pft_i, "/", mo_i)
  }

  # baseline + diff panels, patchwork.
  if (requireNamespace("patchwork", quietly = TRUE)) {
    PEcAn.logger::logger.info("baseline + diffs mixed panels")

    for (row_i in seq_len(nrow(combos))) {
      pft_i <- combos$pft[row_i]
      mo_i <- combos$model_output[row_i]

      lims <- county_stock_limits |>
        dplyr::filter(pft == pft_i, model_output == mo_i)
      dlims <- diff_county_limits |>
        dplyr::filter(pft == pft_i, model_output == mo_i)
      if (nrow(dlims) == 0 || is.na(dlims$max_abs)) next

      unit_total <- format_unit_label(mo_i, "county_total")
      unit_diff <- format_unit_label(mo_i, "diff_total")

      # baseline panel.
      dat_base <- dplyr::filter(
        co_stocks,
        as.character(scenario) == "baseline", pft == pft_i, model_output == mo_i
      )
      p_base <- ggplot2::ggplot(dat_base,
                                ggplot2::aes(geometry = geom, fill = mean_total_per_county)) +
        ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey60") +
        ggplot2::geom_sf() +
        ggplot2::scale_fill_viridis_c(
          option = "viridis", na.value = "white",
          limits = c(lims$lo, lims$hi),
          labels = scales::label_comma()
        ) +
        theme_ccmmf_map(base_size = 11) +
        ggplot2::labs(title = "Baseline (absolute)", fill = unit_total)

      # diff panels.
      diff_plots <- list()
      for (scn in non_baseline) {
        dat_d <- dplyr::filter(
          county_diffs,
          as.character(scenario) == scn, pft == pft_i, model_output == mo_i
        )
        if (nrow(dat_d) == 0) next

        pct_row <- statewide_pct |>
          dplyr::filter(as.character(scenario) == scn, pft == pft_i, model_output == mo_i)
        pct_txt <- if (nrow(pct_row) == 1) paste0(" (", pct_row$pct_label, ")") else ""

        diff_plots[[scn]] <- ggplot2::ggplot(dat_d,
                                             ggplot2::aes(geometry = geom, fill = diff_total)) +
          ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey60") +
          ggplot2::geom_sf() +
          ggplot2::scale_fill_gradient2(
            low = "#542788", mid = "#F7F7F7", high = "#E66101",
            midpoint = 0, na.value = "white",
            limits = c(-dlims$max_abs, dlims$max_abs),
            labels = scales::label_comma()
          ) +
          theme_ccmmf_map(base_size = 11) +
          ggplot2::labs(
            title = paste0(format_scenario_label(scn), " - Baseline", pct_txt),
            fill = unit_diff
          )
      }

      all_plots <- c(list(p_base), diff_plots)
      p_mixed <- patchwork::wrap_plots(all_plots, ncol = 3) +
        patchwork::plot_annotation(
          title = paste(format_variable_label(mo_i), "--", pft_i),
          subtitle = "Panel 1: Baseline (absolute) | Panels 2-6: Scenario minus Baseline",
          theme = ggplot2::theme(
            plot.title = ggplot2::element_text(size = 16, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 13, color = "grey30")
          )
        )

      ggsave_optimized(
        filename = here::here("figures", paste0(
          "mixed_county_", safe_key(pft_i), "_", mo_i, "_baseline_plus_diffs.webp"
        )),
        plot = p_mixed, width = 20, height = 14, units = "in", dpi = 150, bg = "white"
      )
      PEcAn.logger::logger.info("mixed panel: ", pft_i, "/", mo_i)
    }
  } else {
    PEcAn.logger::logger.info(
      "patchwork not available; skipping mixed panels. ",
      "install with: pak::pak('patchwork')"
    )
  }
} else {
  PEcAn.logger::logger.info("only baseline scenario; skipping county diff maps")
}


# section 4 - field-level density maps.
PEcAn.logger::logger.info("field density maps")

# field-level predictions.
dp <- vroom::vroom(
  file.path(model_outdir, "downscaled_preds.csv"),
  col_types = readr::cols(
    scenario = readr::col_character(),
    site_id = readr::col_character(),
    pft = readr::col_character(),
    ensemble = readr::col_double(),
    density_per_ha = readr::col_double(),
    total_per_field = readr::col_double(),
    area_ha = readr::col_double(),
    county = readr::col_character(),
    model_output = readr::col_character()
  )
)

if (!"scenario" %in% names(dp)) {
  dp <- dp |> dplyr::mutate(scenario = "baseline")
}
dp <- dp |>
  dplyr::mutate(scenario = factor(scenario, levels = scenario_levels))

# element mass to gas mass.
dp <- dp |>
  dplyr::mutate(
    .gas_factor = dplyr::case_match(model_output,
      "N2O_flux" ~ 44 / 28,
      "CH4_flux" ~ 16 / 12,
      .default = 1
    ),
    density_per_ha = density_per_ha * .gas_factor,
    total_per_field = total_per_field * .gas_factor
  ) |>
  dplyr::select(-.gas_factor)

# field centroids.
ca_fields <- sf::st_read(file.path(data_dir, "ca_fields.gpkg"))
field_centroids <- ca_fields |>
  sf::st_centroid() |>
  dplyr::select(site_id, geom)

# mean density per field across ensembles.
field_mean_density <- dp |>
  dplyr::group_by(scenario, model_output, pft, site_id) |>
  dplyr::summarize(
    mean_density_per_ha = mean(density_per_ha, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::inner_join(
    sf::st_drop_geometry(field_centroids) |> dplyr::select(site_id),
    by = "site_id"
  )

# join with geometry.
field_mean_density_sf <- field_mean_density |>
  dplyr::inner_join(field_centroids, by = "site_id") |>
  sf::st_as_sf()

# per-(pft, model_output) density limits.
field_density_limits <- field_mean_density |>
  dplyr::group_by(pft, model_output) |>
  dplyr::summarize(
    lo = min(mean_density_per_ha, na.rm = TRUE),
    hi = max(mean_density_per_ha, na.rm = TRUE),
    .groups = "drop"
  )

combos_field <- field_mean_density |>
  dplyr::distinct(pft, model_output) |>
  dplyr::arrange(pft, model_output)

PEcAn.logger::logger.info(nrow(combos_field) * length(scenario_levels), " field density maps")

# individual field density maps.
for (row_i in seq_len(nrow(combos_field))) {
  pft_i <- combos_field$pft[row_i]
  mo_i <- combos_field$model_output[row_i]

  lims <- field_density_limits |>
    dplyr::filter(pft == pft_i, model_output == mo_i)

  for (scn in scenario_levels) {
    dat <- dplyr::filter(
      field_mean_density_sf,
      as.character(scenario) == scn,
      pft == pft_i,
      model_output == mo_i
    )
    if (nrow(dat) == 0) next

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey70", linewidth = 0.3) +
      ggplot2::geom_sf(
        data = dat, ggplot2::aes(fill = mean_density_per_ha),
        shape = 21, size = 0.5, stroke = 0.08, color = "grey40", alpha = 0.7
      ) +
      ggplot2::scale_fill_viridis_c(
        option = "viridis", na.value = "white",
        limits = c(lims$lo, lims$hi),
        labels = scales::label_comma()
      ) +
      theme_ccmmf_map() +
      ggplot2::labs(
        title = paste("Field Density:", format_variable_label(mo_i)),
        subtitle = paste(
          "Scenario:", format_scenario_label(scn),
          " --  PFT:", pft_i
        ),
        fill = format_unit_label(mo_i, "density")
      )

    ggsave_optimized(
      filename = here::here("figures", paste0(
        "field_", safe_key(scn), "_", safe_key(pft_i), "_", mo_i, "_carbon_density.webp"
      )),
      plot = p, width = 10, height = 8, units = "in", dpi = 150, bg = "white"
    )
  }
  PEcAn.logger::logger.info("field density: ", pft_i, "/", mo_i)
}


# section 5 - field density faceted comparison.
PEcAn.logger::logger.info("faceted field density plots")

for (row_i in seq_len(nrow(combos_field))) {
  pft_i <- combos_field$pft[row_i]
  mo_i <- combos_field$model_output[row_i]

  lims <- field_density_limits |>
    dplyr::filter(pft == pft_i, model_output == mo_i)

  dat_facet <- field_mean_density_sf |>
    dplyr::filter(pft == pft_i, model_output == mo_i) |>
    dplyr::mutate(scenario_label = factor(
      format_scenario_label(as.character(scenario)),
      levels = format_scenario_label(scenario_levels)
    ))

  if (nrow(dat_facet) == 0) next

  # heavy: ~264k points x 6 panels.
  p_facet <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey70", linewidth = 0.2) +
    ggplot2::geom_sf(
      data = dat_facet,
      ggplot2::aes(fill = mean_density_per_ha),
      shape = 21, size = 0.2, stroke = 0.03, color = "grey40", alpha = 0.6
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "viridis", na.value = "white",
      limits = c(lims$lo, lims$hi),
      labels = scales::label_comma()
    ) +
    ggplot2::facet_wrap(~scenario_label, ncol = 3) +
    theme_ccmmf_map(base_size = 12) +
    ggplot2::labs(
      title = paste("Field Density:", format_variable_label(mo_i), "--", pft_i),
      subtitle = "All scenarios on consistent scale",
      fill = format_unit_label(mo_i, "density")
    )

  ggsave_optimized(
    filename = here::here("figures", paste0(
      "facet_field_", safe_key(pft_i), "_", mo_i, "_density_comparison.webp"
    )),
    plot = p_facet, width = 18, height = 12, units = "in", dpi = 150, bg = "white"
  )
  PEcAn.logger::logger.info("faceted field density: ", pft_i, "/", mo_i)
}


# section 6 - field-level scenario minus baseline diffs.
if (length(non_baseline) > 0) {
  PEcAn.logger::logger.info("field diff maps")

  baseline_field <- field_mean_density |>
    dplyr::filter(as.character(scenario) == "baseline") |>
    dplyr::select(model_output, pft, site_id, baseline_density = mean_density_per_ha)

  field_diffs <- field_mean_density |>
    dplyr::filter(as.character(scenario) != "baseline") |>
    dplyr::inner_join(baseline_field, by = c("model_output", "pft", "site_id")) |>
    dplyr::mutate(diff_density = mean_density_per_ha - baseline_density) |>
    dplyr::inner_join(field_centroids, by = "site_id") |>
    sf::st_as_sf()

  # diverging limits symmetric around 0.
  field_diff_limits <- field_diffs |>
    sf::st_drop_geometry() |>
    dplyr::group_by(pft, model_output) |>
    dplyr::summarize(
      max_abs = max(abs(diff_density), na.rm = TRUE),
      .groups = "drop"
    )

  # individual field diff maps.
  for (row_i in seq_len(nrow(combos_field))) {
    pft_i <- combos_field$pft[row_i]
    mo_i <- combos_field$model_output[row_i]

    dlims <- field_diff_limits |>
      dplyr::filter(pft == pft_i, model_output == mo_i)
    if (nrow(dlims) == 0 || is.na(dlims$max_abs)) next

    for (scn in non_baseline) {
      dat <- dplyr::filter(
        field_diffs,
        as.character(scenario) == scn,
        pft == pft_i,
        model_output == mo_i
      )
      if (nrow(dat) == 0) next

      p <- ggplot2::ggplot() +
        ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey70", linewidth = 0.3) +
        ggplot2::geom_sf(
          data = dat,
          ggplot2::aes(fill = diff_density),
          shape = 21, size = 0.5, stroke = 0.08, color = "grey40", alpha = 0.7
        ) +
        ggplot2::scale_fill_gradient2(
          low = "#542788", mid = "#F7F7F7", high = "#E66101",
          midpoint = 0, na.value = "white",
          limits = c(-dlims$max_abs, dlims$max_abs),
          labels = scales::label_comma()
        ) +
        theme_ccmmf_map() +
        ggplot2::labs(
          title = paste(
            format_variable_label(mo_i), ":",
            format_scenario_label(scn), "- Baseline"
          ),
          subtitle = paste("PFT:", pft_i, " -- Field-level density difference"),
          fill = format_unit_label(mo_i, "diff_density")
        )

      ggsave_optimized(
        filename = here::here("figures", paste0(
          "diff_field_", safe_key(scn), "_", safe_key(pft_i), "_", mo_i, "_vs_baseline.webp"
        )),
        plot = p, width = 10, height = 8, units = "in", dpi = 150, bg = "white"
      )
    }
    PEcAn.logger::logger.info("field diff: ", pft_i, "/", mo_i)
  }

  # faceted field diff maps.
  PEcAn.logger::logger.info("faceted field diff plots")

  for (row_i in seq_len(nrow(combos_field))) {
    pft_i <- combos_field$pft[row_i]
    mo_i <- combos_field$model_output[row_i]

    dlims <- field_diff_limits |>
      dplyr::filter(pft == pft_i, model_output == mo_i)
    if (nrow(dlims) == 0 || is.na(dlims$max_abs)) next

    dat_facet_diff <- field_diffs |>
      dplyr::filter(pft == pft_i, model_output == mo_i)

    # reuse pct labels from county-level if available.
    if (exists("statewide_pct")) {
      pct_lookup <- statewide_pct |>
        dplyr::filter(pft == pft_i, model_output == mo_i) |>
        dplyr::mutate(
          scenario_label = paste0(
            format_scenario_label(as.character(scenario)), " (", pct_label, ")"
          )
        ) |>
        dplyr::select(scenario, scenario_label)

      dat_facet_diff <- dat_facet_diff |>
        dplyr::left_join(sf::st_drop_geometry(pct_lookup), by = "scenario") |>
        dplyr::mutate(scenario_label = factor(
          scenario_label,
          levels = pct_lookup$scenario_label
        ))
    } else {
      dat_facet_diff <- dat_facet_diff |>
        dplyr::mutate(scenario_label = factor(
          format_scenario_label(as.character(scenario)),
          levels = format_scenario_label(non_baseline)
        ))
    }

    if (nrow(dat_facet_diff) == 0) next

    p_fdiff <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey70", linewidth = 0.2) +
      ggplot2::geom_sf(
        data = dat_facet_diff,
        ggplot2::aes(fill = diff_density),
        shape = 21, size = 0.2, stroke = 0.03, color = "grey40", alpha = 0.6
      ) +
      ggplot2::scale_fill_gradient2(
        low = "#542788", mid = "#F7F7F7", high = "#E66101",
        midpoint = 0, na.value = "white",
        limits = c(-dlims$max_abs, dlims$max_abs),
        labels = scales::label_comma()
      ) +
      ggplot2::facet_wrap(~scenario_label, ncol = 3) +
      theme_ccmmf_map(base_size = 12) +
      ggplot2::labs(
        title = paste(
          format_variable_label(mo_i),
          ": Difference from Baseline (Field Density)"
        ),
        subtitle = paste("PFT:", pft_i, " -- Difference from baseline"),
        fill = format_unit_label(mo_i, "diff_density")
      )

    ggsave_optimized(
      filename = here::here("figures", paste0(
        "facet_diff_field_", safe_key(pft_i), "_", mo_i, "_vs_baseline.webp"
      )),
      plot = p_fdiff, width = 18, height = 12, units = "in", dpi = 150, bg = "white"
    )
    PEcAn.logger::logger.info("faceted field diff: ", pft_i, "/", mo_i)
  }
} else {
  PEcAn.logger::logger.info("only baseline scenario; skipping field diff maps")
}


# section 7 - pft diff maps (woody+annual minus woody). multi-pft only.
mix <- dp |>
  dplyr::filter(pft == "woody + annual")
wood <- dp |>
  dplyr::filter(pft == "woody perennial crop")

if (nrow(mix) > 0 && nrow(wood) > 0) {
  PEcAn.logger::logger.info("pft diff maps")

  mix_sel <- mix |>
    dplyr::select(site_id, ensemble, model_output, county, area_ha_mix = area_ha, total_mix = total_per_field)
  wood_sel <- wood |>
    dplyr::select(site_id, ensemble, model_output, county, area_ha_woody = area_ha, total_woody = total_per_field)

  # check for duplicates.
  mix_dups <- mix_sel |>
    dplyr::count(site_id, ensemble, model_output, county) |>
    dplyr::filter(n > 1)
  wood_dups <- wood_sel |>
    dplyr::count(site_id, ensemble, model_output, county) |>
    dplyr::filter(n > 1)

  if (nrow(mix_dups) > 0 || nrow(wood_dups) > 0) {
    PEcAn.logger::logger.warn(
      "duplicate keys (mix=", nrow(mix_dups), ", wood=", nrow(wood_dups), "); skipping pft diff maps"
    )
  } else {
    diff_county <- mix_sel |>
      dplyr::inner_join(wood_sel, by = c("site_id", "ensemble", "model_output", "county")) |>
      dplyr::mutate(
        diff_total = total_mix - total_woody,
        area_ha = dplyr::coalesce(area_ha_woody, area_ha_mix)
      ) |>
      dplyr::group_by(county, model_output, ensemble) |>
      dplyr::summarise(diff_total = sum(diff_total), total_ha = sum(area_ha), .groups = "drop") |>
      dplyr::mutate(
        diff_density_per_ha = diff_total / total_ha
      ) |>
      dplyr::group_by(county, model_output) |>
      dplyr::summarise(
        mean_diff_total = mean(diff_total),
        mean_diff_density_per_ha = mean(diff_density_per_ha),
        .groups = "drop"
      ) |>
      dplyr::right_join(county_boundaries, by = "county") |>
      # Mg -> Gg for stocks, kg gas -> Gg gas for fluxes.
      dplyr::mutate(
        mean_diff_total = mean_diff_total / dplyr::case_match(model_output,
          c("TotSoilCarb", "AGB") ~ 1e3,
          c("N2O_flux", "CH4_flux") ~ 1e6,
          .default = 1
        )
      )

    for (pool in unique(stats::na.omit(diff_county$model_output))) {
      dat_pool <- dplyr::filter(diff_county, model_output == pool)

      # diverging palette.
      p_stock <- ggplot2::ggplot(dat_pool, ggplot2::aes(geometry = geom, fill = mean_diff_total)) +
        ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey60") +
        ggplot2::geom_sf() +
        ggplot2::scale_fill_gradient2(
          low = "#542788", mid = "#F7F7F7", high = "#E66101",
          midpoint = 0, na.value = "white",
          labels = scales::label_comma()
        ) +
        theme_ccmmf_map() +
        ggplot2::labs(
          title = "Difference in Total: (woody + annual) - (woody)",
          subtitle = format_variable_label(pool),
          fill = format_unit_label(pool, "diff_total")
        )

      ggsave_optimized(
        filename = here::here("figures", paste0(
          "county_diff_woody_plus_annual_minus_woody_", pool, "_carbon_stock.webp"
        )),
        plot = p_stock, width = 10, height = 8, units = "in", dpi = 150, bg = "white"
      )
    }
  }
} else {
  PEcAn.logger::logger.info("woody+annual or woody perennial not present; skipping pft diff maps")
}


# section 8 - delta maps (start to end change).
delta_csv <- file.path(model_outdir, "downscaled_deltas.csv")
if (file.exists(delta_csv)) {
  PEcAn.logger::logger.info("delta maps")

  deltas <- vroom::vroom(
    delta_csv,
    col_types = readr::cols(
      scenario = readr::col_character(),
      site_id = readr::col_character(),
      pft = readr::col_character(),
      ensemble = readr::col_double(),
      delta_density_per_ha = readr::col_double(),
      delta_total = readr::col_double(),
      area_ha = readr::col_double(),
      county = readr::col_character(),
      model_output = readr::col_character()
    )
  )

  if (!"scenario" %in% names(deltas)) {
    deltas <- deltas |> dplyr::mutate(scenario = "baseline")
  }

  # element mass to gas mass.
  deltas <- deltas |>
    dplyr::mutate(
      .gas_factor = dplyr::case_match(model_output,
        "N2O_flux" ~ 44 / 28,
        "CH4_flux" ~ 16 / 12,
        .default = 1
      ),
      delta_density_per_ha = delta_density_per_ha * .gas_factor,
      delta_total = delta_total * .gas_factor
    ) |>
    dplyr::select(-.gas_factor)

  delta_county <- deltas |>
    dplyr::group_by(scenario, model_output, pft, county, ensemble) |>
    dplyr::summarize(total_delta = sum(delta_total), total_ha = sum(area_ha), .groups = "drop") |>
    dplyr::mutate(
      delta_density_per_ha = total_delta / total_ha
    ) |>
    dplyr::group_by(scenario, model_output, pft, county) |>
    dplyr::summarize(
      mean_delta_density_per_ha = mean(delta_density_per_ha),
      mean_delta_total = mean(total_delta),
      .groups = "drop"
    ) |>
    dplyr::right_join(county_boundaries, by = "county") |>
    # Mg -> Gg for stocks, kg gas -> Gg gas for fluxes.
    dplyr::mutate(
      mean_delta_total = mean_delta_total / dplyr::case_match(model_output,
        c("TotSoilCarb", "AGB") ~ 1e3,
        c("N2O_flux", "CH4_flux") ~ 1e6,
        .default = 1
      )
    )

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

      # diverging palette.
      p_stk <- ggplot2::ggplot(datp, ggplot2::aes(geometry = geom, fill = mean_delta_total)) +
        ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey60") +
        ggplot2::geom_sf() +
        ggplot2::scale_fill_gradient2(
          low = "#542788", mid = "#F7F7F7", high = "#E66101",
          midpoint = 0, na.value = "white",
          labels = scales::label_comma()
        ) +
        theme_ccmmf_map() +
        ggplot2::labs(
          title = paste("Delta (start -> end):", format_variable_label(model_output)),
          subtitle = paste(
            "Scenario:", format_scenario_label(scenario),
            " --  PFT:", pft
          ),
          fill = format_unit_label(model_output, "diff_total")
        )

      ggsave_optimized(
        filename = here::here("figures", paste0(
          "county_delta_", safe_key(scenario), "_", safe_key(pft), "_", model_output, "_carbon_stock.webp"
        )),
        plot = p_stk, width = 10, height = 8, units = "in", dpi = 150, bg = "white"
      )
      PEcAn.logger::logger.info("delta map: ", scenario, "/", pft, "/", model_output)
    }
  )
} else {
  PEcAn.logger::logger.warn("downscaled_deltas.csv not found; skipping delta maps")
}


# section 9 - county-level CO2e maps. GWP-100 AR4. negative = climate benefit.
PEcAn.logger::logger.info("CO2e maps")

co2e_gwp <- "AR4"
sim_years <- 8 # 2016-2024.

# pivot to one row per (scenario, county). county_summaries already in Gg of gas.
co2e_wide <- county_summaries |>
  dplyr::filter(pft == "annual crop") |>
  dplyr::select(scenario, county, model_output, mean_total_per_county) |>
  tidyr::pivot_wider(names_from = model_output, values_from = mean_total_per_county)

co2e_baseline <- co2e_wide |>
  dplyr::filter(as.character(scenario) == "baseline") |>
  dplyr::select(county,
    bl_soc = TotSoilCarb,
    bl_n2o = N2O_flux,
    bl_ch4 = CH4_flux
  )

co2e_county <- co2e_wide |>
  dplyr::filter(as.character(scenario) != "baseline") |>
  dplyr::inner_join(co2e_baseline, by = "county") |>
  dplyr::mutate(
    # SOC: annualized stock change.
    co2e_soc = PEcAn.data.land::to_co2e(
      delta_soc = (TotSoilCarb - bl_soc) / sim_years, gwp = co2e_gwp
    ),
    # N2O flux delta.
    co2e_n2o = PEcAn.data.land::to_co2e(
      n2o = N2O_flux - bl_n2o, gwp = co2e_gwp
    ),
    # CH4 flux delta.
    co2e_ch4 = PEcAn.data.land::to_co2e(
      ch4 = CH4_flux - bl_ch4, gwp = co2e_gwp
    ),
    co2e_net = co2e_soc + co2e_n2o + co2e_ch4
  )

# skip CH4, magnitudes below 1.2 Gg CO2e/yr.
co2e_components <- c("co2e_soc", "co2e_n2o", "co2e_net")
co2e_labels <- c(
  co2e_soc = "SOC",
  co2e_n2o = "N\u2082O",
  co2e_net = "Net Total"
)

co2e_long <- co2e_county |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(co2e_components),
    names_to = "component",
    values_to = "co2e_gg"
  ) |>
  dplyr::mutate(
    component_label = co2e_labels[component],
    component_label = factor(component_label, levels = co2e_labels)
  ) |>
  dplyr::right_join(county_boundaries, by = "county") |>
  dplyr::filter(!is.na(scenario), !is.na(component))

# symmetric diverging limits per component.
co2e_limits <- co2e_long |>
  sf::st_drop_geometry() |>
  dplyr::group_by(component) |>
  dplyr::summarize(max_abs = max(abs(co2e_gg), na.rm = TRUE), .groups = "drop")

# individual CO2e maps per scenario x component.
non_baseline_co2e <- setdiff(levels(co2e_county$scenario), "baseline")

for (comp_i in co2e_components) {
  clims <- co2e_limits |> dplyr::filter(component == comp_i)
  if (nrow(clims) == 0 || is.na(clims$max_abs)) next

  for (scn in non_baseline_co2e) {
    dat <- dplyr::filter(co2e_long,
      as.character(scenario) == scn,
      component == comp_i
    )
    if (nrow(dat) == 0) next

    p_co2e <- ggplot2::ggplot(dat, ggplot2::aes(geometry = geom, fill = co2e_gg)) +
      ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey60") +
      ggplot2::geom_sf() +
      ggplot2::scale_fill_gradient2(
        low = "#1B7837", mid = "#F7F7F7", high = "#762A83",
        midpoint = 0, na.value = "white",
        limits = c(-clims$max_abs, clims$max_abs),
        labels = scales::label_comma()
      ) +
      theme_ccmmf_map() +
      ggplot2::labs(
        title = paste0(
          "CO\u2082e: ", co2e_labels[comp_i], " -- ",
          format_scenario_label(scn), " vs Baseline"
        ),
        subtitle = paste0(
          "GWP\u2081\u2080\u2080 AR4 | Negative = climate benefit | ",
          "annual crop"
        ),
        fill = "Gg CO\u2082e yr\u207b\u00b9"
      )

    ggsave_optimized(
      filename = here::here("figures", paste0(
        "co2e_county_", safe_key(scn), "_", comp_i, ".webp"
      )),
      plot = p_co2e, width = 10, height = 8, units = "in", dpi = 150, bg = "white"
    )
  }
  PEcAn.logger::logger.info("CO2e maps: ", comp_i)
}

# faceted CO2e maps per component, faceted by scenario.
for (comp_i in co2e_components) {
  clims <- co2e_limits |> dplyr::filter(component == comp_i)
  if (nrow(clims) == 0 || is.na(clims$max_abs)) next

  dat_facet <- dplyr::filter(co2e_long, component == comp_i)
  if (nrow(dat_facet) == 0) next

  dat_facet <- dat_facet |>
    dplyr::mutate(
      scenario_label = format_scenario_label(as.character(scenario)),
      scenario_label = factor(scenario_label,
        levels = purrr::map_chr(non_baseline_co2e, format_scenario_label)
      )
    )

  p_facet <- ggplot2::ggplot(dat_facet, ggplot2::aes(geometry = geom, fill = co2e_gg)) +
    ggplot2::geom_sf(data = county_boundaries, fill = "white", color = "grey60", linewidth = 0.2) +
    ggplot2::geom_sf(linewidth = 0.1) +
    ggplot2::facet_wrap(~scenario_label, ncol = 3) +
    ggplot2::scale_fill_gradient2(
      low = "#1B7837", mid = "#F7F7F7", high = "#762A83",
      midpoint = 0, na.value = "white",
      limits = c(-clims$max_abs, clims$max_abs),
      labels = scales::label_comma()
    ) +
    theme_ccmmf_map(base_size = 12) +
    ggplot2::labs(
      title = paste0("CO\u2082e from ", co2e_labels[comp_i], ": Scenario vs Baseline"),
      subtitle = "GWP\u2081\u2080\u2080 AR4 | Negative (green) = climate benefit | annual crop",
      fill = "Gg CO\u2082e yr\u207b\u00b9"
    )

  ggsave_optimized(
    filename = here::here("figures", paste0(
      "facet_co2e_county_", comp_i, ".webp"
    )),
    plot = p_facet, width = 18, height = 12, units = "in", dpi = 150, bg = "white"
  )
  PEcAn.logger::logger.info("faceted CO2e: ", comp_i)
}

PEcAn.logger::logger.info("done")
