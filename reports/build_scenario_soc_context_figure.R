#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4L) {
  stop(paste(
    "Usage: Rscript reports/build_scenario_soc_context_figure.R",
    "INVENTORY_STATE_CSV INVENTORY_COUNTY_DELTAS_CSV",
    "SCENARIO_STATE_CSV OUTPUT_PNG"
  ))
}
inventory_state_path <- normalizePath(args[[1]], mustWork = TRUE)
inventory_delta_path <- normalizePath(args[[2]], mustWork = TRUE)
scenario_state_path <- normalizePath(args[[3]], mustWork = TRUE)
output_path <- args[[4]]

crop_labels <- c(`annual crop` = "Annual crops",
                 `woody perennial crop` = "Woody perennial crops",
                 `All crops` = "All crops")
crop_levels <- unname(crop_labels)

inventory_stock <- read.csv(inventory_state_path) |>
  filter(scenario == "baseline", model_output == "TotSoilCarb") |>
  transmute(crop = crop_labels[pft], member = ensemble,
            value = total_per_state / 1e6)
inventory_change <- read.csv(inventory_delta_path) |>
  filter(scenario == "baseline", model_output == "TotSoilCarb") |>
  summarise(value = sum(delta_total_county) / 1e6,
            .by = c("pft", "ensemble")) |>
  transmute(crop = crop_labels[pft], member = ensemble, value)

add_all_crops <- function(data) {
  bind_rows(data, data |>
    summarise(value = sum(value), .by = "member") |>
    mutate(crop = "All crops"))
}
inventory_stock <- add_all_crops(inventory_stock)
inventory_change <- add_all_crops(inventory_change)

scenario <- read.csv(scenario_state_path) |>
  filter(configuration == "reference") |>
  transmute(crop = crop_labels[pft], member,
            stock = start_Mg_C / 1e6, change = delta_soc_Tg_C)

stocks <- bind_rows(
  mutate(inventory_stock, period = "Inventory"),
  transmute(scenario, crop, member, value = stock,
            period = "Baseline scenario")
)
changes <- bind_rows(
  mutate(inventory_change, period = "Inventory"),
  transmute(scenario, crop, member, value = change,
            period = "Baseline scenario")
)
prepare <- function(data) {
  data <- data |>
    mutate(crop = factor(crop, levels = crop_levels),
           period = factor(period, levels = c("Inventory", "Baseline scenario")))
  stopifnot(nrow(data) == 120L,
            !anyNA(data[c("crop", "member", "value", "period")]),
            !anyDuplicated(data[c("crop", "period", "member")]),
            all(data |>
              count(crop, period) |>
              pull(n) == 20L))
  data
}
stocks <- prepare(stocks)
changes <- prepare(changes)

summarize_members <- function(data) data |>
  summarise(mean = mean(value), p05 = quantile(value, 0.05),
            p95 = quantile(value, 0.95), .by = c("crop", "period"))

stock_summary <- summarize_members(stocks) |> arrange(period, crop)
change_summary <- summarize_members(changes) |> arrange(period, crop)
stopifnot(
  max(abs(stock_summary$mean - c(113.22, 87.93, 201.15,
                                  124.96, 95.84, 220.80))) < 0.02,
  max(abs(change_summary$mean - c(-18.66, -18.50, -37.16,
                                   -30.52, -14.81, -45.33))) < 0.02
)

palette <- c(Inventory = "#0072B2", `Baseline scenario` = "#D55E00")
context_theme <- theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom", legend.title = element_blank(),
        plot.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))

make_panel <- function(data, summary, title, subtitle, y_label) {
  dodge <- position_dodge(width = 0.62)
  ggplot() +
    geom_hline(yintercept = 0, color = "grey65", linewidth = 0.4) +
    geom_point(data = data,
               aes(crop, value, color = period, group = period),
               position = position_jitterdodge(jitter.width = 0.13,
                                               dodge.width = 0.62,
                                               seed = 1),
               alpha = 0.3, size = 1.5, show.legend = FALSE) +
    geom_errorbar(data = summary,
                  aes(crop, ymin = p05, ymax = p95, color = period,
                      group = period),
                  position = dodge, width = 0.10, linewidth = 0.7) +
    geom_point(data = summary,
               aes(crop, mean, color = period, shape = period, group = period),
               position = dodge, size = 3.3) +
    scale_color_manual(values = palette) +
    scale_shape_manual(values = c(16, 17)) +
    scale_y_continuous(expand = expansion(mult = c(0.08, 0.14))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = y_label) +
    context_theme
}

figure <- (
  make_panel(stocks, stock_summary, "A  Soil-carbon stock",
             "Inventory: Dec. 2023; baseline scenario: Jan. 2024",
             "Soil carbon (Tg C)") /
  make_panel(changes, change_summary, "B  Change within each simulation period",
             "Inventory: 2016–2023; baseline scenario: 2024–2045",
             "Change in soil carbon (Tg C)")
) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, figure, width = 8.8, height = 7.6,
       units = "in", dpi = 180, bg = "white")
message("Wrote ", normalizePath(output_path, mustWork = TRUE))
