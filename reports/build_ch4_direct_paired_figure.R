#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 2L) {
  stop("Usage: Rscript reports/build_ch4_direct_paired_figure.R CONTRASTS_CSV_GZ [OUTPUT_PNG]")
}
source_path <- normalizePath(args[[1]], mustWork = TRUE)
output_path <- if (length(args) == 2L) args[[2]] else file.path(
  "reports", "assets", "carb-results", "scenario-ch4-direct-paired-v2.png"
)

data <- read.csv(gzfile(source_path), check.names = FALSE)
required <- c(
  "site_id", "member", "reference_pft", "management_pft", "year",
  "management_minus_reference_kg_C_ha", "management_minus_reference_kg_CH4_ha"
)
stopifnot(
  all(required %in% names(data)), nrow(data) == 186670L,
  identical(sort(unique(data$member)), 1:20),
  all(data$reference_pft == data$management_pft),
  max(abs(data$management_minus_reference_kg_CH4_ha -
    data$management_minus_reference_kg_C_ha * 16 / 12)) < 1e-12
)

period_breaks <- c(2024, 2029, 2034, 2039, 2044)
period_labels <- c("2025–2029", "2030–2034", "2035–2039", "2040–2044")
member_period <- data |>
  filter(year >= 2025, year <= 2044) |>
  mutate(period = cut(year, breaks = period_breaks, labels = period_labels)) |>
  group_by(site_id, member, period) |>
  summarise(
    kg_CH4_ha_yr = mean(management_minus_reference_kg_CH4_ha),
    years = n(), .groups = "drop"
  ) |>
  group_by(member, period) |>
  summarise(
    kg_CH4_ha_yr = mean(kg_CH4_ha_yr), sites = n(),
    complete_sites = all(years == 5L), .groups = "drop"
  ) |>
  mutate(
    g_CH4_ha_yr = kg_CH4_ha_yr * 1000,
    period = factor(as.character(period), levels = period_labels)
  )
stopifnot(nrow(member_period) == 80L, all(member_period$complete_sites))

summary_table <- member_period |>
  group_by(period) |>
  summarise(
    mean = mean(g_CH4_ha_yr), median = median(g_CH4_ha_yr),
    p05 = quantile(g_CH4_ha_yr, 0.05), p95 = quantile(g_CH4_ha_yr, 0.95),
    members = n(), min_sites = min(sites), max_sites = max(sites),
    .groups = "drop"
  )
print(summary_table)

annual_member <- data |>
  filter(year >= 2025, year <= 2044) |>
  group_by(member, year) |>
  summarise(g_CH4_ha_yr = mean(management_minus_reference_kg_CH4_ha) * 1000,
            sites = n(), .groups = "drop") |>
  group_by(member) |>
  arrange(year, .by_group = TRUE) |>
  mutate(five_year_mean = as.numeric(stats::filter(
    g_CH4_ha_yr, rep(1 / 5, 5), sides = 2
  ))) |>
  ungroup()
stopifnot(nrow(annual_member) == 400L,
          !anyDuplicated(annual_member[c("member", "year")]),
          all(annual_member$sites >= 398L))
annual_period_check <- annual_member |>
  mutate(period = cut(year, breaks = period_breaks, labels = period_labels)) |>
  summarise(annual_mean = mean(g_CH4_ha_yr),
            .by = c("member", "period")) |>
  left_join(select(member_period, member, period, period_mean = g_CH4_ha_yr),
            by = c("member", "period"))
stopifnot(nrow(annual_period_check) == 80L,
          !anyNA(annual_period_check),
          max(abs(annual_period_check$annual_mean -
                    annual_period_check$period_mean)) < 1e-10)

annual_summary <- annual_member |>
  group_by(year) |>
  summarise(
    annual_median = median(g_CH4_ha_yr),
    five_year_median = if (all(is.na(five_year_mean))) NA_real_ else
      median(five_year_mean, na.rm = TRUE),
    .groups = "drop"
  )

figure <- ggplot(annual_summary, aes(x = year)) +
  geom_hline(yintercept = 0, color = "#59646B", linewidth = 0.55) +
  geom_line(aes(y = annual_median), color = "#355C72", alpha = 0.45,
            linewidth = 0.55) +
  geom_point(aes(y = annual_median), color = "#355C72", alpha = 0.65,
             size = 1.8) +
  geom_line(aes(y = five_year_median), color = "#355C72", linewidth = 1.35,
            na.rm = TRUE) +
  scale_x_continuous(limits = c(2025, 2044),
                     breaks = c(2025, 2030, 2035, 2040, 2044)) +
  labs(
    title = "Methane change at matched locations with rice history",
    x = "Year", y = "NBS minus BAU (g CH₄ ha⁻¹ yr⁻¹)",
    caption = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#E3E6E8", linewidth = 0.35),
    plot.title = element_text(face = "bold", size = 17, color = "#172127"),
    axis.title = element_text(color = "#28343B"),
    axis.text = element_text(size = 12, color = "#404B52"),
    legend.position = "none",
    plot.margin = margin(12, 14, 10, 12)
  )

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, figure, width = 8.5, height = 4.6, units = "in", dpi = 180, bg = "white")
message("Wrote ", normalizePath(output_path, mustWork = TRUE))
