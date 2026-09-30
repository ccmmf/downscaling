#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 2L) {
  stop("Usage: Rscript reports/build_n2o_period_figure.R PROJECTION_DATA_DIR [OUTPUT_PNG]")
}
projection_dir <- normalizePath(args[[1]], mustWork = TRUE)
output_path <- if (length(args) == 2L) args[[2]] else file.path(
  "reports", "assets", "carb-results", "scenario-n2o-annual-v2.png"
)

state <- read.csv(file.path(projection_dir, "annual_n_state_members.csv"), check.names = FALSE)
contrast <- read.csv(file.path(projection_dir, "annual_n_contrast_members.csv"), check.names = FALSE)

period_breaks <- c(2024, 2029, 2034, 2039, 2044)
period_labels <- c("2025–2029", "2030–2034", "2035–2039", "2040–2044")
periodize <- function(data) {
  data |>
    filter(pft == "All crops", year >= 2025, year <= 2044) |>
    mutate(period = cut(year, breaks = period_breaks, labels = period_labels))
}
state <- periodize(state)
contrast <- periodize(contrast)
stopifnot(
  identical(sort(unique(state$configuration)), c("management", "reference")),
  identical(unique(contrast$configuration), "management_minus_reference"),
  !anyDuplicated(state[c("configuration", "member", "year")]),
  !anyDuplicated(contrast[c("member", "year")])
)

state_member_period <- state |>
  group_by(configuration, member, period) |>
  summarise(kg_N_ha = mean(kg_N_ha), years = n(), .groups = "drop")
contrast_member_period <- contrast |>
  group_by(member, period) |>
  summarise(kg_N_ha = mean(kg_N_ha), years = n(), .groups = "drop")
stopifnot(
  nrow(state_member_period) == 160L,
  nrow(contrast_member_period) == 80L,
  all(state_member_period$years == 5L), all(contrast_member_period$years == 5L)
)

state_wide <- tidyr::pivot_wider(
  state_member_period, id_cols = c(member, period),
  names_from = configuration, values_from = kg_N_ha
)
contrast_check <- contrast_member_period |>
  left_join(state_wide, by = c("member", "period")) |>
  mutate(error = kg_N_ha - (management - reference))
stopifnot(max(abs(contrast_check$error)) < 1e-10)

rolling_mean <- function(x) as.numeric(stats::filter(x, rep(1 / 5, 5), sides = 2))
summarize_annual <- function(data, groups) {
  data |>
    group_by(across(all_of(groups)), member) |>
    arrange(year, .by_group = TRUE) |>
    mutate(five_year_mean = rolling_mean(kg_N_ha)) |>
    ungroup() |>
    group_by(across(all_of(groups)), year) |>
    summarise(
      annual_median = median(kg_N_ha),
      five_year_median = if (all(is.na(five_year_mean))) NA_real_ else
        median(five_year_mean, na.rm = TRUE),
      .groups = "drop"
    )
}
annual_state <- summarize_annual(state, "configuration") |>
  mutate(scenario = factor(configuration, levels = c("reference", "management"),
                           labels = c("BAU baseline", "NBS management")))
annual_contrast <- summarize_annual(contrast, character())

theme_annual <- theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "#E3E6E8", linewidth = 0.35),
    strip.text = element_text(face = "bold", size = 15),
    plot.title = element_text(face = "bold", size = 17),
    axis.text = element_text(size = 12),
    plot.margin = margin(8, 12, 8, 8)
  )

year_axis <- scale_x_continuous(limits = c(2025, 2044),
                                breaks = c(2025, 2030, 2035, 2040, 2044))

panel_a <- ggplot(annual_state, aes(x = year, color = scenario)) +
  geom_line(aes(y = annual_median), alpha = 0.45, linewidth = 0.55) +
  geom_point(aes(y = annual_median), alpha = 0.65, size = 1.8) +
  geom_line(aes(y = five_year_median), linewidth = 1.35, na.rm = TRUE) +
  scale_color_manual(values = c("BAU baseline" = "#355C72",
                                "NBS management" = "#BC6C35")) +
  year_axis +
  labs(title = "A  BAU and NBS", x = "Year", color = NULL,
       y = "N₂O-N (kg N ha⁻¹ yr⁻¹)") +
  theme_annual + theme(legend.position = "top")

panel_b <- ggplot(annual_contrast, aes(x = year)) +
  geom_hline(yintercept = 0, color = "#59646B", linewidth = 0.5) +
  geom_line(aes(y = annual_median), color = "#BC6C35", alpha = 0.45, linewidth = 0.55) +
  geom_point(aes(y = annual_median), color = "#BC6C35", alpha = 0.65, size = 1.8) +
  geom_line(aes(y = five_year_median), color = "#BC6C35", linewidth = 1.35,
            na.rm = TRUE) +
  year_axis +
  labs(title = "B  NBS minus BAU", x = "Year",
       y = "N₂O-N difference (kg N ha⁻¹ yr⁻¹)") +
  theme_annual

figure <- panel_a | panel_b

summarize_members <- function(data) data |>
  summarise(
    mean = mean(kg_N_ha), median = median(kg_N_ha),
    p05 = quantile(kg_N_ha, 0.05), p95 = quantile(kg_N_ha, 0.95),
    .groups = "drop"
  )
print(state_member_period |> group_by(configuration, period) |> summarize_members())
print(contrast_member_period |> group_by(period) |> summarize_members())

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, figure, width = 10, height = 4.4, units = "in", dpi = 180, bg = "white")
message("Wrote ", normalizePath(output_path, mustWork = TRUE))
