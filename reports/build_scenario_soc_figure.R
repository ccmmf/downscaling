#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 2L) {
  stop("Usage: Rscript reports/build_scenario_soc_figure.R DATA_DIRECTORY [OUTPUT_PNG]")
}

data_directory <- normalizePath(args[[1]], mustWork = TRUE)
output_path <- if (length(args) == 2L) args[[2]] else file.path(
  "reports", "assets", "carb-results", "scenario-soc-statewide.png"
)
state_path <- file.path(data_directory, "delta_soc_state_members.csv")
contrast_path <- file.path(data_directory, "delta_soc_contrast_members.csv")
if (!file.exists(state_path) || !file.exists(contrast_path)) {
  stop("The data directory must contain both member-level SOC CSV files.")
}

required_columns <- c("configuration", "pft", "member", "window", "delta_soc_Tg_C")
state_all <- read.csv(state_path, check.names = FALSE)
contrast_all <- read.csv(contrast_path, check.names = FALSE)
if (!all(required_columns %in% names(state_all)) ||
    !all(required_columns %in% names(contrast_all))) {
  stop("The member-level SOC files do not contain the required columns.")
}

window_label <- "December 2045 monthly mean minus January 2024 monthly mean"
state <- state_all |>
  filter(pft == "All crops", configuration %in% c("reference", "management", "model", "pathway")) |>
  select(configuration, member, window, delta_soc_Tg_C)
contrasts <- contrast_all |>
  filter(pft == "All crops", configuration %in% c(
    "management_minus_reference", "model_minus_reference", "pathway_minus_reference"
  )) |>
  select(configuration, member, window, delta_soc_Tg_C)
if (any(state$window != window_label) || any(contrasts$window != window_label)) {
  stop("Unexpected SOC comparison window.")
}
if (anyDuplicated(state[c("configuration", "member")]) ||
    anyDuplicated(contrasts[c("configuration", "member")])) {
  stop("Duplicate configuration/member keys in member-level SOC data.")
}

state_member_sets <- split(state$member, state$configuration)
contrast_member_sets <- split(contrasts$member, contrasts$configuration)
expected_members <- sort(unique(state$member[state$configuration == "reference"]))
if (length(expected_members) != 20L ||
    !all(vapply(state_member_sets, function(x) identical(sort(x), expected_members), logical(1))) ||
    !all(vapply(contrast_member_sets, function(x) identical(sort(x), expected_members), logical(1)))) {
  stop("Expected the same 20 original members in every scenario and contrast.")
}

state_wide <- state |>
  select(configuration, member, delta_soc_Tg_C) |>
  pivot_wider(names_from = configuration, values_from = delta_soc_Tg_C)
computed_contrasts <- bind_rows(
  transmute(state_wide, configuration = "management_minus_reference", member,
            computed = management - reference),
  transmute(state_wide, configuration = "model_minus_reference", member,
            computed = model - reference),
  transmute(state_wide, configuration = "pathway_minus_reference", member,
            computed = pathway - reference)
)
contrast_check <- contrasts |>
  select(configuration, member, supplied = delta_soc_Tg_C) |>
  inner_join(computed_contrasts, by = c("configuration", "member")) |>
  mutate(difference = supplied - computed)
if (nrow(contrast_check) != 60L || anyNA(contrast_check) ||
    max(abs(contrast_check$difference)) >= 1e-10) {
  stop("Supplied contrasts do not equal scenario minus baseline by member.")
}

scenario_labels <- c(
  reference = "bau-cesm-ssp370",
  management = "nbs-cesm-ssp370",
  model = "bau-mpi-ssp370",
  pathway = "bau-cesm-ssp585"
)
scenario_levels <- unname(scenario_labels[c("reference", "management", "model", "pathway")])
contrast_labels <- c(
  management_minus_reference = "nbs-cesm-ssp370",
  model_minus_reference = "bau-mpi-ssp370",
  pathway_minus_reference = "bau-cesm-ssp585"
)
contrast_levels <- unname(contrast_labels[c(
  "management_minus_reference", "model_minus_reference", "pathway_minus_reference"
)])
scenario_colors <- c(
  "bau-cesm-ssp370" = "#355C72",
  "nbs-cesm-ssp370" = "#BC6C35",
  "bau-mpi-ssp370" = "#7A7B4F",
  "bau-cesm-ssp585" = "#8A5D83"
)
state_plot <- state |>
  mutate(scenario = factor(scenario_labels[configuration], levels = rev(scenario_levels)))
contrast_plot <- contrasts |>
  mutate(scenario = factor(contrast_labels[configuration], levels = rev(contrast_levels)))

summarize_members <- function(data) {
  data |>
    group_by(scenario) |>
    summarise(
      mean = mean(delta_soc_Tg_C), median = median(delta_soc_Tg_C),
      p05 = quantile(delta_soc_Tg_C, 0.05), p95 = quantile(delta_soc_Tg_C, 0.95),
      .groups = "drop"
    ) |>
    mutate(label = sprintf("mean %.1f | median %.1f | p05–p95 %.1f to %.1f", mean, median, p05, p95))
}
state_summary <- summarize_members(state_plot)
contrast_summary <- summarize_members(contrast_plot)

theme_soc <- theme_minimal(base_size = 11.5) +
  theme(
    panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#E3E6E8", linewidth = 0.35),
    plot.title = element_text(face = "bold", size = 14, color = "#1F2930"),
    plot.subtitle = element_text(size = 10.5, color = "#4C5962"),
    axis.title = element_text(color = "#28343B"), axis.text = element_text(color = "#404B52"),
    axis.text.y = element_text(face = "bold"), legend.position = "none",
    plot.margin = margin(8, 12, 8, 8)
  )

member_plot <- function(data, title, subtitle, x_label) {
  ggplot(data, aes(x = delta_soc_Tg_C, y = scenario, fill = scenario)) +
    geom_vline(xintercept = 0, color = "#59646B", linewidth = 0.55) +
    geom_boxplot(width = 0.42, alpha = 0.15, outlier.shape = NA,
                 color = "#4E5960", linewidth = 0.55) +
    geom_point(position = position_jitter(width = 0, height = 0.075, seed = 42),
               shape = 21, color = "white", stroke = 0.35, size = 2.35, alpha = 0.9) +
    stat_summary(fun = median, geom = "point", shape = 23, size = 3,
                 fill = "white", color = "#263238", stroke = 0.65) +
    scale_fill_manual(values = scenario_colors) +
    scale_x_continuous(expand = expansion(mult = c(0.03, 0.03))) +
    labs(title = title, subtitle = subtitle, x = x_label, y = NULL) +
    theme_soc
}

panel_a <- member_plot(
  state_plot, "A  Soil-carbon change by scenario",
  "One baseline and three alternatives; each point is one original ensemble member",
  "SOC change, January 2024 to December 2045 (Tg C)"
)
panel_b <- member_plot(
  contrast_plot, "B  Matched difference from the baseline",
  "Alternative minus baseline, calculated for each member before summarization",
  "Difference in SOC change (Tg C)"
)

figure <- panel_a | panel_b +
  plot_layout(widths = c(1.05, 1)) +
  plot_annotation(
    title = "Statewide soil-carbon change and scenario contrasts, 2024–2045",
    caption = paste(
      "Points are the 20 original ensemble members. Boxes show the median and interquartile range;",
      "diamonds show medians. Positive values in Panel B indicate more soil carbon than the baseline trajectory."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 18, color = "#172127"),
      plot.caption = element_text(size = 9.5, color = "#4C5962", hjust = 0),
      plot.margin = margin(12, 14, 10, 12)
    )
  )

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, figure, width = 14, height = 5.8, units = "in", dpi = 180, bg = "white")
print(state_summary)
print(contrast_summary)
message("Wrote ", normalizePath(output_path, mustWork = TRUE))
