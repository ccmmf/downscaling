#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 2L) {
  stop(
    "Usage: Rscript reports/build_scenario_soc_figure.R ",
    "DATA_DIRECTORY [OUTPUT_PNG]"
  )
}

data_directory <- normalizePath(args[[1]], mustWork = TRUE)
output_path <- if (length(args) == 2L) {
  args[[2]]
} else {
  file.path("reports", "assets", "carb-results", "scenario-soc-statewide.png")
}

state_path <- file.path(data_directory, "delta_soc_state_members.csv")
contrast_path <- file.path(data_directory, "delta_soc_contrast_members.csv")
if (!file.exists(state_path) || !file.exists(contrast_path)) {
  stop("The data directory must contain both member-level SOC CSV files.")
}

required_columns <- c(
  "configuration", "pft", "member", "window", "delta_soc_Tg_C"
)
state_all <- read.csv(state_path, check.names = FALSE)
contrast_all <- read.csv(contrast_path, check.names = FALSE)
if (!all(required_columns %in% names(state_all)) ||
    !all(required_columns %in% names(contrast_all))) {
  stop("The member-level SOC files do not contain the required columns.")
}

window_label <- "December 2045 monthly mean minus January 2024 monthly mean"
state <- state_all |>
  filter(
    pft == "All crops",
    configuration %in% c("reference", "management", "model", "pathway")
  ) |>
  select(configuration, member, window, delta_soc_Tg_C)

contrasts <- contrast_all |>
  filter(
    pft == "All crops",
    configuration %in% c(
      "management_minus_reference",
      "model_minus_reference",
      "pathway_minus_reference"
    )
  ) |>
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
  transmute(
    state_wide,
    configuration = "management_minus_reference",
    member,
    computed = management - reference
  ),
  transmute(
    state_wide,
    configuration = "model_minus_reference",
    member,
    computed = model - reference
  ),
  transmute(
    state_wide,
    configuration = "pathway_minus_reference",
    member,
    computed = pathway - reference
  )
)

contrast_check <- contrasts |>
  select(configuration, member, supplied = delta_soc_Tg_C) |>
  inner_join(computed_contrasts, by = c("configuration", "member")) |>
  mutate(difference = supplied - computed)

if (nrow(contrast_check) != 60L || anyNA(contrast_check) ||
    max(abs(contrast_check$difference)) >= 1e-10) {
  stop("Supplied contrasts do not equal scenario minus reference by member.")
}

comparison_levels <- c(
  "Management",
  "Alternate climate model",
  "Alternate pathway"
)
configuration_labels <- c(
  management = "Management",
  model = "Alternate climate model",
  pathway = "Alternate pathway"
)
contrast_labels <- c(
  management_minus_reference = "Management",
  model_minus_reference = "Alternate climate model",
  pathway_minus_reference = "Alternate pathway"
)
scenario_colors <- c(
  "Reference" = "#355C72",
  "Management" = "#BC6C35",
  "Alternate climate model" = "#7A7B4F",
  "Alternate pathway" = "#8A5D83"
)

reference_values <- state |>
  filter(configuration == "reference") |>
  select(member, reference = delta_soc_Tg_C)

absolute_plot_data <- state |>
  filter(configuration != "reference") |>
  mutate(
    comparison = factor(configuration_labels[configuration], comparison_levels)
  ) |>
  select(comparison, member, scenario = delta_soc_Tg_C) |>
  left_join(reference_values, by = "member") |>
  pivot_longer(
    cols = c(reference, scenario),
    names_to = "role",
    values_to = "delta_soc_Tg_C"
  ) |>
  mutate(
    role = factor(role, levels = c("reference", "scenario"),
                  labels = c("Reference", "Scenario")),
    series = if_else(role == "Reference", "Reference", as.character(comparison))
  )

contrast_plot_data <- contrasts |>
  mutate(
    comparison = factor(contrast_labels[configuration], comparison_levels)
  )

absolute_limits <- range(absolute_plot_data$delta_soc_Tg_C)
absolute_limits <- c(
  floor((absolute_limits[[1]] - 2) / 10) * 10,
  ceiling((absolute_limits[[2]] + 2) / 10) * 10
)
contrast_limits <- range(contrast_plot_data$delta_soc_Tg_C)
contrast_limits <- c(
  floor(contrast_limits[[1]] - 0.5),
  ceiling(contrast_limits[[2]] + 0.5)
)

theme_soc <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E3E6E8", linewidth = 0.35),
    axis.line.y = element_line(color = "#69747C", linewidth = 0.35),
    axis.ticks.y = element_line(color = "#69747C", linewidth = 0.35),
    strip.text = element_text(face = "bold", color = "#2F3B43", size = 11),
    strip.background = element_rect(fill = "#F3F5F6", color = NA),
    plot.title = element_text(face = "bold", size = 14, color = "#1F2930"),
    plot.subtitle = element_text(size = 10.5, color = "#4C5962"),
    axis.title = element_text(color = "#28343B"),
    axis.text = element_text(color = "#404B52"),
    legend.position = "none",
    plot.margin = margin(8, 10, 8, 8)
  )

panel_a <- ggplot(
  absolute_plot_data,
  aes(x = role, y = delta_soc_Tg_C, group = member)
) +
  geom_hline(yintercept = 0, color = "#59646B", linewidth = 0.55) +
  geom_line(color = "#7C878E", alpha = 0.38, linewidth = 0.48) +
  geom_boxplot(
    aes(group = interaction(comparison, role), color = series, fill = series),
    width = 0.32,
    alpha = 0.12,
    outlier.shape = NA,
    linewidth = 0.55
  ) +
  geom_point(
    aes(fill = series),
    shape = 21,
    color = "white",
    stroke = 0.35,
    size = 2.35,
    alpha = 0.92
  ) +
  facet_wrap(vars(comparison), nrow = 1) +
  scale_color_manual(values = scenario_colors) +
  scale_fill_manual(values = scenario_colors) +
  scale_y_continuous(
    limits = absolute_limits,
    breaks = seq(absolute_limits[[1]], absolute_limits[[2]], by = 25),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(
    title = "A  Absolute soil-carbon change by ensemble member",
    subtitle = "Matched members connect the reference and each scenario; all facets use the same scale",
    x = NULL,
    y = "SOC change (Tg C)"
  ) +
  theme_soc +
  theme(axis.text.x = element_text(face = "bold"))

panel_b <- ggplot(
  contrast_plot_data,
  aes(x = comparison, y = delta_soc_Tg_C, fill = comparison)
) +
  geom_hline(yintercept = 0, color = "#59646B", linewidth = 0.55) +
  geom_boxplot(
    width = 0.38,
    alpha = 0.14,
    outlier.shape = NA,
    color = "#4E5960",
    linewidth = 0.55
  ) +
  geom_point(
    position = position_jitter(width = 0.085, height = 0, seed = 42),
    shape = 21,
    color = "white",
    stroke = 0.35,
    size = 2.5,
    alpha = 0.92
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    shape = 23,
    size = 2.9,
    fill = "white",
    color = "#263238",
    stroke = 0.6
  ) +
  scale_fill_manual(values = scenario_colors[comparison_levels]) +
  scale_y_continuous(
    limits = contrast_limits,
    breaks = seq(contrast_limits[[1]], contrast_limits[[2]], by = 2),
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_x_discrete(labels = c(
    "Management" = "Management",
    "Alternate climate model" = "Alternate\nclimate model",
    "Alternate pathway" = "Alternate\npathway"
  )) +
  labs(
    title = "B  Within-member difference from reference",
    subtitle = "Scenario SOC change minus reference SOC change",
    x = NULL,
    y = "Difference in SOC change (Tg C)"
  ) +
  theme_soc +
  theme(axis.text.x = element_text(face = "bold"))

caption_text <- paste(
  strwrap(
    paste0(
      "Each point is one original ensemble member (n = 20 per comparison). ",
      "Boxes show the median and interquartile range; whiskers extend to 1.5 × IQR. ",
      "Panel B uses a tighter scale: positive values indicate more soil carbon relative ",
      "to the reference trajectory, not necessarily an absolute stock gain."
    ),
    width = 150
  ),
  collapse = "\n"
)

figure <- panel_a / panel_b +
  plot_layout(heights = c(1.55, 1)) +
  plot_annotation(
    title = "Statewide soil-carbon change and scenario contrasts, 2024–2045",
    subtitle = "SOC change is December 2045 monthly mean minus January 2024 monthly mean",
    caption = caption_text,
    theme = theme(
      plot.title = element_text(face = "bold", size = 19, color = "#172127"),
      plot.subtitle = element_text(size = 12.5, color = "#4C5962"),
      plot.caption = element_text(size = 9.5, color = "#4C5962", hjust = 0),
      plot.margin = margin(12, 14, 10, 12)
    )
  )

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(
  filename = output_path,
  plot = figure,
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 180,
  bg = "white"
)

summary_table <- contrast_plot_data |>
  group_by(comparison) |>
  summarise(
    members = n(),
    mean = mean(delta_soc_Tg_C),
    median = median(delta_soc_Tg_C),
    q25 = quantile(delta_soc_Tg_C, 0.25),
    q75 = quantile(delta_soc_Tg_C, 0.75),
    p05 = quantile(delta_soc_Tg_C, 0.05),
    p95 = quantile(delta_soc_Tg_C, 0.95),
    .groups = "drop"
  )

print(summary_table)
message("Wrote ", normalizePath(output_path, mustWork = TRUE))
