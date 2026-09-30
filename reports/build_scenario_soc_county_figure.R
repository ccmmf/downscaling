#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(sf)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L || length(args) > 3L) {
  stop("Usage: Rscript reports/build_scenario_soc_county_figure.R COUNTY_CSV_GZ COUNTIES_GPKG [OUTPUT_PNG]")
}
source_path <- normalizePath(args[[1]], mustWork = TRUE)
geography_path <- normalizePath(args[[2]], mustWork = TRUE)
output_path <- if (length(args) == 3L) args[[3]] else file.path(
  "reports", "assets", "carb-results", "scenario-soc-contrasts-v2.png"
)

values <- read.csv(gzfile(source_path), check.names = FALSE)
stopifnot(
  identical(names(values), c("county", "reference", "scenario", "value")),
  identical(sort(unique(values$scenario)), c("management", "model", "pathway")),
  !anyDuplicated(values[c("county", "scenario")]),
  all(is.finite(values$value)),
  all(table(values$scenario) == 57L)
)
values$comparison <- factor(
  values$scenario,
  levels = c("management", "model", "pathway"),
  labels = c("NBS", "Climate model", "SSP5-8.5")
)
counties <- read_sf(geography_path) |>
  st_transform(3310) |>
  select(county)
map <- counties |>
  inner_join(values, by = "county")
stopifnot(nrow(map) == nrow(values))
limit <- max(abs(map$value))

figure <- ggplot(map) +
  geom_sf(aes(fill = value), color = "white", linewidth = 0.18) +
  facet_wrap(~comparison, nrow = 1) +
  scale_fill_gradient2(
    low = "#a45432", mid = "#f6f4ef", high = "#246b7b", midpoint = 0,
    limits = c(-limit, limit), breaks = c(-limit, 0, limit),
    labels = scales::label_number(accuracy = 0.01), name = "Tg C"
  ) +
  coord_sf(datum = NA) +
  labs(title = "County soil-carbon change relative to the baseline") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text = element_blank(), axis.title = element_blank(),
    axis.ticks = element_blank(), panel.grid = element_blank(),
    strip.text = element_text(face = "bold", size = 16),
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "bottom", legend.text = element_text(size = 13),
    legend.title = element_text(size = 13)
  )

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
ggsave(output_path, figure, width = 8.5, height = 5.8, units = "in", dpi = 180, bg = "white")
message("Wrote ", normalizePath(output_path, mustWork = TRUE))
