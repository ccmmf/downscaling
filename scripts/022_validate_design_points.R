library(ggplot2)
source("000-config.R")
PEcAn.logger::logger.info("*** Design Point Validation ***")

# tabular KPIs + diagnostic figures. produces:
# reports/design_point_validation.md
# figures/cluster_variable_importance.svg
# figures/cdf_overlay_<pft>.webp
# figures/pca_scatter_<pft>.webp
# figures/nn_distance.svg
# figures/cluster_sizes_<pft>.webp
# figures/design_points.webp

figures_dir <- here::here("figures")
reports_dir <- here::here("reports")
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)
if (!dir.exists(reports_dir)) dir.create(reports_dir, recursive = TRUE)

##inputs
design_points <- readr::read_csv(
  here::here("data", "design_points.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(site_id = as.character(site_id))

site_covariates <- readr::read_csv(
  file.path(data_dir, "site_covariates.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(site_id = as.character(site_id))

anchor_sites <- readr::read_csv(
  here::here("data", "anchor_sites.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(site_id = as.character(site_id))

clustering_path <- file.path(cache_dir, "clustering_pool.rds")
if (!file.exists(clustering_path)) {
  PEcAn.logger::logger.severe(
    "run 020_cluster_sites.R first; missing ", clustering_path
  )
}
clustering <- readRDS(clustering_path)

sites_clustered_path <- file.path(cache_dir, "sites_clustered.rds")
if (!file.exists(sites_clustered_path)) {
  PEcAn.logger::logger.severe(
    "run 020_cluster_sites.R first; missing ", sites_clustered_path
  )
}
sites_clustered <- readRDS(sites_clustered_path)

feat_cols <- clustering$feature_cols

# pft allocation, population vs design counts and area
pft_pop <- site_covariates |>
  dplyr::summarise(
    n_pop = dplyr::n(),
    area_pop = sum(area_ha, na.rm = TRUE),
    .by = pft
  ) |>
  dplyr::mutate(
    pct_pop_n = round(n_pop / sum(n_pop) * 100, 1),
    pct_pop_area = round(area_pop / sum(area_pop) * 100, 1)
  )

pft_design <- design_points |>
  dplyr::count(pft, name = "n_design") |>
  dplyr::mutate(pct_design = round(n_design / sum(n_design) * 100, 1))

pft_kpi <- dplyr::full_join(pft_pop, pft_design, by = "pft")
PEcAn.logger::logger.info("pft allocation:")
print(knitr::kable(pft_kpi, format = "simple"))

# mssd in scaled feature space per pft
compute_mssd <- function(pop_scaled, design_scaled) {
  d2 <- numeric(nrow(pop_scaled))
  chunk <- 5000L
  for (s in seq(1L, nrow(pop_scaled), by = chunk)) {
    e <- min(s + chunk - 1L, nrow(pop_scaled))
    block <- pop_scaled[s:e, , drop = FALSE]
    cross <- tcrossprod(block, design_scaled)
    pop_sq <- rowSums(block^2)
    des_sq <- rowSums(design_scaled^2)
    d2_mat <- outer(pop_sq, des_sq, `+`) - 2 * cross
    d2[s:e] <- apply(d2_mat, 1, min)
  }
  mean(d2)
}

mssd_per_pft <- purrr::imap_dfr(clustering$by_pft, function(r, pft_name) {
  pop <- site_covariates |>
    dplyr::filter(pft == pft_name) |>
    dplyr::select(site_id, dplyr::all_of(feat_cols)) |>
    tidyr::drop_na()
  design <- design_points |>
    dplyr::filter(pft == pft_name) |>
    dplyr::left_join(site_covariates, by = "site_id") |>
    dplyr::select(site_id, dplyr::all_of(feat_cols)) |>
    tidyr::drop_na()

  pop_scaled <- base::scale(
    as.matrix(pop[, feat_cols, drop = FALSE]),
    center = r$scaling_center,
    scale = r$scaling_scale
  )
  design_scaled <- base::scale(
    as.matrix(design[, feat_cols, drop = FALSE]),
    center = r$scaling_center,
    scale = r$scaling_scale
  )

  tibble::tibble(
    pft = pft_name,
    n_pop = nrow(pop_scaled),
    n_design = nrow(design_scaled),
    mssd = compute_mssd(pop_scaled, design_scaled)
  )
})
PEcAn.logger::logger.info("mssd per pft:")
print(knitr::kable(mssd_per_pft, format = "simple", digits = 4))

# cluster balance, gini of cluster sizes per pft
gini <- function(x) {
  x <- sort(x)
  n <- length(x)
  (2 * sum(seq_len(n) * x) - (n + 1) * sum(x)) / (n * sum(x))
}
cluster_balance <- purrr::imap_dfr(clustering$by_pft, function(r, pft_name) {
  sizes <- as.numeric(table(r$cluster_assignment$cluster))
  tibble::tibble(
    pft = pft_name,
    n_clusters = length(sizes),
    min_size = min(sizes),
    median_size = median(sizes),
    max_size = max(sizes),
    gini = gini(sizes)
  )
})
PEcAn.logger::logger.info("cluster balance per pft:")
print(knitr::kable(cluster_balance, format = "simple", digits = 3))

# anchor recovery
anchor_recovery <- tibble::tibble(
  n_anchors = nrow(anchor_sites),
  in_design = sum(anchor_sites$site_id %in% design_points$site_id),
  missing = sum(!anchor_sites$site_id %in% design_points$site_id)
)
PEcAn.logger::logger.info(
  "anchor recovery: ", anchor_recovery$in_design, "/", anchor_recovery$n_anchors
)
# log which anchors didn't make it in. non-ag anchors (e.g. fallow) are
# expected to be missing under the 2 PFT scheme; ag anchors missing here
# would be a real flag
if (anchor_recovery$missing > 0) {
  missed <- anchor_sites |>
    dplyr::filter(!site_id %in% design_points$site_id) |>
    dplyr::select(site_id, external_site_id, site_name, pft)
  PEcAn.logger::logger.info("anchors not in design:")
  print(knitr::kable(missed, format = "simple"))
}

# monitoring layer coverage, how many parcels have each feature in the
# population vs design. low design side coverage means the FPS subsample
# missed that feature's variance; flag parcels below 80% threshold
mgmt_features <- intersect(
  c("tillage_rank", "tillage_freq",
    "leafon_doy", "leafoff_doy",
    "leafon_doy_sd", "leafoff_doy_sd",
    "irr_canopy", "irr_flood"),
  colnames(site_covariates)
)
if (length(mgmt_features) > 0) {
  design_cov <- site_covariates |>
    dplyr::filter(site_id %in% design_points$site_id) |>
    dplyr::select(dplyr::all_of(mgmt_features))
  pop_cov <- site_covariates |>
    dplyr::select(dplyr::all_of(mgmt_features))
  nonzero_features <- c("irr_canopy", "irr_flood", "tillage_rank", "tillage_freq")
  mgmt_kpi <- purrr::map_dfr(mgmt_features, function(f) {
    pop_n <- if (f %in% nonzero_features) {
      sum(pop_cov[[f]] > 0, na.rm = TRUE)
    } else {
      sum(!is.na(pop_cov[[f]]))
    }
    des_n <- if (f %in% nonzero_features) {
      sum(design_cov[[f]] > 0, na.rm = TRUE)
    } else {
      sum(!is.na(design_cov[[f]]))
    }
    tibble::tibble(
      feature = f,
      pop_n = pop_n,
      pop_pct = round(pop_n / nrow(pop_cov) * 100, 1),
      design_n = des_n,
      design_pct = round(des_n / nrow(design_cov) * 100, 1)
    )
  })
  PEcAn.logger::logger.info("monitoring layer coverage:")
  print(knitr::kable(mgmt_kpi, format = "simple"))
  low_cov <- mgmt_kpi |> dplyr::filter(design_pct < 80 & pop_pct >= 80)
  if (nrow(low_cov) > 0) {
    PEcAn.logger::logger.warn(
      "features with design coverage <80% of population coverage: ",
      paste(low_cov$feature, collapse = ", ")
    )
  }
} else {
  mgmt_kpi <- NULL
}

# climate region coverage
if ("climregion_id" %in% colnames(site_covariates)) {
  region_pop <- site_covariates |>
    dplyr::count(climregion_id, name = "n_pop") |>
    dplyr::mutate(pct_pop = n_pop / sum(n_pop) * 100)
  region_design <- design_points |>
    dplyr::left_join(site_covariates |> dplyr::select(site_id, climregion_id),
                     by = "site_id") |>
    dplyr::count(climregion_id, name = "n_design") |>
    dplyr::mutate(pct_design = n_design / sum(n_design) * 100)
  region_kpi <- dplyr::full_join(region_pop, region_design, by = "climregion_id") |>
    dplyr::mutate(
      n_pop = tidyr::replace_na(n_pop, 0),
      n_design = tidyr::replace_na(n_design, 0),
      pct_pop = round(tidyr::replace_na(pct_pop, 0), 1),
      pct_design = round(tidyr::replace_na(pct_design, 0), 1)
    )
  PEcAn.logger::logger.info("climate region coverage:")
  print(knitr::kable(region_kpi, format = "simple"))
} else {
  region_kpi <- NULL
}

# variable importance via eta-squared
# eta squared = between cluster variance / total variance.
# higher = predictor carries more cluster separation
PEcAn.logger::logger.info("variable importance (eta-squared):")
eta2_tbl <- purrr::map_dfr(feat_cols, function(vn) {
  x <- sites_clustered[[vn]]
  cl <- as.factor(sites_clustered$cluster)
  m <- mean(x, na.rm = TRUE)
  g_mean <- tapply(x, cl, function(v) mean(v, na.rm = TRUE))
  g_n <- tapply(x, cl, function(v) sum(!is.na(v)))
  N <- sum(!is.na(x))
  total <- stats::var(x, na.rm = TRUE) * max(N - 1, 1)
  between <- sum(g_n * (g_mean - m)^2, na.rm = TRUE)
  eta2 <- ifelse(is.finite(total) & total > 0, between / total, NA_real_)
  tibble::tibble(variable = vn, eta2 = eta2)
}) |>
  dplyr::filter(!is.na(eta2)) |>
  dplyr::arrange(dplyr::desc(eta2))

vi_cluster_plot <- ggplot(eta2_tbl, aes(x = reorder(variable, eta2), y = eta2)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Predictor", y = expression(eta^2 ~ " (between / total variance)"),
    title = "Variable importance (eta-squared)"
  ) +
  theme_minimal()

ggsave_optimized("figures/cluster_variable_importance.svg", plot = vi_cluster_plot)

# per feature ECDF, population vs design.
# KS per panel flags features the design fails to track
ks_per_feat <- function(pop_x, des_x) {
  if (length(des_x) < 2) return(NA_real_)
  suppressWarnings(stats::ks.test(pop_x, des_x)$statistic)
}

for (pft_name in names(clustering$by_pft)) {
  pop <- sites_clustered |> dplyr::filter(pft == pft_name)
  des <- design_points |>
    dplyr::filter(pft == pft_name) |>
    dplyr::left_join(sites_clustered |> dplyr::select(-pft), by = "site_id")

  ks <- vapply(feat_cols, function(f)
                 round(ks_per_feat(pop[[f]], des[[f]]), 3),
               numeric(1))

  long <- dplyr::bind_rows(
    pop |> dplyr::select(dplyr::all_of(feat_cols)) |>
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = "feature", values_to = "value") |>
      dplyr::mutate(source = "population"),
    des |> dplyr::select(dplyr::all_of(feat_cols)) |>
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = "feature", values_to = "value") |>
      dplyr::mutate(source = "design")
  ) |>
    dplyr::mutate(feature = factor(feature, levels = feat_cols))

  ks_lab <- tibble::tibble(
    feature = factor(names(ks), levels = feat_cols),
    label = paste0("KS=", round(ks, 2))
  )

  cdf_plot <- ggplot(long, aes(value, color = source)) +
    stat_ecdf(linewidth = 0.6) +
    facet_wrap(~ feature, scales = "free_x", ncol = 5) +
    geom_text(data = ks_lab, aes(x = -Inf, y = 0.95, label = label),
              hjust = -0.1, color = "grey20",
              inherit.aes = FALSE, size = 2.6) +
    scale_color_brewer(palette = "Dark2") +
    labs(title = paste0("Empirical CDF: population vs design (", pft_name, ")"),
         x = NULL, y = "cumulative fraction") +
    theme_minimal()

  fname <- sprintf("figures/cdf_overlay_%s.webp",
                   gsub("[^a-z]", "_", tolower(pft_name)))
  ggsave_optimized(fname, plot = cdf_plot,
                   width = 12, height = 9, units = "in", dpi = 96, bg = "white")
}

# PCA scatter, pop subsample + design overlay.
# project pop and design with the same V via X %*% V manually.
# predict.prcomp re centers even with prcomp(center = FALSE), so use SVD directly
for (pft_name in names(clustering$by_pft)) {
  r <- clustering$by_pft[[pft_name]]
  pop <- sites_clustered |>
    dplyr::filter(pft == pft_name) |>
    dplyr::select(site_id, dplyr::all_of(feat_cols)) |>
    tidyr::drop_na()
  des <- design_points |>
    dplyr::filter(pft == pft_name) |>
    dplyr::left_join(sites_clustered, by = c("site_id", "pft")) |>
    dplyr::select(site_id, dplyr::all_of(feat_cols)) |>
    tidyr::drop_na()
  pop_X <- base::scale(as.matrix(pop[, feat_cols]),
                       center = r$scaling_center, scale = r$scaling_scale)
  des_X <- base::scale(as.matrix(des[, feat_cols]),
                       center = r$scaling_center, scale = r$scaling_scale)

  sv <- svd(pop_X, nu = 0, nv = 2)
  V <- sv$v
  ve <- sv$d[1:2]^2 / sum(svd(pop_X, nu = 0, nv = 0)$d^2)
  pop_pc <- as.data.frame(pop_X %*% V)
  des_pc <- as.data.frame(des_X %*% V)
  colnames(pop_pc) <- c("PC1", "PC2")
  colnames(des_pc) <- c("PC1", "PC2")

  withr::with_seed(42, {
    pop_sub <- pop_pc[sample.int(nrow(pop_pc), min(50000, nrow(pop_pc))), ]
  })

  pca_plot <- ggplot() +
    geom_point(data = pop_sub, aes(PC1, PC2),
               color = "lightblue", size = 0.18, alpha = 0.2) +
    geom_point(data = des_pc, aes(PC1, PC2),
               color = "black", size = 0.7, alpha = 0.85) +
    labs(title = paste0("PCA of scaled features: design overlay (", pft_name, ")"),
         x = sprintf("PC1 (%.1f%%)", 100 * ve[1]),
         y = sprintf("PC2 (%.1f%%)", 100 * ve[2])) +
    theme_minimal()

  fname <- sprintf("figures/pca_scatter_%s.webp",
                   gsub("[^a-z]", "_", tolower(pft_name)))
  ggsave_optimized(fname, plot = pca_plot,
                   width = 8, height = 6.5, units = "in", dpi = 96, bg = "white")
}

# nearest neighbor distance for design points in scaled feature space.
# right shifted distribution = better space filling
nn_dist <- function(X) {
  if (nrow(X) < 2) return(numeric(0))
  res <- numeric(nrow(X))
  for (i in seq_len(nrow(X))) {
    diffs <- sweep(X, 2, X[i, ], "-")
    d2 <- rowSums(diffs * diffs)
    d2[i] <- Inf
    res[i] <- sqrt(min(d2))
  }
  res
}
nn_df <- purrr::imap_dfr(clustering$by_pft, function(r, pft_name) {
  des <- design_points |>
    dplyr::filter(pft == pft_name) |>
    dplyr::left_join(sites_clustered, by = c("site_id", "pft")) |>
    dplyr::select(site_id, dplyr::all_of(feat_cols)) |>
    tidyr::drop_na()
  des_X <- base::scale(as.matrix(des[, feat_cols]),
                       center = r$scaling_center, scale = r$scaling_scale)
  tibble::tibble(pft = pft_name, nn = nn_dist(des_X))
})

nn_plot <- ggplot(nn_df, aes(nn)) +
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.8) +
  facet_wrap(~ pft, scales = "free") +
  labs(title = "Design-point nearest-neighbor distance in scaled feature space",
       x = "NN distance", y = "count") +
  theme_minimal()
ggsave_optimized("figures/nn_distance.svg", plot = nn_plot,
                 width = 10, height = 4, units = "in")

# sorted cluster sizes per pft -- catch degenerate singletons or giants
for (pft_name in names(clustering$by_pft)) {
  r <- clustering$by_pft[[pft_name]]
  sizes <- as.numeric(table(r$cluster_assignment$cluster))
  size_df <- tibble::tibble(size = sort(sizes), rank = seq_along(sizes))

  size_plot <- ggplot(size_df, aes(rank, size)) +
    geom_col(width = 1, fill = "steelblue", color = NA) +
    geom_hline(yintercept = median(sizes), linetype = "dashed", color = "grey30") +
    labs(title = paste0("Sorted cluster sizes (", pft_name, ")"),
         x = "cluster rank (smallest to largest)",
         y = "members") +
    theme_minimal()

  # webp not svg, cuz 5000 vector bars produce ~800 KB SVG, webp is ~30 KB.
  fname <- sprintf("figures/cluster_sizes_%s.webp",
                   gsub("[^a-z]", "_", tolower(pft_name)))
  ggsave_optimized(fname, plot = size_plot,
                   width = 8, height = 4, units = "in", dpi = 96, bg = "white")
}

# spatial map of design points on CA cropland. anchors highlighted
ca_climregions <- sf::st_read(
  file.path(data_dir, "caladapt_climregions.gpkg"),
  quiet = TRUE
) |>
  sf::st_transform(4326)
ca_fields <- sf::st_read(
  file.path(data_dir, "cadwr_crops_sites.gpkg"),
  quiet = TRUE
) |>
  sf::st_transform(4326)

design_sf <- design_points |>
  tidyr::drop_na(lat, lon) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

anchor_sf <- anchor_sites |>
  dplyr::filter(site_id %in% design_points$site_id) |>
  tidyr::drop_na(lat, lon) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

design_pt_plot <- ggplot() +
  geom_sf(data = ca_climregions, fill = "white", color = "grey80") +
  geom_sf(data = ca_fields, fill = "lightgrey", color = "lightgrey", alpha = 0.25) +
  geom_sf(data = design_sf, aes(color = pft), size = 0.6, alpha = 0.8) +
  geom_sf(data = anchor_sf, color = "black", shape = 21, fill = "yellow",
          size = 2, stroke = 0.6) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "PFT",
       title = "Design points on California cropland (anchors in yellow)") +
  theme_minimal()

ggsave_optimized("figures/design_points.webp",
                 plot = design_pt_plot,
                 width = 10, height = 6, units = "in", dpi = 96, bg = "white")

# optional: internal validity indices vs k. expensive on large pools
# because each k requires recomputing on the 20k subsample. uncomment
# to scan a coarse k grid (e.g. 50, 200, 1000, 2000, 5000, 8000) and
# pick the value of k that best matches the area proportional allocation.
# requires: factoextra, fpc, clusterSim (none in renv yet)
#
# # silhouette method
# fviz_nbclust(d, kmeans, k.max = 20, method = "silhouette") +
#   labs(subtitle = "Silhouette method")
#
# # Calinski Harabasz index
# fpc::cluster.stats(stats::dist(d), clusters)$ch
#
# # davies bouldin index
# clusterSim::index.DB(d, clusters)$DB

##write markdown report
report_dir <- here::here("reports")
if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE)

report_lines <- c(
  "# Design Point Validation",
  "",
  paste0("Generated: ", format(Sys.time())),
  paste0("Total design points: ", nrow(design_points)),
  paste0("Seed: ", clustering$seed),
  paste0("Features: ", paste(clustering$feature_cols, collapse = ", ")),
  "",
  "## PFT allocation",
  "",
  knitr::kable(pft_kpi, format = "markdown"),
  "",
  "## Feature space coverage (MSSD, scaled)",
  "",
  knitr::kable(mssd_per_pft, format = "markdown", digits = 4),
  "",
  "## Cluster balance (Gini of cluster sizes)",
  "",
  knitr::kable(cluster_balance, format = "markdown", digits = 3),
  "",
  "## Anchor recovery",
  "",
  knitr::kable(anchor_recovery, format = "markdown"),
  ""
)

if (!is.null(mgmt_kpi)) {
  report_lines <- c(
    report_lines,
    "## Monitoring layer coverage",
    "",
    knitr::kable(mgmt_kpi, format = "markdown"),
    ""
  )
}

if (!is.null(region_kpi)) {
  report_lines <- c(
    report_lines,
    "## Climate region coverage",
    "",
    knitr::kable(region_kpi, format = "markdown"),
    ""
  )
}

report_lines <- c(
  report_lines,
  "## Figures",
  "",
  "![Variable importance (eta-squared)](../figures/cluster_variable_importance.svg)",
  "",
  "![Design points on California cropland](../figures/design_points.webp)",
  "",
  "![Nearest-neighbor distances](../figures/nn_distance.svg)",
  ""
)

# embed per pft figures (CDF + PCA + cluster sizes). loop over PFTs
# in the clustering cache so the report stays in sync if a third PFT is
# ever added
for (pft_name in names(clustering$by_pft)) {
  slug <- gsub("[^a-z]", "_", tolower(pft_name))
  report_lines <- c(
    report_lines,
    sprintf("![CDF overlay (%s)](../figures/cdf_overlay_%s.webp)", pft_name, slug),
    "",
    sprintf("![PCA scatter (%s)](../figures/pca_scatter_%s.webp)", pft_name, slug),
    "",
    sprintf("![Sorted cluster sizes (%s)](../figures/cluster_sizes_%s.webp)", pft_name, slug),
    ""
  )
}

report_path <- file.path(report_dir, "design_point_validation.md")
writeLines(report_lines, report_path)
PEcAn.logger::logger.info("report written: ", report_path)
