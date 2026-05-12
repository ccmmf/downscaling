#' Allocate design points across PFTs with per-PFT floors
#'
#' @param site_covariates tibble with `pft` and `area_ha` columns.
#' @param total integer total design points.
#' @param floors named integer vector, PFT name -> minimum design points.
#' @return named integer vector summing to `total`.
#' @export
allocate_design_points_by_pft <- function(site_covariates, total, floors) {
  # area-proportional share per PFT.
  area <- site_covariates |>
    dplyr::filter(pft %in% names(floors)) |>
    dplyr::summarise(area = sum(area_ha, na.rm = TRUE), .by = pft) |>
    dplyr::mutate(frac = area / sum(area))

  alloc <- setNames(integer(length(floors)), names(floors))
  for (p in names(floors)) {
    share <- area$frac[area$pft == p]
    if (length(share) == 0) share <- 0
    alloc[p] <- max(floors[p], round(total * share))
  }

  # rescale to exactly total; push rounding drift to largest bucket.
  alloc <- round(alloc * total / sum(alloc))
  drift <- total - sum(alloc)
  if (drift != 0) alloc[which.max(alloc)] <- alloc[which.max(alloc)] + drift

  PEcAn.logger::logger.info(
    "design point allocation (", total, " total): ",
    paste0(names(alloc), "=", alloc, collapse = ", ")
  )
  alloc
}


#' Cluster one PFT population and pick one design point per cluster
#'
#' Two-stage when population exceeds `subsample_threshold`: cluster a random
#' subsample with k-means++, assign all fields to nearest centroid. Single
#' stage otherwise. Design point per cluster is the field closest to the
#' cluster centroid in scaled feature space. Anchor sites are swapped into
#' their own cluster's slot so they always end up in the design.
#'
#' @param pop_data tibble with site_id, lat, lon, pft, and feature_cols.
#' @param feature_cols character vector of numeric feature columns.
#' @param n_clusters integer, equals number of design points.
#' @param subsample_threshold integer, use two-stage above this size.
#' @param num_init integer, k-means++ restarts.
#' @param seed integer random seed.
#' @param anchor_site_ids character vector of anchors to force-include.
#' @param selection_mode "nearest_centroid" (default) or "weighted_random".
#' @return list: design, cluster_assignment, centers, scaling, sizes.
#' @export
cluster_pft_population <- function(
    pop_data,
    feature_cols,
    n_clusters,
    subsample_threshold = 20000L,
    num_init = 5L,
    seed = 42L,
    anchor_site_ids = character(0),
    selection_mode = c("nearest_centroid", "weighted_random")) {
  selection_mode <- match.arg(selection_mode)
  if (!requireNamespace("ClusterR", quietly = TRUE)) {
    PEcAn.logger::logger.severe("ClusterR package is required for clustering.")
  }

  n_total <- nrow(pop_data)
  if (n_clusters > n_total) {
    PEcAn.logger::logger.severe(
      "requested ", n_clusters, " clusters on population of ", n_total
    )
  }

  # feature matrix, drop rows with any NA.
  feat_mat <- as.matrix(pop_data[, feature_cols, drop = FALSE])
  ok <- stats::complete.cases(feat_mat)
  if (any(!ok)) {
    PEcAn.logger::logger.warn(
      "dropping ", sum(!ok), " of ", n_total, " fields with NA features"
    )
    pop_data <- pop_data[ok, ]
    feat_mat <- feat_mat[ok, , drop = FALSE]
    n_total <- nrow(pop_data)
  }

  # z-score
  feat_scaled <- base::scale(feat_mat)
  center_attr <- attr(feat_scaled, "scaled:center")
  scale_attr <- attr(feat_scaled, "scaled:scale")

  set.seed(seed)
  if (n_total > subsample_threshold) {
    PEcAn.logger::logger.info(
      "two-stage clustering: ", subsample_threshold,
      " subsample, assign all ", n_total
    )
    sub_idx <- sample.int(n_total, subsample_threshold)
    fit <- ClusterR::KMeans_rcpp(
      feat_scaled[sub_idx, , drop = FALSE],
      clusters = n_clusters,
      num_init = num_init,
      initializer = "kmeans++",
      seed = seed,
      verbose = FALSE
    )
    centers <- fit$centroids
    clusters_all <- as.integer(
      ClusterR::predict_KMeans(feat_scaled, CENTROIDS = centers)
    )
  } else {
    fit <- ClusterR::KMeans_rcpp(
      feat_scaled,
      clusters = n_clusters,
      num_init = num_init,
      initializer = "kmeans++",
      seed = seed,
      verbose = FALSE
    )
    centers <- fit$centroids
    clusters_all <- as.integer(fit$clusters)
  }

  n_filled <- length(unique(clusters_all))
  if (n_filled < n_clusters) {
    PEcAn.logger::logger.warn(
      n_clusters - n_filled, " empty cluster(s) out of ", n_clusters
    )
  }

  pop_data$cluster <- clusters_all
  pop_data$dist_to_centroid <- sqrt(rowSums(
    (feat_scaled - centers[clusters_all, , drop = FALSE])^2
  ))

  design <- switch(
    selection_mode,
    nearest_centroid = pop_data |>
      dplyr::slice_min(dist_to_centroid, n = 1L, with_ties = FALSE, by = cluster) |>
      dplyr::ungroup(),
    weighted_random = {
      set.seed(seed)
      pop_data |>
        dplyr::slice_sample(n = 1L, by = cluster)
    }
  )

  # swap each anchor into its own cluster's slot. when two anchors land
  # in the same cluster, both are kept -- the filter exempts already
  # inserted anchors so a later iteration doesn't evict an earlier one.
  anchors_in_pop <- intersect(anchor_site_ids, pop_data$site_id)
  design$is_anchor <- FALSE
  for (aid in anchors_in_pop) {
    anchor_row <- pop_data |> dplyr::filter(site_id == aid)
    if (nrow(anchor_row) == 0) next
    c <- anchor_row$cluster[1]
    design <- design |>
      dplyr::filter(cluster != c | is_anchor) |>
      dplyr::bind_rows(anchor_row |> dplyr::mutate(is_anchor = TRUE))
  }
  design$is_anchor <- NULL
  if (length(anchors_in_pop) > 0) {
    PEcAn.logger::logger.info(
      "force included ", length(anchors_in_pop), " anchor(s)"
    )
  }

  list(
    design = design |>
      dplyr::select(site_id, lat, lon, dplyr::any_of("pft"), cluster, dist_to_centroid),
    cluster_assignment = pop_data |> dplyr::select(site_id, cluster),
    centers = centers,
    scaling_center = center_attr,
    scaling_scale = scale_attr,
    feature_cols = feature_cols,
    n_clusters_requested = n_clusters,
    n_clusters_filled = n_filled,
    n_population = n_total
  )
}
