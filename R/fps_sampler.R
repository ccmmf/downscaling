#' greedy farthest-point sampling (maximin)
#'
#' picks points one at a time so each new point is the one farthest from
#' the set already chosen. produces a nested ordering: the first n points
#' for any n <= n_max are a valid maximin subset. used for building a
#' skill vs N ladder where smaller designs must be prefixes of larger ones.
#'
#' @param x numeric matrix, rows are points, cols are features.
#' @param n_max integer, number of points to select. defaults to nrow(x).
#' @param seed_idx integer, starting row index.
#' @return integer vector of length n_max, row indices in FPS order.
#' @references Johnson, Moore, Ylvisaker 1990 J. Stat. Plan. Inf. 26:131-148;
#'   Sener & Savarese 2018 ICLR core-set; Qian 2009 Biometrika 96:957 NLHD.
#' @export
greedy_fps <- function(x, n_max = nrow(x), seed_idx = 1L) {
  n <- nrow(x)
  if (n_max > n) n_max <- n
  order <- integer(n_max)
  order[1] <- seed_idx

  # min squared distance of each row to the already selected set
  diffs <- sweep(x, 2, x[seed_idx, ], "-")
  min_d2 <- rowSums(diffs * diffs)

  for (i in 2:n_max) {
    j <- which.max(min_d2)
    order[i] <- j
    diffs <- sweep(x, 2, x[j, ], "-")
    d2_new <- rowSums(diffs * diffs)
    min_d2 <- pmin(min_d2, d2_new)
  }
  order
}
