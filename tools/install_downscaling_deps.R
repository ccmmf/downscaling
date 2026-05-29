#!/usr/bin/env Rscript
# Top-up install on top of pecan-all-1.10 for the 030 -> 043 pipeline.
# Run once after setup-pecan-env.sh and conda activate.

if (Sys.getenv("CONDA_PREFIX") == "") {
  message("Heads up: not in a conda env. Activate pecan-all first if you meant to.")
}

cran_pkgs <- c(
  "argparse", "iml", "janitor", "magick",
  "patchwork", "randomForest", "ranger", "svglite"
)

needed <- cran_pkgs[!vapply(cran_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(needed) > 0) {
  message("Installing from CRAN: ", paste(needed, collapse = ", "))
  install.packages(needed, repos = "https://cloud.r-project.org")
}

if (!requireNamespace("PEcAnAssimSequential", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", repos = "https://cloud.r-project.org")
  }
  remotes::install_github(
    "dlebauer/pecan",
    subdir  = "modules/assim.sequential",
    ref     = "ensemble_downscaling",
    upgrade = "never"
  )
}

critical <- c(
  cran_pkgs, "PEcAnAssimSequential",
  "PEcAn.utils", "PEcAn.logger", "PEcAn.settings", "PEcAn.data.land",
  "dplyr", "tidyr", "readr", "vroom", "ggplot2", "sf", "terra",
  "lubridate", "stringr", "glue", "purrr", "tibble", "rlang",
  "future", "furrr", "arrow", "ncdf4", "jsonlite", "here",
  "knitr", "memoise"
)

failures <- critical[!vapply(critical, requireNamespace, logical(1), quietly = TRUE)]
if (length(failures) > 0) {
  stop("Still missing: ", paste(failures, collapse = ", "))
}
message("Downscaling deps OK.")
