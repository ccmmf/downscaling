## --- Custom R repositories (CRAN + R-universe) ------------------------
options(repos = c(
  pecanproject = "https://pecanproject.r-universe.dev",
  ajlyons      = "https://ajlyons.r-universe.dev", # caladaptr
  CRAN         = "https://cloud.r-project.org"
))

# Sys.setenv(R_LIBS_USER = file.path(
#   Sys.getenv("RENV_PATHS_LIBRARY"),
#   renv:::renv_platform_prefix()
# ))
options(renv.config.autoloader.enabled = FALSE)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::activate()
}

options(bitmapType = "cairo")


### Get plotting to work in VS Code on HPC
## Always prefer cairo bitmaps (avoids X11)
options(bitmapType = "cairo")

## Interactive: try httpgd, else fall back to headless pdf
if (interactive()) {
  # Prefer httpgd (works in VS Code without X11). If missing, use OS GUI.
  if (requireNamespace("httpgd", quietly = TRUE)) {
    # Make httpgd the default device for *all* interactive plots
    options(device = function(...) httpgd::hgd(silent = TRUE, ...))
    # Start a viewer immediately so first plot appears
    try(httpgd::hgd(silent = TRUE), silent = TRUE)
  } else if (.Platform$OS.type == "windows") {
    options(device = "windows")
  } else if (capabilities("aqua")) { # macOS
    options(device = "quartz")
  } else {
    # Last resort: warn instead of silently discarding plots
    packageStartupMessage(
      "No interactive device. Install 'httpgd' for X11-free plotting in VS Code: ",
      "install.packages('httpgd') or remotes::install_github('r-lib/httpgd')"
    )
  }
}

## Non-interactive (e.g. quarto render): set knitr defaults once
if (!interactive() && requireNamespace("knitr", quietly = TRUE)) {
  dev_choice <- if (requireNamespace("ragg", quietly = TRUE)) {
    "ragg_png"
  } else if (requireNamespace("svglite", quietly = TRUE)) {
    "svglite"
  } else if (requireNamespace("Cairo", quietly = TRUE)) {
    "cairo_png"
  } else {
    "png"
  }

  knitr::opts_chunk$set(
    dev = dev_choice,
    fig.width = 7,
    fig.height = 4,
    dpi = 144
  )
}
cat("repository Rprofile loaded\n")
