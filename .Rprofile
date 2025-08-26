## --- Custom R repositories (CRAN + R-universe) ------------------------
options(repos = c(
    pecanproject = "https://pecanproject.r-universe.dev",
    ajlyons      = "https://ajlyons.r-universe.dev", # caladaptr
    CRAN         = "https://cloud.r-project.org"
))

Sys.setenv(R_LIBS_USER = file.path(
  Sys.getenv("RENV_PATHS_LIBRARY"),
  renv:::renv_platform_prefix()
))
options(renv.config.autoloader.enabled = FALSE)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::activate()
}