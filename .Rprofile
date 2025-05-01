## --- Custom R repositories (CRAN + R-universe) ------------------------
options(repos = c(
    pecanproject = "https://pecanproject.r-universe.dev",
    ajlyons      = "https://ajlyons.r-universe.dev", # caladaptr
    CRAN         = "https://cloud.r-project.org"
))

# Pick up the values set in .Renviron (or fall back to ../ccmmf)
data_dir <- Sys.getenv("CCMMF_DIR", unset = "../ccmmf")
renv_cache_dir <- Sys.getenv("RENV_PATHS_CACHE",
    unset = file.path(data_dir, "cache/renv")
)

# Point renv to the shared cache *before* activation
Sys.setenv(RENV_PATHS_CACHE = renv_cache_dir)

source("renv/activate.R")
