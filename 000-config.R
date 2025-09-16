### Workflow Configuration Settings ###

# Check that we are in the correct working directory
if (!basename(here::here(getwd())) == "downscaling") {
  PEcAn.logger::logger.error("Please run this script from the 'downscaling' directory")
}

## Global configuration settings for logging

options(
  # Print all tibble columns
  tibble.width = Inf,
  # Suppress readr::read_csv messages
  readr.show_col_types = FALSE,
  vroom.show_col_types = FALSE
)

## Set parallel processing options
no_cores <- max(future::availableCores() - 1, 1)
future::plan(future::multicore, workers = no_cores)


# **set ccmmf_dir and pecan_outdir**
# Define the CCMMF directory from environment variable
ccmmf_dir <- Sys.getenv("CCMMF_DIR")
if (ccmmf_dir == "") {
  ccmmf_dir <- "/projectnb2/dietzelab/ccmmf"
}
pecan_outdir <- file.path(ccmmf_dir, "modelout", "ccmmf_phase_2b_mixed_pfts_20250701")
# **Is this a test or production run?**
# Accepts command line argument '--production=false' to set testing mode
#
# Global switch to toggle between fast, small scale runs for development and testing
# and full-scale production runs. Defaults to production mode.
args <- commandArgs(trailingOnly = TRUE)
prod_arg <- grep("^--production=", args, value = TRUE)
if (length(prod_arg) > 0) {
  PRODUCTION <- tolower(sub("^--production=", "", prod_arg)) != "false"
} else {
  PRODUCTION <- TRUE
}

# **Variables to extract**
# see docs/workflow_documentation.qmd for complete list of outputs
# outputs_to_extract <- c(
#   "TotSoilCarb",
#   "AGB"
# )
outputs_to_extract <- "TotSoilCarb"
if (!PRODUCTION) {
  # can subset for testing
  # depending on what part of the workflow you are testing
  outputs_to_extract <- outputs_to_extract[1]
}

### Configuration Settings that can be set to default ###

# Assume consistent directory structure for other directories
data_dir <- file.path(ccmmf_dir, "data")
raw_data_dir <- file.path(ccmmf_dir, "data_raw")
cache_dir <- file.path(ccmmf_dir, "cache")
model_outdir <- file.path(pecan_outdir, "out")

# Misc
set.seed(42)
ca_albers_crs <- 3310

#### Messagees ####
PEcAn.logger::logger.info("\n\n",
  "##### SETTINGS SUMMARY #####\n\n",
  "Running in", ifelse(PRODUCTION, "**production**", "**development**"), "mode\n\n",
  "### Directory Settings ###\n",
  "- CCMMF directory:", ccmmf_dir, "\n",
  "- data_dir       :", data_dir, "\n",
  "- cache_dir      :", cache_dir, "\n",
  "- raw_data_dir.  :", raw_data_dir, "\n",
  "- pecan_outdir.  :", pecan_outdir, "\n",
  "- model_outdir.  :", model_outdir, "\n\n",
  "### Other Settings ###\n",
  "- will extract variables:", paste(outputs_to_extract, collapse = ", "), "\n",
  "- ca_albers_crs  :", ca_albers_crs,
  ifelse(ca_albers_crs == 3310, ", which is NAD83 / California Albers", ""), "\n",
  wrap = FALSE
)

# Source all R scripts in the R/ directory
r_scripts <- list.files(file.path(here::here(), "R"), pattern = "\\.R$", full.names = TRUE)
lapply(r_scripts, source)
