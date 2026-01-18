### Workflow Configuration Settings ###

parser <- argparse::ArgumentParser()

## Set development vs production mode ##
# Dev mode speeds up workflows by subsetting data for testing and debugging
parser$add_argument("--production",
  type = "logical", default = TRUE,
  help = "Set to true for production mode, false for faster development (default: FALSE)"
)
args <- parser$parse_args()
PRODUCTION <- args$production

# Manual override for interactive sessions
if (rlang::is_interactive()) {
  PRODUCTION <- FALSE  # change this (just for testing)
}

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

# -----Management scenarios config -----
# set to TRUE to use Phase 3 management scenario outputs
USE_PHASE_3_SCENARIOS <- TRUE

phase_3_outdir <- file.path(ccmmf_dir, "modelout", "ccmmf_phase_3_scenarios_20251210")

management_scenarios <- c(
  "baseline",
  "compost",
  "reduced_till",
  "zero_till",
  "reduced_irrig_drip",
  "stacked"
)

model_outdir <- file.path(pecan_outdir, "out")
# override paths if using Phase 3
if (USE_PHASE_3_SCENARIOS) {
  pecan_outdir <- phase_3_outdir
}

# **Variables to extract**
# see docs/workflow_documentation.qmd for complete list of outputs
outputs_to_extract <- c("TotSoilCarb", "AGB")
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

#### Messages ####
msg <- glue::glue(
  "\n\n",
  "##### SETTINGS SUMMARY #####\n\n",
  "Running in {ifelse(PRODUCTION, '**PRODUCTION**', '**DEVELOPMENT**')} mode\n\n",
  "### Directory Settings ###\n",
  "- CCMMF directory: {ccmmf_dir}\n",
  "- data_dir       : {data_dir}\n",
  "- cache_dir      : {cache_dir}\n",
  "- raw_data_dir.  : {raw_data_dir}\n",
  "- pecan_outdir.  : {pecan_outdir}\n",
  "- model_outdir.  : {model_outdir}\n\n",
  "### Other Settings ###\n",
  "- will extract variables: {paste(outputs_to_extract, collapse = ', ')}\n",
  "- ca_albers_crs  : {ca_albers_crs}{if(ca_albers_crs == 3310) ', which is NAD83 / California Albers' else ''}\n"
)
if (!isTRUE(getOption("ccmmf.quiet_banner", FALSE))) {
  PEcAn.logger::logger.info(msg, wrap = FALSE)
}

## Global configuration settings for logging

options(
  # Print all tibble columns
  tibble.width = Inf,
  # Suppress readr::read_csv messages
  readr.show_col_types = FALSE
)

## Source all R scripts in the R/ directory
r_scripts <- list.files(file.path(here::here(), "R"), pattern = "\\.R$", full.names = TRUE)
lapply(r_scripts, source)
