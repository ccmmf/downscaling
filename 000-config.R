### Workflow Configuration Settings ###

parser <- argparse::ArgumentParser()

## Run mode: production (full), dev (subset), demo (single slice) ##
parser$add_argument("--mode",
  type = "character", default = "production",
  choices = c("production", "dev", "demo"),
  help = "Run mode: production (full), dev (subsetted for fast iteration), demo (single slice for walkthroughs)"
)
args <- parser$parse_args()
MODE <- args$mode
PRODUCTION <- MODE == "production"
DEMO       <- MODE == "demo"

# Interactive sessions always run in production mode.
# TODO: interactive mode should default to "dev"
if (rlang::is_interactive()) {
  PRODUCTION <- TRUE
  DEMO       <- FALSE
  MODE       <- "production"
}

## Set parallel processing options
no_cores <- max(future::availableCores() - 1, 1)
future::plan(future::multicore, workers = no_cores)

# **set ccmmf_dir and pecan_outdir**
# Define the CCMMF directory from environment variable
ccmmf_dir <- Sys.getenv("CCMMF_DIR")
if (ccmmf_dir == "") {
  ccmmf_dir <- "/projectnb2/dietzelab/ccmmf"
  if (!dir.exists(ccmmf_dir)) {
    # Fallback to repository root for local development
    ccmmf_dir <- here::here()
  }
}
# Phase 3 management scenario model outputs (SIPNET v2 with N-cycle)
pecan_outdir <- file.path(ccmmf_dir, "modelout", "ccmmf_phase_3_scenarios_v2_n2o_ch4")

management_scenarios <- c(
  "baseline",
  "compost",
  "reduced_till",
  "zero_till",
  "reduced_irrig_drip",
  "stacked"
)

# **Variables to extract**
# see docs/workflow_documentation.qmd for complete list of outputs
outputs_to_extract <- c("TotSoilCarb", "AGB", "N2O_flux", "CH4_flux")
if (!PRODUCTION) {
  # Dev mode: subset to one variable to keep iterations quick
  outputs_to_extract <- outputs_to_extract[1]
}
if (DEMO) {
  # Demo: one variable, one scenario. Site/ensemble/year slicing happens in 030.
  outputs_to_extract   <- "TotSoilCarb"
  management_scenarios <- "baseline"
}

### Configuration Settings that can be set to default ###

##site selection outputs + management staging
# stage under usr/akash to avoid clobbering the shared data dir artifacts used by any prev runs
# management_s3/ mirrors s3://carb/management/<product>/<version>/. the version is in
# the path so a run record can state what was read; bump mgmt_versions to move to a new
# one rather than overwriting in place.
# data/ and cache/ are specific to a parcel basis, so the consolidated build writes to
# its own directories and leaves the v4.1 artifacts intact.
akash_dir      <- "/projectnb/dietzelab/ccmmf/usr/akash"
data_dir       <- file.path(akash_dir, "data_consolidated_v4.1.2")
cache_dir      <- file.path(akash_dir, "cache_consolidated_v4.1.2")
management_dir <- file.path(akash_dir, "management_s3")
mgmt_versions  <- c(phenology = "v2.0", tillage = "v1.0", irrigation = "v1.1")

# management_dir is deliberately not created here: it is staged from s3, and a missing
# one means the stage step never ran.
for (d in c(data_dir, cache_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

raw_data_dir <- file.path(ccmmf_dir, "data_raw")
model_outdir <- pecan_outdir

##parcel basis (009)
# v4.1.2 adds gap-filled crop identity and ADOY; its geometry is shared with v4.1.
landiq_version <- "v4.1.2"

# design-point clustering + subsampling (020, 022).
# pool_size is the clustered-site pool (frozen artifact).
# n_design is the subsample used for SIPNET runs.
# floors are per-PFT minimums for the pool allocation.
# subsample_threshold triggers two-stage clustering when PFT population > this.
if (PRODUCTION) {
  pool_size <- 10000L
  n_design <- 1000L
  pool_floors <- c("annual crop" = 700L, "woody perennial crop" = 300L)
  subsample_threshold <- 20000L
} else {
  pool_size <- 500L
  n_design <- 100L
  pool_floors <- c("annual crop" = 35L, "woody perennial crop" = 15L)
  subsample_threshold <- 5000L
}

# Misc
set.seed(42)
ca_albers_crs <- "EPSG:3310"

if (terra::is.lonlat(ca_albers_crs)) {
  PEcAn.logger::logger.severe("`ca_albers_crs` must remain a projected CRS (EPSG:3310).")
}

ca_albers_info <- terra::crs(ca_albers_crs, describe = TRUE)
if (is.null(ca_albers_info$code) || ca_albers_info$code != 3310) {
  PEcAn.logger::logger.severe("`ca_albers_crs` is reserved for California's standard NAD83 / California Albers (EPSG:3310).")
}
ca_albers_name <- ca_albers_info[["name"]]

msg <- glue::glue(
  "\n\n",
  "Settings summary\n\n",
  "Running in {toupper(MODE)} mode\n\n",
  "Directory settings\n",
  "- CCMMF directory: {ccmmf_dir}\n",
  "- data_dir       : {data_dir}\n",
  "- cache_dir      : {cache_dir}\n",
  "- raw_data_dir   : {raw_data_dir}\n",
  "- pecan_outdir   : {pecan_outdir}\n\n",
  "Other settings\n",
  "- scenarios      : {paste(management_scenarios, collapse = ', ')}\n",
  "- variables      : {paste(outputs_to_extract, collapse = ', ')}\n",
  "- ca_albers_crs  : {ca_albers_crs}{if(!is.na(ca_albers_name)) paste0(' (', ca_albers_name, ')') else ''}\n"
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
