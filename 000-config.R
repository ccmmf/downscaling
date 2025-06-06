### Workflow Configuration Settings ### 

# **set ccmmf_dir and pecan_outdir**
# Define the CCMMF directory from environment variable
ccmmf_dir <- Sys.getenv("CCMMF_DIR")
pecan_outdir <- file.path(ccmmf_dir, "modelout", "ccmmf_phase_2a_DRAFT_output_20250520")

# **Is this a test or production run?**
PRODUCTION <- FALSE

# **Variables to extract**
# see docs/workflow_documentation.qmd for complete list of outputs
outputs_to_extract <- c(
    "TotSoilCarb",
    "AGB"
)

if(!PRODUCTION) {
  outputs_to_extract <- outputs_to_extract[1]
}

### Configuration Settings that can be set to default ###

# Assume consistent directory structure for other directories
data_dir     <- file.path(ccmmf_dir, "data")
raw_data_dir <- file.path(ccmmf_dir, "data_raw")
cache_dir <- file.path(ccmmf_dir, "cache")
model_outdir  <- file.path(pecan_outdir, "out")

# Suppress readr::read_csv messages
options(readr.show_col_types = FALSE)

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

