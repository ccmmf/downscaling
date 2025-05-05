
options(readr.show_col_types = FALSE)
set.seed(42)

ca_albers_crs <- 3310
data_dir <- "/projectnb2/dietzelab/ccmmf/data"
raw_data_dir <- "/projectnb2/dietzelab/ccmmf/data_raw"
modeloutdir <- "/projectnb/dietzelab/ccmmf/ccmmf_phase_1b_20250319064759_14859"

PEcAn.logger::logger.info("`ca_albers_crs` set to 3310, which is NAD83 / California Albers")
PEcAn.logger::logger.info("`data_dir` set to /projectnb2/dietzelab/ccmmf/data")
PEcAn.logger::logger.info("`raw_data_dir` set to /projectnb2/dietzelab/ccmmf/data_raw")
PEcAn.logger::logger.info("`modeloutdir` set to /projectnb/dietzelab/ccmmf/ccmmf_phase_1b_20250319064759_14859")