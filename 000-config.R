
options(readr.show_col_types = FALSE)
set.seed(42)

ca_albers_crs <- 3310

ccmmf_dir    <- "/projectnb2/dietzelab/ccmmf"
data_dir     <- file.path(ccmmf_dir, "data")
raw_data_dir <- file.path(ccmmf_dir, "data_raw")
cache_dir    <- file.path(ccmmf_dir, "cache")
modeloutdir  <- file.path(ccmmf_dir, "ccmmf_phase_1b_20250319064759_14859")

PEcAn.logger::logger.info("`ca_albers_crs` set to 3310, which is NAD83 / California Albers")
PEcAn.logger::logger.info("CCMMF directory set to ", ccmmf_dir)
PEcAn.logger::logger.info("`data_dir` set to ", data_dir)
PEcAn.logger::logger.info("`cache_dir` set to ", cache_dir)
PEcAn.logger::logger.info("`raw_data_dir` set to ", raw_data_dir)
PEcAn.logger::logger.info("`modeloutdir` set to ", modeloutdir)