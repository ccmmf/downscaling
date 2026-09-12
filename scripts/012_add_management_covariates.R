source("000-config.R")
PEcAn.logger::logger.info("*** Adding Management Covariates ***")

# reads staged monitoring products from `management_dir` (set in 000-config.R)
# and aggregates to per-parcel summary features for clustering.
#
# features produced:
# tillage_rank      NDTI-based tillage intensity rank in [0, 1]
# tillage_freq      share of monitored years with a detected tillage event
# leafon_doy        mean green-up DOY across monitored years
# leafoff_doy       mean senescence DOY across monitored years
# leafon_doy_sd     SD of green-up DOY (interannual variability signal)
# leafoff_doy_sd    SD of senescence DOY
# irr_canopy        1 if parcel's dominant monitored irrigation is above-canopy
# irr_flood         1 if parcel's dominant monitored irrigation is surface
#
# caveats for downstream consumers:
# * tillage_rank is a ranking index, NOT a physical tillage fraction.
#   NDTI pct-change is confounded by soil moisture (Zheng et al. 2013
#   J. Soil Water Conservation 68:120-128) and by residual green cover.
# * irr_canopy vs irr_flood cannot distinguish drip from above-canopy
#   sprinkler; both land in canopy. CA drip/microsprinkler adoption in
#   2018 was ~48% of irrigated acres (CA DWR) so this is a real limit
#   of the monitoring product, not of our ingest.

site_covariates <- readr::read_csv(
  file.path(data_dir, "site_covariates.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(site_id = as.character(site_id))

# drop any mgmt cols left from a prior 012 run so the left_joins below
# don't make tillage_rank.x / tillage_rank.y duplicates.
mgmt_cols <- c("tillage_rank", "tillage_freq",
               "leafon_doy", "leafoff_doy",
               "leafon_doy_sd", "leafoff_doy_sd",
               "irr_canopy", "irr_flood")
site_covariates <- site_covariates |>
  dplyr::select(-dplyr::any_of(mgmt_cols))

ids <- site_covariates$site_id

years_covered <- c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)

# each product is read from management_dir/<product>/<version>/, pinned in 000-config.R.
# the queries below glob that directory, so check it resolves rather than returning
# zero rows and giving every parcel an NA covariate.
mgmt_product_dir <- function(product) {
  d <- file.path(management_dir, product, mgmt_versions[[product]])
  if (!dir.exists(d)) {
    PEcAn.logger::logger.severe("management product dir not found: ", d)
  }
  if (length(list.files(d, pattern = "\\.parq")) == 0) {
    PEcAn.logger::logger.severe("no parquet files in management product dir: ", d)
  }
  d
}

##tillage
# NDTI percent change is rescaled to [0, 1] as a rank and averaged across
# years. parcels with no detected event get 0 for both intensity and freq.
# frequency is introduced feature (ensemble pipeline only uses per-
# event intensity); it carries signal because a parcel tilled 1 year in 7
# is agronomically different from one tilled 6 of 7.
# isnan guard is load bearing: GREATEST/LEAST don't propagate NaN.
PEcAn.logger::logger.info("tillage: fetch + aggregate")
till_dbdir <- file.path(Sys.getenv("TMPDIR", "/tmp"), paste0("till_", Sys.getpid(), ".duckdb"))
till_conn <- DBI::dbConnect(duckdb::duckdb(dbdir = till_dbdir))
on.exit({
  DBI::dbDisconnect(till_conn, shutdown = TRUE)
  unlink(till_dbdir)
}, add = TRUE)
DBI::dbWriteTable(till_conn, "wanted_ids",
                  data.frame(site_id_int = as.integer(ids)),
                  temporary = TRUE)
till_dir <- mgmt_product_dir("tillage")
tillage_mgmt <- DBI::dbGetQuery(till_conn, sprintf("
  WITH src AS (
    SELECT CAST(site_id AS BIGINT) AS site_id_int,
           year,
           CASE WHEN ndti_pct_change IS NULL OR isnan(ndti_pct_change) THEN NULL
                ELSE GREATEST(0.0, LEAST(1.0, CAST(ndti_pct_change AS DOUBLE) / 100.0))
           END AS ndti_rank
    FROM read_parquet('%s/*.parquet')
    WHERE year IN (%s)
  ),
  filtered AS (
    SELECT s.* FROM src s JOIN wanted_ids w USING (site_id_int)
  )
  SELECT
    CAST(site_id_int AS VARCHAR) AS site_id,
    AVG(ndti_rank) AS tillage_rank,
    CAST(COUNT(DISTINCT year) AS DOUBLE) / %d.0 AS tillage_freq
  FROM filtered
  GROUP BY site_id_int
", till_dir, paste(years_covered, collapse = ", "), length(years_covered)))

##phenology
# DOY mean and SD. SD captures interannual stress and management signal that
# the mean washes out. duckdb does the mean / sd directly. the source isn't
# huge (~4M rows) but the R side yday and grouped sd were the slow part
PEcAn.logger::logger.info("phenology: fetch + aggregate")
phen_dbdir <- file.path(Sys.getenv("TMPDIR", "/tmp"), paste0("phen_", Sys.getpid(), ".duckdb"))
phen_conn <- DBI::dbConnect(duckdb::duckdb(dbdir = phen_dbdir))
on.exit({
  DBI::dbDisconnect(phen_conn, shutdown = TRUE)
  unlink(phen_dbdir)
}, add = TRUE)
DBI::dbWriteTable(phen_conn, "wanted_ids",
                  data.frame(site_id_int = as.integer(ids)),
                  temporary = TRUE)
phen_dir <- mgmt_product_dir("phenology")
phen_mgmt <- DBI::dbGetQuery(phen_conn, sprintf("
  WITH src AS (
    SELECT CAST(site_id AS BIGINT) AS site_id_int,
           DAYOFYEAR(CAST(leafonday  AS DATE)) AS doy_on,
           DAYOFYEAR(CAST(leafoffday AS DATE)) AS doy_off
    FROM read_parquet('%s/*.parq*')
    WHERE year IN (%s)
  ),
  filtered AS (
    SELECT s.* FROM src s JOIN wanted_ids w USING (site_id_int)
  )
  SELECT
    CAST(site_id_int AS VARCHAR) AS site_id,
    AVG(doy_on)  AS leafon_doy,
    AVG(doy_off) AS leafoff_doy,
    CASE WHEN COUNT(*) > 1 THEN STDDEV_SAMP(doy_on)  ELSE 0 END AS leafon_doy_sd,
    CASE WHEN COUNT(*) > 1 THEN STDDEV_SAMP(doy_off) ELSE 0 END AS leafoff_doy_sd
  FROM filtered GROUP BY site_id_int
", phen_dir, paste(years_covered, collapse = ", ")))

##irrigation
# dominant irrigation method per parcel. canopy = above ground (sprinkler,
# drip, microsprinkler all lumped, the product doesn't split them and
# landiq's IRR_TYP_PA is also just i / n upstream). flood = surface. the
# source is ~600M rows across 20 ensembles, too big to pull into R, so the
# per parcel mode happens in duckdb and only the ~600k rows come back.
PEcAn.logger::logger.info("irrigation: fetch + aggregate")
irr_dbdir <- file.path(Sys.getenv("TMPDIR", "/tmp"), paste0("irr_", Sys.getpid(), ".duckdb"))
irr_conn <- DBI::dbConnect(duckdb::duckdb(dbdir = irr_dbdir))
on.exit({
  DBI::dbDisconnect(irr_conn, shutdown = TRUE)
  unlink(irr_dbdir)
}, add = TRUE)
DBI::dbWriteTable(irr_conn, "wanted_ids",
                  data.frame(site_id_int = as.integer(ids)),
                  temporary = TRUE)
irr_dir <- mgmt_product_dir("irrigation")
irr_mgmt <- DBI::dbGetQuery(irr_conn, sprintf("
  WITH src AS (
    SELECT CAST(parcel_id AS BIGINT) AS site_id_int, method
    FROM read_parquet('%s/*.parquet')
    WHERE date >= DATE '2016-01-01'
  ),
  filtered AS (
    SELECT s.site_id_int, s.method
    FROM src s JOIN wanted_ids w USING (site_id_int)
  ),
  counts AS (
    SELECT site_id_int, method, COUNT(*) AS n
    FROM filtered GROUP BY site_id_int, method
  ),
  ranked AS (
    SELECT site_id_int, method,
      ROW_NUMBER() OVER (PARTITION BY site_id_int ORDER BY n DESC, method) AS rk
    FROM counts
  )
  SELECT
    CAST(site_id_int AS VARCHAR) AS site_id,
    CAST(method = 'canopy' AS INTEGER) AS irr_canopy,
    CAST(method = 'flood'  AS INTEGER) AS irr_flood
  FROM ranked WHERE rk = 1
", irr_dir))

##join + impute
# tillage absent -> 0 for both (no event detected = no tillage).
# irrigation absent -> 0 for both (no event detected).
# phenology absent -> NA left intact; 020's scale() drops those rows.
# phenology SD is NA when only one year of observations; leave NA.
site_covariates <- site_covariates |>
  dplyr::left_join(tillage_mgmt, by = "site_id") |>
  dplyr::left_join(phen_mgmt, by = "site_id") |>
  dplyr::left_join(irr_mgmt, by = "site_id") |>
  dplyr::mutate(
    tillage_rank = tidyr::replace_na(tillage_rank, 0),
    tillage_freq = tidyr::replace_na(tillage_freq, 0),
    irr_canopy = tidyr::replace_na(irr_canopy, 0L),
    irr_flood = tidyr::replace_na(irr_flood, 0L)
  )

coverage <- tibble::tibble(
  feature = c("tillage_rank", "tillage_freq",
              "leafon_doy", "leafoff_doy",
              "leafon_doy_sd", "leafoff_doy_sd",
              "irr_canopy", "irr_flood"),
  n = c(
    sum(site_covariates$tillage_rank > 0),
    sum(site_covariates$tillage_freq > 0),
    sum(!is.na(site_covariates$leafon_doy)),
    sum(!is.na(site_covariates$leafoff_doy)),
    sum(!is.na(site_covariates$leafon_doy_sd)),
    sum(!is.na(site_covariates$leafoff_doy_sd)),
    sum(site_covariates$irr_canopy == 1),
    sum(site_covariates$irr_flood == 1)
  ),
  total = nrow(site_covariates)
) |>
  dplyr::mutate(pct = round(n / total * 100, 1))
PEcAn.logger::logger.info("management coverage:")
print(knitr::kable(coverage, format = "simple"))

readr::write_csv(site_covariates, file.path(data_dir, "site_covariates.csv"))
PEcAn.logger::logger.info("management covariates written")
