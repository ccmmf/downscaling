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

list_shards <- function(product) {
  d <- file.path(management_dir, product)
  if (!dir.exists(d)) {
    PEcAn.logger::logger.severe("management product dir not found: ", d)
  }
  list.files(d, pattern = "\\.parq(uet)?$", full.names = TRUE)
}

# open all parquet shards under a product as one arrow dataset.
# lets dplyr filter/select push into the parquet scan; no full bind.
# pass file list (not the dir) because tillage has json sidecars.
open_product <- function(product) {
  arrow::open_dataset(list_shards(product))
}

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

##tillage
# NDTI percent change is rescaled to [0, 1] as a rank and averaged across
# years. parcels with no detected event get 0 for both intensity and freq.
# frequency is introduced feature (ensemble pipeline only uses per-
# event intensity); it carries signal because a parcel tilled 1 year in 7
# is agronomically different from one tilled 6 of 7
PEcAn.logger::logger.info("tillage: fetch + aggregate")
tillage_mgmt <- open_product("tillage") |>
  dplyr::filter(year %in% years_covered) |>
  dplyr::select(site_id, year, ndti_pct_change) |>
  dplyr::collect() |>
  dplyr::mutate(site_id = as.character(site_id)) |>
  dplyr::filter(site_id %in% ids) |>
  dplyr::transmute(
    site_id,
    year,
    ndti_rank = pmax(0, pmin(1, ndti_pct_change / 100))
  ) |>
  dplyr::summarise(
    tillage_rank = mean(ndti_rank, na.rm = TRUE),
    tillage_freq = dplyr::n_distinct(year) / length(years_covered),
    .by = site_id
  )

##phenology
# DOY mean and SD. SD captures interannual stress and management signal that
# the mean washes out
PEcAn.logger::logger.info("phenology: fetch + aggregate")
phen_mgmt <- open_product("phenology") |>
  dplyr::filter(year %in% years_covered) |>
  dplyr::select(site_id, year, leafonday, leafoffday) |>
  dplyr::collect() |>
  dplyr::mutate(site_id = as.character(site_id)) |>
  dplyr::filter(site_id %in% ids) |>
  dplyr::transmute(
    site_id,
    on = lubridate::yday(leafonday),
    off = lubridate::yday(leafoffday)
  ) |>
  dplyr::summarise(
    leafon_doy = mean(on, na.rm = TRUE),
    leafoff_doy = mean(off, na.rm = TRUE),
    leafon_doy_sd = dplyr::if_else(dplyr::n() > 1, stats::sd(on, na.rm = TRUE), 0),
    leafoff_doy_sd = dplyr::if_else(dplyr::n() > 1, stats::sd(off, na.rm = TRUE), 0),
    .by = site_id
  )

##irrigation
# v1.0 method vocab is {canopy, flood}. canopy = above-canopy application
# (sprinkler + drip + microsprinkler collapsed). flood = surface application.
# sampling one ensemble member because method is ensemble invariant per
# parcel in this product. drip vs sprinkler is NOT recoverable from
# this monitoring product (harmonized LandIQ IRR_TYP_PA is also only
# {i, n}, so the distinction is lost upstream).
PEcAn.logger::logger.info("irrigation: fetch + aggregate")
# filter ens_id + date inside arrow (drops 20 ensembles, pre 2016 rows).
# parcel_id %in% ids stays in R; by then the dataset is ~450 MB
irr_mgmt <- open_product("irrigation") |>
  dplyr::filter(
    ens_id == "irr_ens_001",
    date >= as.Date("2016-01-01")
  ) |>
  dplyr::select(parcel_id, method) |>
  dplyr::collect() |>
  dplyr::mutate(site_id = as.character(parcel_id)) |>
  dplyr::filter(site_id %in% ids) |>
  dplyr::summarise(
    dominant = names(sort(table(method), decreasing = TRUE))[1],
    .by = site_id
  ) |>
  dplyr::mutate(
    irr_canopy = as.integer(dominant == "canopy"),
    irr_flood = as.integer(dominant == "flood")
  ) |>
  dplyr::select(site_id, irr_canopy, irr_flood)

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
