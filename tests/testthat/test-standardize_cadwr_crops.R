library(testthat)

# small synthetic fixture: a parquet of crop records, a parcels gpkg, and a
# PFT map csv. runs standardize_cadwr_crops end to end without the real
# inputs so the columns, typing, and pft mapping stay pinned down.
make_cadwr_fixture <- function(dir) {
  # PFT map columns match LandIQ_cropCode_lookup_table.csv.
  pft_csv <- file.path(dir, "pft_map.csv")
  readr::write_csv(
    tibble::tibble(
      legend_year = c(2021L, 2021L, 2016L),
      PFT = c("woody", "row", "row"),
      CLASS = c("D", "G", "T"),
      SUBCLASS = c(1L, 1L, 17L),
      SUBCLASS_desc = c("apples", "barley", "mixed")
    ),
    pft_csv
  )

  # crop records keyed by parcel_id (numeric on purpose to exercise typing).
  # parcel 100: two apple records (D/1) -> dominant woody, dominant crop apples
  # parcel 200: one barley record (G/1) -> dominant annual, dominant crop barley
  # parcel 300: class T, NA subclass -> subclass coerced to "0", no PFT match
  # centx/centy are EPSG:3310 meters; PCNT 0 means 100% (single use field)
  crops <- tibble::tibble(
    parcel_id = c(100, 100, 200, 300),
    centx = c(-100000, -100000, -50000, 0),
    centy = c(-100000, -100000, -50000, 0),
    CLASS = c("D", "D", "G", "T"),
    SUBCLASS = c(1, 1, 1, NA),
    PCNT = c(0, 100, 0, 0),
    COUNTY = c("Fresno", "Fresno", "Kern", "Kings"),
    year = c(2018L, 2019L, 2018L, 2018L),
    season = c(2L, 2L, 2L, 2L)
  )
  parquet_path <- file.path(dir, "crops.parq")
  arrow::write_parquet(crops, parquet_path)

  # parcel polygons, tiny squares around each centroid, EPSG:3310
  square <- function(cx, cy, half = 50) {
    sf::st_polygon(list(rbind(
      c(cx - half, cy - half),
      c(cx + half, cy - half),
      c(cx + half, cy + half),
      c(cx - half, cy + half),
      c(cx - half, cy - half)
    )))
  }
  parcels <- sf::st_sf(
    parcel_id = c(100, 200, 300),
    geometry = sf::st_sfc(
      square(-100000, -100000),
      square(-50000, -50000),
      square(0, 0),
      crs = 3310L
    )
  )
  parcels_path <- file.path(dir, "parcels.gpkg")
  sf::st_write(parcels, parcels_path, quiet = TRUE)

  list(parquet = parquet_path, parcels = parcels_path, pft = pft_csv)
}

test_that("returns the three named outputs with expected classes", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("sf")
  fx <- make_cadwr_fixture(withr::local_tempdir())

  res <- standardize_cadwr_crops(
    input_parquet = fx$parquet,
    parcels_gpkg = fx$parcels,
    output_dir = NULL,
    pft_mapping_csv = fx$pft,
    write_outputs = FALSE
  )

  expect_named(res, c("sites", "attributes", "site_summary"))
  expect_s3_class(res$sites, "sf")
  expect_s3_class(res$attributes, "tbl_df")
  expect_s3_class(res$site_summary, "tbl_df")
})

test_that("attributes carry required columns and character site_id", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("sf")
  fx <- make_cadwr_fixture(withr::local_tempdir())

  res <- standardize_cadwr_crops(fx$parquet, fx$parcels, NULL, fx$pft, FALSE)
  att <- res$attributes

  required <- c("site_id", "year", "season", "lat", "lon", "county",
                "class", "subclass", "pcnt", "crop", "pft")
  expect_true(all(required %in% names(att)))

  # parcel_id is numeric in the input; site_id must come out character
  expect_type(att$site_id, "character")
  expect_setequal(att$site_id, c("100", "200", "300"))

  # PCNT 0 in DWR data means 100%
  expect_true(all(att$pcnt[att$class == "D"] == 100))
})

test_that("LandIQ PFT mapping resolves woody and annual crops correctly", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("sf")
  fx <- make_cadwr_fixture(withr::local_tempdir())

  att <- standardize_cadwr_crops(fx$parquet, fx$parcels, NULL, fx$pft, FALSE)$attributes

  woody <- att[att$class == "D" & att$subclass == "1", ]
  expect_true(all(woody$pft == "woody perennial crop"))
  expect_true(all(woody$crop == "apples"))

  annual <- att[att$class == "G" & att$subclass == "1", ]
  expect_true(all(annual$pft == "annual crop"))
  expect_true(all(annual$crop == "barley"))

  # NA subclass becomes "0", which has no PFT row, then NA pft
  zero <- att[att$class == "T", ]
  expect_equal(unique(zero$subclass), "0")
  expect_true(all(is.na(zero$pft)))
})

test_that("site summary picks dominant pft and crop per parcel", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("sf")
  fx <- make_cadwr_fixture(withr::local_tempdir())

  ss <- standardize_cadwr_crops(fx$parquet, fx$parcels, NULL, fx$pft, FALSE)$site_summary

  expect_type(ss$site_id, "character")
  expect_true(all(c("dominant_pft", "dominant_crop", "area_ha",
                    "years_observed", "year_min", "year_max") %in% names(ss)))

  p100 <- ss[ss$site_id == "100", ]
  expect_equal(p100$dominant_pft, "woody perennial crop")
  expect_equal(p100$dominant_crop, "apples")
  expect_equal(p100$years_observed, 2L)
  expect_equal(p100$year_min, 2018)
  expect_equal(p100$year_max, 2019)

  p200 <- ss[ss$site_id == "200", ]
  expect_equal(p200$dominant_pft, "annual crop")
  expect_equal(p200$dominant_crop, "barley")
})

test_that("spatial layer keeps character site_id, valid geom, EPSG:3310 area", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("sf")
  fx <- make_cadwr_fixture(withr::local_tempdir())

  sites <- standardize_cadwr_crops(fx$parquet, fx$parcels, NULL, fx$pft, FALSE)$sites

  expect_type(sites$site_id, "character")
  expect_true(all(sf::st_is_valid(sites)))
  expect_true(all(sites$area_ha > 0))
  expect_equal(sf::st_crs(sites)$epsg, 3310L)
})

test_that("subclasses normalize numeric and character inputs to character keys", {
  expect_identical(standardize_subclass(c(NA_real_, 0, 1, 17)),
                   c("0", "0", "1", "17"))
  expect_identical(
    standardize_subclass(c(NA_character_, "**", "", "  ", " ** ", " 01 ", "17.0")),
    c("0", "0", "0", "0", "0", "1", "17")
  )
})

test_that("unrecognized subclasses fail instead of becoming class-level keys", {
  expect_error(standardize_subclass(c("1", "unknown")),
               "unrecognized SUBCLASS values:.*unknown")
})

test_that("LandIQ lookup retains both legends and maps all supported crop PFTs", {
  path <- file.path(withr::local_tempdir(), "lookup.csv")
  readr::write_csv(tibble::tibble(
    legend_year = c(2016L, 2021L, 2021L, 2016L, 2021L),
    PFT = c("woody", "row", "hay", "rice", NA_character_),
    CLASS = c("D", "G", "P", "R", "U"),
    SUBCLASS = c("1", "1", "**", "3", NA_character_),
    SUBCLASS_desc = c("apples", "barley", "hay", "rice", "urban")
  ), path)

  map <- load_landiq_pft_map(path)
  expect_identical(map$class, c("D", "G", "P", "R", "U"))
  expect_identical(map$subclass, c("1", "1", "0", "3", "0"))
  expect_identical(map$crop, c("apples", "barley", "hay", "rice", "urban"))
  expect_identical(map$pft, c("woody perennial crop", rep("annual crop", 3), NA_character_))
})

test_that("equivalent normalized lookup keys collapse to a single mapping", {
  path <- file.path(withr::local_tempdir(), "lookup.csv")
  readr::write_csv(tibble::tibble(
    legend_year = c(2016L, 2021L, 2021L),
    PFT = c("row", "hay", "row"),
    CLASS = c("G", "G", "G"),
    SUBCLASS = c("**", "0", NA_character_),
    SUBCLASS_desc = rep("grain", 3)
  ), path)

  map <- load_landiq_pft_map(path)
  expect_equal(nrow(map), 1L)
  expect_identical(map$class, "G")
  expect_identical(map$subclass, "0")
  expect_identical(map$pft, "annual crop")
})

test_that("conflicting PFTs on normalized lookup keys are rejected", {
  path <- file.path(withr::local_tempdir(), "lookup.csv")
  for (other_pft in c("woody", NA_character_)) {
    readr::write_csv(tibble::tibble(
      legend_year = c(2016L, 2021L),
      PFT = c("row", other_pft),
      CLASS = c("G", "G"),
      SUBCLASS = c("**", "0"),
      SUBCLASS_desc = c("grain", "conflicting entry")
    ), path)

    expect_error(load_landiq_pft_map(path),
                 "conflicting PFTs:.*G/0")
  }
})

test_that("character subclasses join normalized class-level lookup keys", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("sf")
  fx <- make_cadwr_fixture(withr::local_tempdir())
  crops <- arrow::read_parquet(fx$parquet)
  crops$SUBCLASS <- c(" 01 ", "1.0", "**", NA_character_)
  arrow::write_parquet(crops, fx$parquet)
  lookup <- readr::read_csv(fx$pft, show_col_types = FALSE)
  lookup$SUBCLASS <- c("1", "0", "**")
  readr::write_csv(lookup, fx$pft)

  att <- standardize_cadwr_crops(fx$parquet, fx$parcels, NULL, fx$pft, FALSE)$attributes
  expect_equal(nrow(att), 4L)
  expect_identical(att$subclass, c("1", "1", "0", "0"))
  expect_identical(att$pft, c(rep("woody perennial crop", 2), rep("annual crop", 2)))
})

test_that("cropland selection uses PCNT weights and includes equal-weight ties", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("sf")
  fx <- make_cadwr_fixture(withr::local_tempdir())
  template <- arrow::read_parquet(fx$parquet)[1, ]
  lookup <- readr::read_csv(fx$pft, show_col_types = FALSE)
  lookup <- dplyr::bind_rows(lookup, tibble::tibble(
    legend_year = 2021L, PFT = NA_character_, CLASS = "U",
    SUBCLASS = 0, SUBCLASS_desc = "urban"
  ))
  readr::write_csv(lookup, fx$pft)

  # U is a known non-crop code. All cases use the same parcel geometry;
  # only record weights change. G and D map to annual and woody crops.
  cases <- list(
    below = list(classes = c("G", "U"), weights = c(49, 51), pft = NA_character_),
    equal = list(classes = c("G", "U"), weights = c(50, 50), pft = "annual crop"),
    above = list(classes = c("G", "U"), weights = c(51, 49), pft = "annual crop"),
    more_crop_records = list(classes = c("G", "G", "U"), weights = c(20, 20, 60), pft = NA_character_),
    fewer_crop_records = list(classes = c("G", "U", "U"), weights = c(60, 20, 20), pft = "annual crop"),
    combined_crops = list(classes = c("G", "D", "U"), weights = c(30, 25, 45), pft = "annual crop"),
    only_uncropped = list(classes = "U", weights = 100, pft = NA_character_)
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    crops <- template[rep(1L, length(case$classes)), ]
    crops$CLASS <- case$classes
    crops$SUBCLASS <- ifelse(case$classes == "U", 0, 1)
    crops$PCNT <- case$weights
    crops$year <- seq.int(2018L, length.out = nrow(crops))
    arrow::write_parquet(crops, fx$parquet)

    res <- standardize_cadwr_crops(fx$parquet, fx$parcels, NULL, fx$pft, FALSE)
    expect_identical(res$site_summary$site_id, "100", info = case_name)
    expect_identical(res$site_summary$dominant_pft, case$pft, info = case_name)
    expect_identical(res$sites$site_id, "100", info = case_name)
    expect_identical(res$sites$dominant_pft, case$pft, info = case_name)
    expect_equal(nrow(res$attributes), length(case$classes), info = case_name)
  }
})
