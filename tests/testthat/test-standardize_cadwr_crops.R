library(testthat)

# small synthetic fixture: a parquet of crop records, a parcels gpkg, and a
# PFT map csv. runs standardize_cadwr_crops end to end without the real
# inputs so the columns, typing, and pft mapping stay pinned down.
make_cadwr_fixture <- function(dir) {
  # PFT map columns match CARB_PFTs_table.csv:
  # crop_type -> class, crop_code -> subclass, crop_desc, pft_group
  pft_csv <- file.path(dir, "pft_map.csv")
  readr::write_csv(
    tibble::tibble(
      crop_type = c("D", "G", "T"),
      crop_code = c(1L, 1L, 17L),
      crop_desc = c("apples", "barley", "mixed"),
      pft_group = c("woody", "herbaceous", "herbaceous")
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

test_that("PFT mapping resolves woody and herbaceous correctly", {
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

  # NA subclass becomes "0", which has no PFT row,then NA pft
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
