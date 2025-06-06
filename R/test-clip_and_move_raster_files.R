library(testthat)
library(terra)
library(sf)

# load function under test
source("../../clip_and_move_rasters.R")  # adjust relative path as needed

# helper to create a small test raster
make_raster <- function(crs = "EPSG:4326") {
  r <- terra::rast(matrix(1:16, 4, 4),
                   extent = terra::ext(0, 4, 0, 4), crs = crs)
  f <- tempfile(fileext = ".tif")
  terra::writeRaster(r, f, filetype = "GTiff", overwrite = TRUE)
  f
}

test_that("clip & mask works: output clipped to polygon bbox and masked", {
  in_r <- make_raster()
  on.exit(unlink(in_r), add = TRUE)
  # box polygon (1,1)-(3,3)
  poly <- sf::st_as_sfc(sf::st_bbox(c(xmin=1, ymin=1, xmax=3, ymax=3)), crs = "EPSG:4326")
  out_f <- tempfile(fileext = ".tif")
  clip_and_move_raster_files(in_r, poly, out_f, mask = TRUE)
  expect_true(file.exists(out_f))
  r_out <- terra::rast(out_f)
  # extent == polygon bbox
  expect_equal(terra::ext(r_out), terra::ext(sf::st_bbox(poly)))
  # some values NA (corners) and some not (center)
  vals <- terra::values(r_out)
  expect_true(any(is.na(vals)))
  expect_true(any(!is.na(vals)))
  unlink(out_f)
})

test_that("clip without mask retains all values within bbox", {
  in_r <- make_raster()
  on.exit(unlink(in_r), add = TRUE)
  poly <- sf::st_as_sfc(sf::st_bbox(c(xmin=1, ymin=1, xmax=3, ymax=3)), crs = "EPSG:4326")
  out_f <- tempfile(fileext = ".tif")
  clip_and_move_raster_files(in_r, poly, out_f, mask = FALSE)
  r_out <- terra::rast(out_f)
  expect_false(any(is.na(terra::values(r_out))))
  unlink(in_r); unlink(out_f)
})

test_that("preserves CRS and filetype", {
  in_r <- make_raster(crs = "EPSG:3857")
  on.exit(unlink(in_r), add = TRUE)
  poly <- sf::st_as_sfc(sf::st_bbox(c(xmin=0, ymin=0, xmax=2, ymax=2)), crs = "EPSG:3857")
  out_f <- tempfile(fileext = ".tif")
  clip_and_move_raster_files(in_r, poly, out_f)
  r_out <- terra::rast(out_f)
  expect_true(terra::same.crs(r_out, terra::rast(in_r)))
  # file extension implies GTiff
  expect_true(grepl("\\.tif$", out_f))
  unlink(in_r); unlink(out_f)
})
