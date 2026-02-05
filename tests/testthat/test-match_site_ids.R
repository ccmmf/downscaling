context("match_site_ids_by_location")
test_that("match_site_ids_by_location returns full mapping when all IDs exist", {
  target <- data.frame(
    site_id = c("A", "B"),
    lat = c(34.0, 35.0),
    lon = c(-118.0, -119.0)
  )
  reference <- target

  map <- match_site_ids_by_location(target, reference)
  expect_s3_class(map, "data.frame")
  expect_equal(nrow(map), nrow(target))
  expect_equal(map$target_site_id, target$site_id)
  expect_equal(map$matched_site_id, target$site_id)
  expect_true(all(map$distance_m == 0))

  updated <- update_site_ids_by_location(target, reference)
  expect_equal(updated$site_id, target$site_id)
})

test_that("update_site_ids_by_location replaces missing IDs by nearest", {
  # B is missing from reference; it's closest to Y
  target <- data.frame(
    site_id = c("A", "B"),
    lat = c(34.0000, 35.3000),
    lon = c(-118.0000, -119.4000)
  )
  reference <- data.frame(
    site_id = c("A", "X", "Y"),
    lat = c(34.0000, 35.3000, 35.3000),
    lon = c(-118.0000, -119.5000, -119.4010) # Y is ~90m from B
  )

  map <- match_site_ids_by_location(target, reference)
  expect_s3_class(map, "data.frame")
  expect_equal(nrow(map), 2)
  # A remains A with zero distance
  rowA <- map[map$target_site_id == "A", ]
  expect_equal(rowA$matched_site_id, "A")
  expect_true(rowA$distance_m == 0)
  # B maps to Y with positive distance
  rowB <- map[map$target_site_id == "B", ]
  expect_equal(rowB$matched_site_id, "Y")
  expect_true(is.numeric(rowB$distance_m))
  expect_gt(rowB$distance_m, 0)

  updated <- update_site_ids_by_location(target, reference)
  expect_equal(updated$site_id, c("A", "Y"))
})

test_that("match_site_ids_by_location map_all=TRUE maps all rows and includes close class", {
  target <- data.frame(
    site_id = c("A", "B"),
    lat = c(34.0, 35.0),
    lon = c(-118.0, -119.0)
  )
  reference <- data.frame(
    site_id = c("A", "B"),
    lat = c(34.0, 35.0),
    lon = c(-118.0, -119.0)
  )

  map_all <- match_site_ids_by_location(target, reference, map_all = TRUE)
  expect_s3_class(map_all, "data.frame")
  expect_equal(nrow(map_all), 2)
  expect_true(all(c("target_site_id", "matched_site_id", "target_lat", "target_lon", "ref_lat", "ref_lon", "distance_m", "close") %in% names(map_all)))
  expect_equal(sort(map_all$target_site_id), c("A", "B"))
  expect_equal(map_all$target_site_id, map_all$matched_site_id)
  # identical coords should produce zero distance, even when map_all=TRUE
  expect_true(all(map_all$distance_m == 0))
})

test_that("update_site_ids_by_location supports custom column names", {
  target <- data.frame(
    id = c("A", "B"),
    latitude = c(34.0000, 35.3000),
    longitude = c(-118.0000, -119.4000)
  )
  reference <- data.frame(
    id = c("A", "X", "Y"),
    latitude = c(34.0000, 35.3000, 35.3000),
    longitude = c(-118.0000, -119.5000, -119.4010)
  )

  updated <- update_site_ids_by_location(
    target_df = target,
    reference_df = reference,
    id_col = "id",
    target_lat_col = "latitude",
    target_lon_col = "longitude",
    reference_id_col = "id",
    reference_lat_col = "latitude",
    reference_lon_col = "longitude"
  )
  expect_equal(updated$id, c("A", "Y"))
})

test_that("match_site_ids_by_location errors when exceeding max_distance", {
  # Construct a far-away reference so distance >> 100 m
  target <- data.frame(
    site_id = c("A"),
    lat = c(34.0000),
    lon = c(-118.0000)
  )
  reference <- data.frame(
    site_id = c("Z"),
    lat = c(35.0000), # ~111 km north
    lon = c(-118.0000)
  )

  expect_error(
    match_site_ids_by_location(target, reference, map_all = TRUE, max_distance = 100),
    regexp = "exceed max_distance|exceed max_distance of"
  )
})

test_that("close classification reflects ~90m as 'very close (<=100m)'", {
  testthat::skip_if_not_installed("terra")

  target <- data.frame(
    site_id = c("A", "B"),
    lat = c(34.0000, 35.3000),
    lon = c(-118.0000, -119.4000)
  )
  reference <- data.frame(
    site_id = c("A", "X", "Y"),
    lat = c(34.0000, 35.3000, 35.3000),
    lon = c(-118.0000, -119.5000, -119.4010)
  )

  map <- match_site_ids_by_location(target, reference)
  rowB <- map[map$target_site_id == "B", ]
  expect_true(rowB$distance_m > 0)
  expect_equal(rowB$close, "very close (<=100m)")
})

test_that("map_all computes distances for ID-matched rows", {
  testthat::skip_if_not_installed("terra")

  target <- data.frame(
    site_id = c("A"),
    lat = c(34.0000),
    lon = c(-118.0000)
  )
  reference <- data.frame(
    site_id = c("A"),
    lat = c(34.0000),
    lon = c(-118.0009) # ~82 m west at this latitude
  )

  map_default <- match_site_ids_by_location(target, reference, map_all = FALSE)
  expect_equal(nrow(map_default), 1)
  expect_equal(map_default$matched_site_id, "A")
  expect_true(map_default$distance_m == 0)

  map_all <- match_site_ids_by_location(target, reference, map_all = TRUE)
  expect_equal(nrow(map_all), 1)
  expect_equal(map_all$matched_site_id, "A")
  expect_true(map_all$distance_m > 0)
  expect_equal(map_all$close, "very close (<=100m)")
})
