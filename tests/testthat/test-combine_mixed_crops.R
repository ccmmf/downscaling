library(testthat)

test_that("weighted mixing basic case", {
    res <- combine_mixed_crops(
        woody_value = 100, annual_value = 50,
        annual_cover = 0.2, woody_cover = 0.8,
        method = "weighted"
    )
    expected <- 0.8 * 100 + 0.2 * 50
    expect_true(abs(res - expected) < 1e-10)
})

test_that("weighted cover sum mismatch yields NA output", {
    res <- combine_mixed_crops(
        woody_value = 100, annual_value = 50,
        annual_cover = 0.25, woody_cover = 0.8,
        method = "weighted"
    )
    expect_true(is.na(res))
})

test_that("incremental mixing basic case", {
    res <- combine_mixed_crops(
        woody_value = 200, annual_value = 220, annual_init = 200,
        annual_cover = 0.3, woody_cover = 1.0, method = "incremental"
    )
    expected <- 200 + 0.3 * (220 - 200)
    expect_true(abs(res - expected) < 1e-10)
})

test_that("incremental requires annual_init", {
    testthat::expect_error(
        res <- combine_mixed_crops(
            woody_value = 200, annual_value = 220,
            annual_cover = 0.3, woody_cover = 1.0,
            method = "incremental"
        ),
        "annual_init is required but missing"
    )
})

test_that("incremental requires woody_cover == 1", {
    testthat::expect_error(
        res <- combine_mixed_crops(
            woody_value = 200, annual_value = 220, annual_init = 200,
            annual_cover = 0.3, woody_cover = 0.9,
            method = "incremental"
        ),
        "woody_cover must be 1"
    )
})

test_that("NA inputs rejected", {
    expect_error(
        res <- combine_mixed_crops(
            woody_value = NA_real_, annual_value = 50,
            annual_cover = 0.2, woody_cover = 0.8,
            method = "weighted"
        ),
        "NA values not allowed in inputs"
    )
})

test_that("length mismatch rejected", {
    expect_error(
        res <- combine_mixed_crops(
            woody_value = 1:2, annual_value = 1:3,
            annual_cover = 0.2, woody_cover = 0.8,
            method = "weighted"
        ),
        "Can't recycle"
    )
})

test_that("weighted mixing works with vector inputs", {
    res <- combine_mixed_crops(
        woody_value = c(100, 200, 150),
        annual_value = c(50, 100, 75),
        annual_cover = 0.2,
        woody_cover = 0.8,
        method = "weighted"
    )
    expected <- 0.8 * c(100, 200, 150) + 0.2 * c(50, 100, 75)
    expect_equal(res, expected, tolerance = 1e-10)
    expect_length(res, 3)
})

test_that("incremental mixing handles delta = 0 (annual_init equals annual_value)", {
    res <- combine_mixed_crops(
        woody_value = 200,
        annual_value = 150,  # same as annual_init
        annual_init = 150,
        annual_cover = 0.5,
        woody_cover = 1.0,
        method = "incremental"
    )
    # delta = 150 - 150 = 0, so result should equal woody_value
    expect_equal(res, 200)
})

test_that("incremental accepts woody_cover within tolerance of 1", {
    # woody_cover = 0.999 should be accepted (within 0.1% of 1)
    res <- combine_mixed_crops(
        woody_value = 200,
        annual_value = 220,
        annual_init = 200,
        annual_cover = 0.3,
        woody_cover = 0.999,  # within 1e-3 tolerance
        method = "incremental"
    )
    expected <- 200 + 0.3 * (220 - 200)
    expect_equal(res, expected, tolerance = 1e-10)
})

test_that("incremental rejects woody_cover outside tolerance of 1", {
    # woody_cover = 0.99 is outside the 0.1% tolerance
    expect_error(
        combine_mixed_crops(
            woody_value = 200,
            annual_value = 220,
            annual_init = 200,
            annual_cover = 0.3,
            woody_cover = 0.99,  # outside tolerance (1% away)
            method = "incremental"
        ),
        "woody_cover must be 1"
    )
})