library(testthat)

loglevel <- PEcAn.logger::logger.setLevel("OFF")

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

PEcAn.logger::logger.setLevel(loglevel)
