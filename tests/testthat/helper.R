# testthat helper: configure logging and source all R/ functions before tests

# Silence PEcAn.logger during tests to reduce noise
level <- PEcAn.logger::logger.setLevel("OFF")
withr::defer(PEcAn.logger::logger.setLevel(level))

# Source project functions
r_dir <- here::here("R")
r_scripts <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
for (f in r_scripts) {
    result <- tryCatch(
        source(f, local = TRUE),
        error = function(e) {
            warning("Failed to source ", basename(f), ": ", conditionMessage(e))
            NULL
        }
    )
}
