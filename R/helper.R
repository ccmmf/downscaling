# ---- helper instrumentation utilities ---------------------------------------
ts_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
step_timer <- function() list(start = Sys.time())
step_elapsed <- function(timer) as.numeric(difftime(Sys.time(), timer$start, units = "secs"))
log_mem <- function(prefix = "") {
    # Best-effort simple memory usage (RSS) on Linux; fallback to NA
    rss <- tryCatch(
        {
            pid <- Sys.getpid()
            # RSS in kB
            as.numeric(system(paste("ps -o rss= -p", pid), intern = TRUE)) / 1024
        },
        error = function(e) NA_real_
    )
    PEcAn.logger::logger.info(prefix, "Memory ~", ifelse(is.na(rss), "NA", paste0(round(rss, 1), " MB")))
}
overall_timer <- step_timer()
