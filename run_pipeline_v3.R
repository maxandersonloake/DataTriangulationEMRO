# ================================================================
# CI driver for the PAK IDSR data pipeline.
# ----------------------------------------------------------------
# Sources the main extraction script and runs extract_PAK_data_main(),
# capturing every message()/warning() raised along the way (fuzzy-match
# notices, unresolved-name errors, header-parsing notes, etc.) plus any
# NEW lines written to Data/failed_reports.txt during this run.
#
# Writes a plain-text summary to Data/run_log.txt. This file is what the
# GitHub Actions workflow emails out -- see .github/workflows/update_data.yml.
#
# Run from the repo root: Rscript run_pipeline.R
# ================================================================

source("1_1_DownloadData_PAK_v3.R")

cases_file      <- "Data/PAK_IDSR_Data.csv"
compliance_file <- "Data/PAK_IDSR_Compliance.csv"
failed_log      <- "Data/failed_reports.txt"
run_log         <- "Data/run_log.txt"

# failed_reports.txt accumulates across every run, not just today's -- only
# the lines added DURING this run should go in today's summary.
failed_before <- if (file.exists(failed_log)) length(readLines(failed_log)) else 0

captured <- character(0)

msg_handler <- function(cond) {
  captured <<- c(captured, paste0("[MESSAGE] ", trimws(conditionMessage(cond))))
  invokeRestart("muffleMessage")
}
warn_handler <- function(cond) {
  captured <<- c(captured, paste0("[WARNING] ", trimws(conditionMessage(cond))))
  invokeRestart("muffleWarning")
}

# withCallingHandlers intercepts message()/warning() WITHOUT stopping
# execution (unlike tryCatch, which would abort the whole run on the first
# one) -- exactly what's needed here, since most of these are informational
# notices the pipeline is designed to keep going after.
result <- withCallingHandlers(
  extract_PAK_data_main(),
  message = msg_handler,
  warning = warn_handler
)

failed_after <- if (file.exists(failed_log)) readLines(failed_log) else character(0)
new_failures <- if (length(failed_after) > failed_before) {
  failed_after[(failed_before + 1):length(failed_after)]
} else {
  character(0)
}

lines <- c(
  paste0("PAK IDSR data pipeline run -- ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  paste0("New weeks processed: ", result$new),
  paste0("Changed weeks reprocessed: ", result$changed),
  ""
)

if (length(new_failures) > 0) {
  lines <- c(lines,
             paste0("=== ERRORS (", length(new_failures), ") -- bulletin(s) could not be processed ==="),
             new_failures, "")
}

if (length(captured) > 0) {
  lines <- c(lines,
             paste0("=== WARNINGS (", length(captured), ") -- review recommended ==="),
             "(fuzzy name matches, unresolved diseases/regions, header/row parsing notes, etc.)",
             "",
             captured, "")
}

if (length(new_failures) == 0 && length(captured) == 0) {
  lines <- c(lines, "No warnings or errors.")
}

writeLines(lines, run_log)

# Also echo to the GitHub Actions log itself, for anyone checking the run
# directly rather than waiting on the email.
cat(paste(lines, collapse = "\n"), "\n")
