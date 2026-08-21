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
#
# By default this extracts BOTH province-level (Table 1) and district-level
# data. To run province-level only (faster, skips the district case/
# compliance tables entirely), set the INCLUDE_DISTRICT environment
# variable to "false"/"0"/"no" before invoking Rscript, e.g.:
#   INCLUDE_DISTRICT=false Rscript run_pipeline.R
# In GitHub Actions, set it via `env: INCLUDE_DISTRICT: "false"` on the step,
# or as a workflow_dispatch input passed through the same way -- no need to
# edit this file to change it per-run.
# ================================================================

source("1_1_DownloadData_PAK_v4.R")

cases_file      <- "Data/PAK_IDSR_Data.csv"
compliance_file <- "Data/PAK_IDSR_Compliance.csv"
failed_log      <- "Data/failed_reports.txt"
run_log         <- "Data/run_log.txt"

# Province-level (Table 1 + national compliance) is always extracted.
# District-level tables are extracted too unless INCLUDE_DISTRICT is
# explicitly set to a falsy value. Unset/anything else defaults to TRUE.
include_district_env <- Sys.getenv("INCLUDE_DISTRICT", unset = "true")
include_district <- !tolower(trimws(include_district_env)) %in% c("false", "0", "no", "n")

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
  extract_PAK_data_main(include_district = include_district),
  message = msg_handler,
  warning = warn_handler
)

failed_after <- if (file.exists(failed_log)) readLines(failed_log) else character(0)
new_lines <- if (length(failed_after) > failed_before) {
  failed_after[(failed_before + 1):length(failed_after)]
} else {
  character(0)
}

# process_one_bulletin() tags every line it writes to failed_log with its
# kind -- "[ERROR]" for genuine failures, "[WARNING]"/"[MESSAGE]" for every
# warning()/message() it raises (a persistent on-disk record independent of
# this driver). The WARNING/MESSAGE lines are already captured separately
# above via the withCallingHandlers around extract_PAK_data_main(), so only
# the ERROR-tagged lines belong in the "ERRORS" section below.
new_failures <- new_lines[grepl(": \\[ERROR\\]", new_lines)]

# (Two-branch string built separately, not inline in paste0() below --
# paste0() concatenates EVERY argument regardless of which arm of an
# embedded if/else fires, so passing the trailing include_district_env/")"
# pieces as separate paste0() arguments appended them unconditionally on
# BOTH branches, producing garbled text like "...+ district-leveltrue)" on
# a completely normal run.)
mode_line <- if (include_district) {
  "Mode: province-level + district-level"
} else {
  paste0("Mode: province-level only (district-level skipped -- INCLUDE_DISTRICT=", include_district_env, ")")
}

lines <- c(
  paste0("PAK IDSR data pipeline run -- ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  mode_line,
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
