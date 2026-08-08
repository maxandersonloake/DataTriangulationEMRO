# ================================================================
# WHO EMRO — Pakistan IDSR Surveillance Dashboard
# ----------------------------------------------------------------
# Built for deployment on Posit Connect Cloud (https://docs.posit.co/connect-cloud/)
#
# WHO Colours
#   - Navy Blue (#00205C) primary, WHO Blue (#009ADE) secondary/accent
#   - Source Sans Pro typography
#   - WHO Emergency Red (#EF3842), WHO Green (#80BC00) for the table scale
#
# Folder layout expected (all relative to this app.R, which should sit at
# the project root next to the Data/ and www/ folders):
#   app.R
#   www/who_brand.css
#   www/logo.png            <- WHO logo, add this yourself (not shipped here)
#   Data/PAK_IDSR_Data.csv
#   Data/PAK_IDSR_Compliance.csv
#   Data/pakistan_admin1.geojson  <- bundled province boundaries for the map
# ================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(base64enc)
library(sf)
library(ggrepel)

REGIONS_GEOJSON <- "Data/pakistan_admin1.geojson"

# ---- WHO brand colours -----------------------------------------
who_navy   <- "#00205C"
who_blue   <- "#009ADE"
who_blue10 <- "#E6EFF9"
who_red    <- "#EF3842"
who_red_dark <- "#B0121A"
who_green  <- "#80BC00"
who_orange <- "#F26829"
who_yellow <- "#F4A81D"
who_purple <- "#5B2C86"
who_magenta <- "#A6228C"

DATA_PATH       <- "Data/PAK_IDSR_Data.csv"
COMPLIANCE_PATH <- "Data/PAK_IDSR_Compliance.csv"

# ---- Alert detection method ------------------------------------
# Modified CDC EARS-style CUSUM/C2 aberration detection method (Hutwagner
# et al., "Comparing Aberration Detection Methods with Simulated Data",
# https://pmc.ncbi.nlm.nih.gov/articles/PMC3320440/). A rolling 9-week
# lookback window is used, but the 2 most recent weeks adjacent to the
# current week are dropped (a "guard band") so the baseline isn't
# contaminated by cases that may belong to the same emerging cluster as
# the current week. The mean (mu) and standard deviation (sigma) of the
# remaining 7 baseline weeks define escalating alert thresholds:
#   T1 = mu + 2*sigma  (yellow)      T2 = mu + 3*sigma (medium red)
#   T3 = mu + 4*sigma  (dark red)
# At least 10 total calendar weeks of history (9 lookback weeks + the
# current week) are required before a location/disease series is
# evaluated. The lookback is based on actual elapsed CALENDAR weeks (via
# `week_calendar` below), not on row position -- so a week with no
# submitted report doesn't silently shift which weeks count as "recent".
# If a report is missing for one of the 7 baseline weeks, the mean/SD are
# calculated from whatever baseline weeks ARE available, as long as at
# least CUSUM_MIN_BASELINE_WEEKS remain; otherwise the week is flagged as
# having insufficient baseline data rather than evaluated.
#
# Three distinct "not evaluated" outcomes are tracked, not just one:
#   - "no_report": the target week itself was explicitly marked NR (Not
#     Reported) in the source bulletin -- the disease WAS in that week's
#     table, this location just didn't report on it. Alert-worthy: this
#     is a real data gap for a disease that's normally tracked.
#   - "absent": the disease simply wasn't a row in that week's bulletin
#     at all (no explicit NR, no row of any kind). NOT alert-worthy --
#     there was nothing to report on in the first place, so this is
#     silently skipped rather than flagged.
#   - "insufficient_history" / "insufficient_baseline": the target week
#     has a real report, but there isn't a usable baseline to compare it
#     against.
CUSUM_LOOKBACK    <- 9
CUSUM_GUARD_BAND  <- 2
CUSUM_BASELINE_N  <- CUSUM_LOOKBACK - CUSUM_GUARD_BAND  # 7
CUSUM_MIN_POINTS  <- 10
# If some of the 7 baseline weeks are missing (no report submitted that
# week), the mean/SD are still calculated from whichever baseline weeks
# ARE available, as long as at least this many remain. Below this, there
# isn't enough signal to trust a baseline, and the week is flagged as
# "insufficient baseline data" rather than evaluated.
CUSUM_MIN_BASELINE_WEEKS <- 3
# A perfectly flat (SD = 0) baseline is common for rare/sporadic diseases
# whose baseline weeks are mostly or entirely the same value (often all
# zero) -- rather than treating this as "no usable baseline", the EARS/
# CUSUM aberration-detection literature floors the SD at a small minimum
# so thresholds stay finite and well-calibrated. See Hutwagner et al.,
# "Enhancing Time-Series Detection Algorithms for Automated
# Biosurveillance", Emerg Infect Dis 2009 (flags SD=0 as a known EARS
# limitation, recommends a minimum SD to avoid division by zero, but
# cautions that too low a floor -- e.g. 0.2 -- makes a single case a 5-SD
# event); and the EARS-W2 comparison study (Muscatello et al.), which
# found a minimum SD of 1.0 improved sensitivity without that
# hypersensitivity problem. A floor of 1 means, for an otherwise-flat
# baseline, T1/T2/T3 become simply mu+2/mu+3/mu+4 cases.
CUSUM_MIN_BASELINE_SD <- 1
# Code-level setting (not exposed in the UI): should a BASELINE week where
# the disease simply wasn't listed in that week's bulletin at all (status
# "absent" -- distinct from an explicit NR, which is ALWAYS excluded from
# the baseline regardless of this flag) be imputed as 0 cases for the
# purposes of the baseline mean/SD, rather than excluded entirely?
# Default FALSE: a week the disease wasn't tracked in at all is treated as
# unknown and excluded, not assumed to be zero. Flip to TRUE to instead
# assume 0 for those weeks (a more aggressive baseline that will lower
# both mu and sigma when a disease has gaps in its reporting history).
CUSUM_IMPUTE_ABSENT_WEEKS_AS_ZERO <- FALSE

# Filename only, relative to the www/ folder next to app.R.
LOGO_FILENAME <- "logo.png"
LOGO_FULL_PATH <- file.path("www", LOGO_FILENAME)

if (file.exists(LOGO_FULL_PATH)) {
  logo_uri <- base64enc::dataURI(file = LOGO_FULL_PATH, mime = "image/png")
} else {
  warning(
    "WHO logo NOT FOUND. R is looking for it at:\n  ",
    normalizePath(LOGO_FULL_PATH, mustWork = FALSE),
    "\nPlace the official WHO logo PNG at www/logo.png (see WHO brand guidance)."
  )
  logo_uri <- NULL
}

# ---- Load disease data ----------------------------------------------------
# A row's Cases can legitimately be NA for exactly one reason: an explicit
# "NR" cell in the source bulletin (Status == "NR", added by
# 1_1_DownloadData_PAK_v3.R). That's kept as a real row rather than
# dropped, so it can be distinguished downstream from a disease that
# simply never appeared as a row in a given week's table at all (which has
# no CSV row to begin with, and so is naturally absent here too -- see the
# status handling in compute_cusum_stats_at()).
raw_data <- read_csv(DATA_PATH, show_col_types = FALSE)

# Backward compatibility: CSVs written before this NR/absent distinction
# was added have no Status column at all, and never wrote a row for an NR
# cell in the first place -- so every row from an older file is safely
# "reported".
if (!"Status" %in% names(raw_data)) {
  raw_data$Status <- ifelse(is.na(raw_data$Cases), NA_character_, "reported")
}

raw_data <- raw_data %>%
  mutate(
    Disease  = as.character(Disease),
    Province = as.character(Province),
    Cases    = suppressWarnings(as.numeric(Cases)),
    Week     = as.integer(Week),
    Year     = as.integer(Year),
    Status   = as.character(Status)
  ) %>%
  filter(!is.na(Week), !is.na(Year)) %>%
  # Keep a row if it has a real case count (including a genuine 0) OR is
  # an explicit NR. Drop anything else (e.g. a malformed/unparseable
  # cell, which would have Cases NA and Status != "NR").
  filter(!is.na(Cases) | Status == "NR")

# ---- Load compliance data --------------------------------------------------
# Note: a row can have a recorded "Compliance (%)" even when Expected
# Reports is 0 or missing (a division-by-zero artefact upstream, e.g. a
# defaulted/placeholder value). We treat that as NO usable compliance
# signal -- Compliance is set to NA in that case -- so that a genuinely
# missing/undefined compliance figure is never mistaken for 100% and
# doesn't silently produce a projection anywhere downstream.
compliance_data <- read_csv(COMPLIANCE_PATH, show_col_types = FALSE) %>%
  mutate(
    Region = as.character(Region),
    `Compliance (%)`   = suppressWarnings(as.numeric(`Compliance (%)`)),
    `Expected Reports` = suppressWarnings(as.numeric(`Expected Reports`)),
    `Received Reports` = suppressWarnings(as.numeric(`Received Reports`)),
    Week = as.integer(Week),
    Year = as.integer(Year),
    Compliance = ifelse(
      is.na(`Expected Reports`) | `Expected Reports` <= 0 | is.na(`Compliance (%)`),
      NA_real_, `Compliance (%)`
    )
  ) %>%
  select(Region, Week, Year, Compliance)

disease_choices  <- sort(unique(raw_data$Disease))
location_choices <- c("National", sort(unique(raw_data$Province[raw_data$Province != "Total"])))

latest_year <- max(raw_data$Year, na.rm = TRUE)
latest_week <- max(raw_data$Week[raw_data$Year == latest_year], na.rm = TRUE)

# ---- Calendar of every (Year, Week) combination seen anywhere in the data -
# Used two ways: (1) to compute calendar-week offsets for the CUSUM
# lookback, so "9 weeks before" means 9 actual elapsed calendar weeks
# rather than 9 rows of a particular disease/location series (which could
# be a much longer span if that series has gaps); and (2) to populate the
# "As of week" dropdowns on the Alerts and Data visualisation tabs.
week_calendar <- raw_data %>%
  distinct(Year, Week) %>%
  arrange(Year, Week) %>%
  mutate(week_idx = row_number())

week_choice_values <- paste0(week_calendar$Year, "-", sprintf("%02d", week_calendar$Week))
week_choice_labels <- paste0("Week ", week_calendar$Week, ", ", week_calendar$Year)
WEEK_CHOICES <- setNames(rev(week_choice_values), rev(week_choice_labels))  # most recent first
LATEST_WEEK_CHOICE <- paste0(latest_year, "-", sprintf("%02d", latest_week))

# Parses an "As of week" dropdown value (e.g. "2026-31") back into a
# year/week pair.
parse_asof <- function(x) {
  parts <- strsplit(x, "-")[[1]]
  list(year = as.integer(parts[1]), week = as.integer(parts[2]))
}

# Small null-coalesce helper (base R has no built-in %||%)
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Helper: weekly reported + projected series for a disease/location ----
# Projected total cases = reported cases / (compliance / 100). NA if
# compliance is 0 or missing (can't estimate a projection with no reports).
get_trend_data <- function(disease, location) {
  d <- raw_data %>% filter(Disease == disease)
  d <- if (location == "National") d %>% filter(Province == "Total") else d %>% filter(Province == location)

  d <- d %>%
    group_by(Year, Week) %>%
    # sum(Cases, na.rm = TRUE) alone would silently turn a week that's
    # ONLY an explicit-NR row (Cases = NA) into 0, since summing zero
    # non-NA values returns 0 rather than NA. Keep it NA in that case so
    # the row still exists (this week WAS in the table) but correctly
    # reads as "no report" rather than "zero cases" downstream.
    summarise(Cases = if (all(is.na(Cases))) NA_real_ else sum(Cases, na.rm = TRUE), .groups = "drop") %>%
    mutate(join_key = if (location == "National") "National" else location)

  d %>%
    left_join(compliance_data, by = c("join_key" = "Region", "Week" = "Week", "Year" = "Year")) %>%
    mutate(
      Reported  = Cases,
      Projected = ifelse(is.na(Compliance) | Compliance <= 0, NA_real_, Cases / (Compliance / 100))
    ) %>%
    select(Year, Week, Reported, Projected, Compliance) %>%
    arrange(Year, Week)
}

# ---- Helper: national/district weekly series for ALL diseases -------------
get_all_disease_series <- function(location) {
  d <- raw_data
  d <- if (location == "National") d %>% filter(Province == "Total") else d %>% filter(Province == location)

  d %>%
    group_by(Disease, Year, Week) %>%
    # See get_trend_data() above for why this isn't a plain sum(na.rm=TRUE).
    summarise(Cases = if (all(is.na(Cases))) NA_real_ else sum(Cases, na.rm = TRUE), .groups = "drop") %>%
    mutate(join_key = if (location == "National") "National" else location) %>%
    left_join(compliance_data, by = c("join_key" = "Region", "Week" = "Week", "Year" = "Year")) %>%
    mutate(Projected = ifelse(is.na(Compliance) | Compliance <= 0, NA_real_, Cases / (Compliance / 100))) %>%
    select(Disease, Year, Week, Reported = Cases, Projected, Compliance)
}



# ---- Helper: CUSUM/C2-style aberration stats at a specific (year, week) --
# `series` must be a single disease/location series (as produced by
# get_all_disease_series() %>% filter(Disease == ...), or get_trend_data()).
# Always returns a list with a `status` field, one of:
#   "ok"                    -- current week has a report and a usable
#                               baseline; $current/$mu/$sigma/$z/$level set.
#   "no_report"              -- the target week WAS in that week's bulletin
#                               (a row exists) but was explicitly marked
#                               NR. Alert-worthy: a real data gap for a
#                               disease that's normally tracked here.
#   "absent"                 -- the target week has no row at all (the
#                               disease wasn't in that week's bulletin, no
#                               explicit NR either). NOT alert-worthy --
#                               there was nothing to report on.
#   "insufficient_history"   -- fewer than CUSUM_LOOKBACK calendar weeks
#                               exist before the target week at all.
#   "insufficient_baseline"  -- enough calendar history exists, but fewer
#                               than CUSUM_MIN_BASELINE_WEEKS of the 7
#                               baseline weeks actually have a report.
# The lookback is based on actual elapsed CALENDAR weeks (via the global
# `week_calendar`), not on row position within `series` -- so a week with
# no report doesn't silently pull the baseline further back in time than
# intended. Missing baseline weeks are simply skipped (rather than voiding
# the whole calculation), as long as at least CUSUM_MIN_BASELINE_WEEKS
# remain -- except that an explicit NR baseline week is ALWAYS excluded,
# while an "absent" (disease not in that week's table) baseline week is
# excluded or imputed as 0 depending on CUSUM_IMPUTE_ABSENT_WEEKS_AS_ZERO
# (see that constant's comment above for why these are handled
# differently).
#
# A genuinely reported ZERO is NOT the same as "no report", and is fully
# usable both as a current value and as a baseline contributor -- this
# distinction is preserved end-to-end from the extraction pipeline through
# to here:
#   - the extraction pipeline (1_1_DownloadData_PAK_v3.R, convert_cases_table)
#     converts the literal token "NR" to NA but KEEPS that row (Status =
#     "NR"), so the CSV has a row whenever the disease was in that week's
#     table -- whether the reported value was a real count (including 0)
#     or an explicit NR. A disease that wasn't a row in the table AT ALL
#     that week has no CSV row, exactly as before.
#   - get_all_disease_series()/get_trend_data() group_by + summarise() can
#     only produce an output row for a (Year, Week) combination that has
#     at least one input row, so a disease truly absent from a week stays
#     absent through aggregation; and the aggregation is written so an
#     all-NR group stays NA rather than being summed down to 0.
#   - the checks just below are presence/NA-based, never value-based, so
#     they never mistake a real 0 for a missing or NR report.
compute_cusum_stats_at <- function(series, year, week, value_col = "Reported") {
  cur_row <- series[series$Year == year & series$Week == week, ]
  if (nrow(cur_row) == 0) {
    return(list(status = "absent", year = year, week = week))
  }
  if (is.na(cur_row[[value_col]][1])) {
    return(list(status = "no_report", year = year, week = week))
  }
  current <- cur_row[[value_col]][1]

  target_idx_v <- week_calendar$week_idx[week_calendar$Year == year & week_calendar$Week == week]
  if (length(target_idx_v) == 0) {
    return(list(status = "absent", year = year, week = week))
  }
  target_idx <- target_idx_v[1]

  if (target_idx <= CUSUM_LOOKBACK) {
    return(list(status = "insufficient_history", year = year, week = week))
  }

  # The 7 baseline weeks: the 9 calendar weeks before the target, minus the
  # 2 most recent (the guard band).
  baseline_idxs <- (target_idx - CUSUM_LOOKBACK):(target_idx - CUSUM_GUARD_BAND - 1)
  baseline_cal <- week_calendar[week_calendar$week_idx %in% baseline_idxs, c("Year", "Week")]

  baseline_vals <- vapply(seq_len(nrow(baseline_cal)), function(i) {
    r <- series[series$Year == baseline_cal$Year[i] & series$Week == baseline_cal$Week[i], ]
    if (nrow(r) == 0) {
      # Disease wasn't listed in that week's bulletin at all -- distinct
      # from an explicit NR (the else branch below, where
      # r[[value_col]][1] is NA and is always excluded regardless of this
      # flag). Governed by CUSUM_IMPUTE_ABSENT_WEEKS_AS_ZERO.
      if (CUSUM_IMPUTE_ABSENT_WEEKS_AS_ZERO) 0 else NA_real_
    } else {
      r[[value_col]][1]
    }
  }, numeric(1))
  baseline_vals <- baseline_vals[!is.na(baseline_vals)]

  if (length(baseline_vals) < CUSUM_MIN_BASELINE_WEEKS) {
    return(list(status = "insufficient_baseline", year = year, week = week))
  }

  mu        <- mean(baseline_vals)
  sigma_raw <- sd(baseline_vals)
  if (is.na(sigma_raw)) {
    return(list(status = "insufficient_baseline", year = year, week = week))
  }
  # Floor a flat (or near-flat) baseline's SD rather than treating it as
  # unusable -- see CUSUM_MIN_BASELINE_SD's comment above for the
  # literature basis.
  sigma <- max(sigma_raw, CUSUM_MIN_BASELINE_SD)

  T1 <- mu + 2 * sigma
  T2 <- mu + 3 * sigma
  T3 <- mu + 4 * sigma
  level <- if (current > T3) 3L else if (current > T2) 2L else if (current > T1) 1L else 0L

  list(status = "ok", current = current, mu = mu, sigma = sigma, sigma_raw = sigma_raw,
       z = (current - mu) / sigma, T1 = T1, T2 = T2, T3 = T3, level = level, year = year, week = week,
       n_baseline = length(baseline_vals))
}

# Convenience wrapper: just the z-score (in SD units) at a specific week, or
# NA if the week has no report or there isn't a usable baseline. Used to
# colour the map and the weekly summary table.
cusum_z_at <- function(series, year, week, value_col = "Reported") {
  stats <- compute_cusum_stats_at(series, year, week, value_col = value_col)
  if (stats$status != "ok") NA_real_ else stats$z
}

# ---- Helper: alerts -- every (location, disease) combo above threshold ----
# Evaluated at a specific (sel_year, sel_week) -- the "As of week" the user
# has selected -- rather than always the latest available data. Returns a
# list with three parts:
#   $alerts  -- one row per location/disease pair that exceeds T1 (mu +
#               2*sigma) at that week. The Alerts tab groups these by
#               disease and lists locations inline (e.g. "Pertussis:
#               National (6.8SD), ICT (2.4SD)").
#   $missing -- one row per location/disease pair that COULD be evaluated
#               (i.e. has some history at that location) but wasn't,
#               either because that week was explicitly marked NR
#               ("no_report") or there isn't a usable baseline
#               ("insufficient_history" / "insufficient_baseline"). A
#               disease that simply wasn't in that week's bulletin at all
#               ("absent") is skipped entirely -- it's not included here.
#               The Alerts tab renders $missing as grey flags grouped by
#               location.
#   $reported_counts -- a named list, location -> count of diseases that
#               HAD a real report that week (status != "no_report" and !=
#               "absent"), used to detect a location that's NR across
#               every disease it's normally evaluated for, so that can be
#               collapsed to a single "No reporting (all diseases)" flag
#               rather than a long per-disease list.
compute_alerts_long <- function(value_col = "Reported", sel_year, sel_week) {
  alert_rows      <- list()
  missing_rows    <- list()
  reported_counts <- list()

  for (loc in location_choices) {
    series <- get_all_disease_series(loc)
    if (nrow(series) == 0) next
    diseases_here <- sort(unique(series$Disease))
    reported_count <- 0L

    for (dis in diseases_here) {
      s <- series %>% filter(Disease == dis) %>% arrange(Year, Week)
      stats <- compute_cusum_stats_at(s, sel_year, sel_week, value_col = value_col)

      if (stats$status == "absent") next  # not in this week's bulletin at all -- not evaluated, not flagged

      if (stats$status != "no_report") reported_count <- reported_count + 1L

      if (stats$status == "ok") {
        if (stats$level > 0L) {
          alert_rows[[length(alert_rows) + 1]] <- data.frame(
            Location = loc, Disease = dis, Year = stats$year, Week = stats$week,
            Current = stats$current, Mu = stats$mu, Sigma = stats$sigma,
            Z = stats$z, Level = stats$level, stringsAsFactors = FALSE
          )
        }
      } else {
        missing_rows[[length(missing_rows) + 1]] <- data.frame(
          Location = loc, Disease = dis, Status = stats$status, stringsAsFactors = FALSE
        )
      }
    }

    reported_counts[[loc]] <- reported_count
  }

  alerts <- bind_rows(alert_rows)
  if (is.null(alerts) || nrow(alerts) == 0) {
    alerts <- data.frame(
      Location = character(), Disease = character(), Year = integer(), Week = integer(),
      Current = numeric(), Mu = numeric(), Sigma = numeric(), Z = numeric(), Level = integer(),
      stringsAsFactors = FALSE
    )
  } else {
    # Order locations within a disease the same way they appear in
    # location_choices (National first, then districts alphabetically)
    alerts$Location <- factor(alerts$Location, levels = location_choices)
    alerts <- alerts %>% arrange(Disease, desc(Level), desc(Z))
  }

  missing <- bind_rows(missing_rows)
  if (is.null(missing) || nrow(missing) == 0) {
    missing <- data.frame(Location = character(), Disease = character(), Status = character(), stringsAsFactors = FALSE)
  } else {
    # If a disease has insufficient baseline data at the National level,
    # it almost always does at the regional level too (the same sparse
    # reporting history), so listing it separately for every region adds
    # noise without adding information. Drop those regional duplicates,
    # keeping only the National line -- an explicit NR for that disease at
    # a specific region is a distinct, region-specific signal and is
    # always kept regardless.
    insuff_national <- missing$Disease[missing$Location == "National" &
                                          missing$Status %in% c("insufficient_history", "insufficient_baseline")]
    drop <- missing$Location != "National" &
      missing$Disease %in% insuff_national &
      missing$Status %in% c("insufficient_history", "insufficient_baseline")
    missing <- missing[!drop, ]

    missing$Location <- factor(missing$Location, levels = location_choices)
    missing <- missing %>% arrange(Location, Disease)
  }

  list(alerts = alerts, missing = missing, reported_counts = reported_counts)
}

# ---- Shared diverging colour scale for SD-from-baseline tables -----------
# Same CUSUM/C2 method as the Alerts tab (rolling 9-week window, 2-week
# guard band, 7-week baseline). Deliberately wide neutral band: a week
# within +-2 SD of its expected baseline stays plain white. Colour escalates
# through graduated tiers on both sides -- green shades the further below
# baseline, red shades the further above -- independent of the Alerts tab's
# own (separately styled) yellow/medium-red/dark-red boxes.
SD_BREAKS   <- c(-4, -3, -2, 2, 3, 4)
SD_BG_PAL   <- c("#1B7837", "#5AAE61", "#C7E9C0", "#FFFFFF", "#FCE9B0", who_red, who_red_dark)
SD_FONT_PAL <- c("#FFFFFF", "#111111", "#111111", "#111111", "#111111", "#FFFFFF", "#FFFFFF")
SD_LEGEND_LABELS <- c(
  "More than 4 SD below baseline",
  "3 to 4 SD below baseline",
  "2 to 3 SD below baseline",
  "Within \u00b12 SD (typical)",
  "2 to 3 SD above baseline",
  "3 to 4 SD above baseline",
  "More than 4 SD above baseline"
)

# ---- Fixed year -> colour map ----------------------------------------------
# Assigned once from every year present in the data (most recent = WHO Navy,
# then green, pale blue, orange, ...), so a year's colour never shifts
# depending on which other years happen to be checked in "Years to show".
who_pale_blue <- "#6FB3E0"
YEAR_COLOR_SEQUENCE <- c(who_navy, who_green, who_pale_blue, who_orange, who_magenta, who_yellow, who_purple)

ALL_YEARS_DESC <- sort(unique(raw_data$Year), decreasing = TRUE)
YEAR_COLOR_MAP <- setNames(
  rep(YEAR_COLOR_SEQUENCE, length.out = length(ALL_YEARS_DESC)),
  as.character(ALL_YEARS_DESC)
)

# ---- Fixed region -> colour map (for the regional-contribution bar chart) -
REGION_COLOR_SEQUENCE <- c(who_navy, who_blue, who_green, who_orange, who_yellow, who_purple, who_magenta)
REGION_NAMES <- location_choices[location_choices != "National"]
REGION_COLOR_MAP <- setNames(
  rep(REGION_COLOR_SEQUENCE, length.out = length(REGION_NAMES)),
  REGION_NAMES
)

# ---- Helper: Pakistan admin-1 boundaries (bundled, static) -----------------
# Shipped as Data/pakistan_admin1.geojson: 7 provinces/territories dissolved
# from district-level polygons (source: click_that_hood, CC-licensed OSM-
# derived data), with a `province` field already matching our region codes
# (Balochistan, GB, ICT, KP, Sindh, AJK, Punjab). No internet access is
# needed at runtime -- this avoids the earlier issue where the map failed
# to load in environments without outbound connectivity.
load_pak_regions <- function() {
  tryCatch({
    if (!file.exists(REGIONS_GEOJSON)) stop("boundary file not found at ", REGIONS_GEOJSON)
    sf_obj <- sf::st_read(REGIONS_GEOJSON, quiet = TRUE)
    sf::st_make_valid(sf_obj)
  }, error = function(e) {
    message("Could not load Pakistan boundaries: ", conditionMessage(e))
    NULL
  })
}

pak_regions_sf <- load_pak_regions()

# =================================================================
# UI
# =================================================================
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "who_brand.css?v=11"),
    tags$title("WHO EMRO | Pakistan IDSR Dashboard")
  ),

  # ---- WHO branded header ----
  # Inline styles are applied alongside the who-header/.who-title classes
  # (in www/who_brand.css) as a safety net -- if a browser or CDN ever
  # serves a stale cached copy of the CSS file, the header still renders
  # correctly because these rules don't depend on the external file at all.
  div(
    class = "who-header",
    style = "display:flex; flex-direction:row-reverse; align-items:center; justify-content:space-between; padding:14px 24px; background-color:#FFFFFF; border-bottom:3px solid #00205C;",
    if (!is.null(logo_uri)) {
      tags$img(src = logo_uri, alt = "World Health Organization",
               style = "height:92px; border:none; outline:none; box-shadow:none;")
    } else {
      div(
        style = "color:#EF3842; border:2px dashed #EF3842; padding:6px 10px; font-size:12px;",
        paste0("LOGO NOT FOUND at www/", LOGO_FILENAME, " -- add the official WHO logo PNG there.")
      )
    },
    div(
      class = "who-header-text",
      style = "text-align:left;",
      div(class = "who-title", style = "font-size:34px; font-weight:700; color:#00205C; line-height:1.15;",
          "Pakistan IDSR Surveillance Dashboard"),
      div(class = "who-subtitle", style = "font-size:15px; color:#555555; margin-top:2px;",
          "Alpha Version - Internal and Preliminary")
    )
  ),

  navbarPage(
    title = NULL,
    id = "who_nav",
    collapsible = TRUE,

    # ---------------------------------------------------------
    tabPanel(
      "Home",
      div(
        class = "page-tint-bg",
        div(
          style = "max-width: 980px; margin: 0 auto;",
          h4("About this dashboard"),
          p("This dashboard supports the World Health Organisation (WHO) Regional Office for the Eastern Medierranean (EMRO)
            in monitoring disease case reporting from Pakistan, provided by the 
            Integrated Disease Surveillance and Response (IDSR) bulletins."),
          p("Check the ", strong("Alerts"), " tab for a scan of every region and disease for unusually large ",
            "increases, use ", strong("Data visualisation"), " to explore weekly trends by disease and region, and ",
            strong("Weekly summary table"), " for the full week-by-week breakdown."),
          h4("Data sources and limitations", style = "margin-top: 20px;"),
          tags$ul(
            tags$li(strong("Suspected vs confirmed cases: "), "Case counts reported through IDSR are mostly ",
                    "suspected cases, not laboratory-confirmed diagnoses. Treat them as an early-warning signal, ",
                    "not a confirmed count of cases."),
            tags$li(strong("Reporting compliance: "), "Not every expected reporting site submits data every week. ",
                    "\u201cCompliance %\u201d is the share of expected reports a region actually received for a given ",
                    "week -- weeks or regions with low compliance will under-represent the true number of cases."),
            tags$li(strong("Geographic coverage: "), "Regions match those published in the NIH IDSR bulletins: ",
                    "Balochistan, Gilgit-Baltistan, Islamabad Capital Territory (ICT), Khyber Pakhtunkhwa (KP), ",
                    "Punjab, Sindh, and Azad Jammu & Kashmir (AJK)."),
            tags$li(strong("Missing data: "), "Some regions don't report for a given week -- these are marked NR ",
                    "(Not Reported) in the bulletin. Some diseases are also missing from the table entirely for a ",
                    "given week, most likely because there were no cases to report. In this case, we also treat the value ",
                    "as missing rather than assuming it's zero. Missing weeks appear as grey flags on the map and ",
                    "in the Alerts tab.")
          ),
          h4("Methodology", style = "margin-top: 20px;"),
          h5("Projected total cases", style = "margin-bottom: 4px; color: var(--who-navy); font-size: 14px;"),
          p("Using the number of reported cases and the compliance percentage, we can estimate the true number of ",
            "cases for a given week and region. We use a simple projection: reported cases \u00f7 (compliance % / ",
            "100). For example, 20 reported cases with 50% compliance gives a projected total of ",
            "20 \u00f7 0.5 = 40 cases. This assumes non-reporting sites have a similar case rate to reporting ",
            "sites, which may not hold -- particularly during active outbreaks or access constraints -- so treat ",
            "projected figures as a rough estimate, not a precise count."),
          h5("CUSUM alert detection method", style = "margin-bottom: 4px; margin-top: 14px; color: var(--who-navy); font-size: 14px;"),
          p("Alerts are generated using a CUSUM/C2 aberration detection method. For a given disease and location, ",
            "we look at the 9 weeks immediately before the week being evaluated. We drop the 2 most recent of ",
            "those weeks (a \u201cguard band\u201d), so an emerging outbreak can't inflate its own baseline. The ",
            "mean (\u03bc) and standard deviation (\u03c3) of the remaining 7 baseline weeks set three thresholds: ",
            "T1 = \u03bc + 2\u03c3 (yellow), T2 = \u03bc + 3\u03c3 (medium red), and T3 = \u03bc + 4\u03c3 (dark red)."),
          tags$ul(
            tags$li(strong("Handling missing weeks in the baseline: "), "If one or more of the 7 baseline weeks is ",
                    "marked NR, or missing from the bulletin entirely, we calculate the mean and standard ",
                    "deviation from the remaining weeks -- as long as at least 3 have a report. Below that, we ",
                    "don't apply the method, and the week is flagged as having ", strong("insufficient baseline data")),
            tags$li(strong("Flat baselines: "), "A rare or sporadic disease can have a baseline where all 7 weeks ",
                    "report the same value, often zero -- giving a standard deviation of exactly 0. In this case, ",
                    "we apply a minimum standard deviation of 1, following standard practice in the ",
                    "aberration-detection literature for this method (see the ", strong("References"), " tab).")
          ),
          div(
            class = "info-card",
            style = "border-left: 6px solid #B0121A; background-color: #FDF2F2; margin-top: 24px;",
            h4("Disclaimer", style = "color:#B0121A;"),
            p(strong("Disclaimer:"), " This dashboard is an internal WHO analytical tool developed to support the ",
              "exploratory review and interpretation of publicly available surveillance data. Its outputs, including ",
              "statistical signals and projected estimates, are preliminary and require validation. They do not ",
              "constitute official WHO epidemiological assessments, alerts, or recommendations and do not replace ",
              "established Public Health Intelligence (PHI) processes, expert epidemiological review, verification ",
              "by national authorities, or formal WHO information products. The dashboard should not be shared ",
              "externally or used for operational or public communication without appropriate technical review and ",
              "authorization.")
          )
        )
      )
    ),

    # ---------------------------------------------------------
    tabPanel(
      "Alerts",
      div(
        style = "padding-top: 16px; padding-bottom: 28px;",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            selectInput(
              "asof_week_alerts", "As of week",
              choices = WEEK_CHOICES, selected = LATEST_WEEK_CHOICE
            ),
            radioButtons(
              "case_type_alerts", "Case counts to evaluate",
              choices = c("Reported cases" = "reported", "Projected total cases" = "projected"),
              selected = "reported"
            ),
            tags$hr(),
            tags$p(
              style = "font-size: 12px; color: #555;",
              strong("Projected total cases"), "estimates total cases by dividing the number of reported ",
              "cases by the compliance percentage. For example, 20 reported cases and a compliance of 50% gives ",
              "a projected total case estimate of 40. See the ", strong("Home"), " tab for full details."
            )
          ),
          mainPanel(
            width = 9,
            uiOutput("alerts_output")
          )
        )
      )
    ),

    # ---------------------------------------------------------
    tabPanel(
      "Data visualisation",
      div(
        style = "padding: 14px 24px;",
        div(
          style = "background-color:#F4F5F6; border:1px solid #DDE1E4; border-radius:6px; padding:8px 18px; margin-bottom:10px; display:flex; gap:24px; align-items:flex-end;",
          div(style = "flex: 1 1 0;", selectInput("disease", "Disease", choices = disease_choices, selected = disease_choices[1], width = "100%")),
          div(style = "flex: 1 1 0;", selectInput("location", "Location", choices = location_choices, selected = "National", width = "100%")),
          div(style = "flex: 1 1 0;", selectInput("asof_week_viz", "As of week", choices = WEEK_CHOICES, selected = LATEST_WEEK_CHOICE, width = "100%"))
        ),
        div(
          class = "info-disclosure",
          div(class = "info-disclosure-title", "\u2139"),
          div(
            style = "font-size: 12.5px; color: #555;",
            p(style = "margin-bottom: 6px;",
              strong("Projected total cases"), "estimates total cases by dividing the number of reported ",
              "cases by the compliance percentage. For example, 20 reported cases and a compliance of 50% gives ",
              "a projected total case estimate of 40."),
            
            p(style = "margin-bottom: 6px;",
              strong("SD"), " shows how many standard deviations a week's case count is from its expected baseline, ",
              "using a CUSUM aberration detection method (rolling 9-week baseline, most recent 2 weeks dropped)."),
            
            p(style = "margin-bottom: 0;",
              "See the ", strong("Home"), " tab for full details.")
          )
        ),
        fluidRow(
          column(
            width = 6,
            div(
              class = "viz-panel",
              style = "background-color:#FFFFFF; border:1px solid #E0E4E8; border-radius:8px; padding:16px 18px 12px 18px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
              h4("Weekly case trend", style = "margin-top:0; margin-bottom:2px; font-size:17px;"),
              uiOutput("trend_subtitle"),
              plotlyOutput("trend_plot", height = "420px"),
              div(
                class = "viz-panel-controls",
                style = "margin-top:10px; padding-top:10px; border-top:1px solid #EEF1F4; min-height:150px;",
                uiOutput("year_selector"),
                radioButtons(
                  "case_type", "Case counts to show",
                  choices = c("Reported cases only" = "reported",
                              "Projected total cases" = "projected",
                              "Both" = "both"),
                  selected = "reported", inline = TRUE
                )
              )
            )
          ),
          column(
            width = 6,
            div(
              class = "viz-panel",
              style = "background-color:#FFFFFF; border:1px solid #E0E4E8; border-radius:8px; padding:16px 18px 12px 18px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
              h4("Deviation from expected baseline (SD), by region", style = "margin-top:0; margin-bottom:2px; font-size:17px;"),
              uiOutput("map_subtitle"),
              plotOutput("region_map", height = "420px"),
              div(
                class = "viz-panel-controls",
                style = "margin-top:10px; padding-top:10px; border-top:1px solid #EEF1F4; min-height:150px;",
                radioButtons(
                  "case_type_map", "Case counts to colour the map by",
                  choices = c("Reported cases" = "reported", "Projected total cases" = "projected"),
                  selected = "reported", inline = TRUE
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "viz-panel",
              style = "background-color:#FFFFFF; border:1px solid #E0E4E8; border-radius:8px; padding:16px 18px 12px 18px; box-shadow:0 1px 3px rgba(0,0,0,0.06); margin-top:16px;",
              h4("Regional contribution to total cases", style = "margin-top:0; margin-bottom:2px; font-size:17px;"),
              uiOutput("stack_subtitle"),
              plotlyOutput("region_stack_plot", height = "380px"),
              div(
                class = "viz-panel-controls",
                style = "margin-top:10px; padding-top:10px; border-top:1px solid #EEF1F4;",
                radioButtons(
                  "case_type_stack", "Case counts to show",
                  choices = c("Reported cases" = "reported", "Projected total cases" = "projected"),
                  selected = "reported", inline = TRUE
                )
              )
            )
          )
        )
      )
    ),

    # ---------------------------------------------------------
    tabPanel(
      "Weekly summary table",
      div(
        style = "padding-top: 16px;",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("location_tbl", "Location", choices = location_choices, selected = "National"),
          numericInput("n_weeks", "Number of recent weeks to show", value = 8, min = 4, max = 20, step = 1),
          radioButtons(
            "case_type_tbl", "Case counts to display",
            choices = c("Reported cases" = "reported", "Projected total cases" = "projected"),
            selected = "reported"
          ),
          tags$hr(),
          div(
            style = "font-weight:600; color:var(--who-navy); font-size:13px; margin-bottom:8px;",
            "Cell shading"
          ),
          div(
            lapply(rev(seq_along(SD_BG_PAL)), function(i) {
              div(class = "sd-legend-row",
                  span(class = "sd-legend-swatch", style = paste0("background-color:", SD_BG_PAL[i], ";")),
                  SD_LEGEND_LABELS[i])
            })
          ),
          tags$p(
            style = "font-size: 12px; color: #555; margin-top: 8px;",
            "Standard deviations are based on the CUSUM aberration detection method (rolling 9-week baseline, most recent 2 weeks dropped). See the ", strong("Home"), " tab for details."
          )
        ),
        mainPanel(
          width = 9,
          p(
            style = "font-size: 13px; color:#555;",
            icon("triangle-exclamation"), " Looking for a scan across ", strong("all"), " regions and diseases at once? ",
            "See the ", strong("Alerts"), " tab."
          ),
          DTOutput("weekly_table")
        )
      )
      )
    ),

    # ---------------------------------------------------------
    tabPanel(
      "References",
      div(
        class = "page-tint-bg",
        div(
          style = "max-width: 900px; margin: 0 auto;",
          h4("Primary data source"),
          p("Weekly IDSR bulletins published by Pakistan's National Institute of Health (NIH):"),
          tags$a(href = "https://www.nih.org.pk/phb/weekly-bulletin", target = "_blank",
                 "https://www.nih.org.pk/phb/weekly-bulletin"),
          h4("Alert detection methodology", style = "margin-top: 20px;"),
          p("The Alerts tab and the SD-based shading on the map and weekly summary table use a modified CUSUM/C2 ",
            "aberration detection method (rolling 9-week baseline, most recent 2 weeks dropped as a guard band). ",
            "See the ", strong("Home"), " tab for a full explanation of how it's calculated."),
          tags$a(href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC3320440/", target = "_blank",
                 "Hutwagner et al., Comparing Aberration Detection Methods with Simulated Data"),
          p(style = "margin-top: 10px;",
            "For rare/sporadic diseases with an all-zero (or otherwise flat) baseline, a minimum standard ",
            "deviation of 1 is applied rather than treating the week as unusable -- see:"),
          tags$a(href = "https://wwwnc.cdc.gov/eid/article/15/4/08-0616_article", target = "_blank",
                 "Hutwagner et al., Enhancing Time-Series Detection Algorithms for Automated Biosurveillance, Emerg Infect Dis, 2009"),
          h4("Contact", style = "margin-top: 20px;"),
          p("For questions about this dashboard or its data pipeline, contact mandersonloake@gmail.com.")
        )
      )
    )
  ),

  div(
    class = "who-footer",
    style = "padding:16px 28px; font-size:12px; color:#555555; background-color:#FFFFFF; border-top:1px solid #C9DEF3;",
    "Data source: National Institute of Health (NIH) Pakistan, IDSR weekly bulletins."
  )
)

# =================================================================
# SERVER
# =================================================================
server <- function(input, output, session) {

  # ---------------- Trends tab ----------------

  # Full (unfiltered-by-year) trend data for the current disease/location
  trend_data_all <- reactive({
    req(input$disease, input$location)
    get_trend_data(input$disease, input$location)
  })

  output$trend_subtitle <- renderUI({
    req(input$disease, input$location)
    div(class = "viz-subtitle",
        paste0("Disease: ", input$disease, ", Location: ", input$location))
  })

  output$year_selector <- renderUI({
    d <- trend_data_all()
    yrs <- sort(unique(d$Year), decreasing = TRUE)
    default_selected <- head(yrs, 2)
    checkboxGroupInput("years_selected", "Years to show", choices = yrs, selected = default_selected, inline = TRUE)
  })

  output$trend_plot <- renderPlotly({
    req(input$asof_week_viz)
    d <- trend_data_all()
    validate(need(nrow(d) > 0, "No data available for this selection."))
    years_selected <- if (is.null(input$years_selected)) unique(d$Year) else as.integer(input$years_selected)
    d <- d %>% filter(Year %in% years_selected)

    # "As of week": don't show data beyond the selected point in time, so
    # the view reflects what the dashboard would have looked like then.
    asof <- parse_asof(input$asof_week_viz)
    d <- d %>% filter(Year < asof$year | (Year == asof$year & Week <= asof$week))
    validate(need(nrow(d) > 0, "No years selected."))

    cols <- YEAR_COLOR_MAP[as.character(sort(unique(d$Year)))]

    case_type <- input$case_type %||% "reported"

    # Year-to-date cumulative totals, computed per calendar year so the
    # hover box can show "how many cases so far this year" alongside the
    # single week's count. NA weeks are treated as 0 for the running total
    # (a missing report isn't the same as a known zero, but for a *running
    # total* silently skipping it is the least misleading option).
    d <- d %>%
      arrange(Year, Week) %>%
      group_by(Year) %>%
      mutate(
        Reported_YTD  = cumsum(ifelse(is.na(Reported), 0, Reported)),
        Projected_YTD = cumsum(ifelse(is.na(Projected), 0, Projected))
      ) %>%
      ungroup()

    # Build a long-format frame: one row per (Year, Week, Series) where
    # Series is Reported (solid) and/or Projected (dashed)
    long_list <- list()
    if (case_type %in% c("reported", "both")) {
      long_list[["Reported"]] <- d %>%
        transmute(Year, Week, Series = "Reported", Cases = Reported, Compliance,
                  tooltip = paste0("Year: ", Year, "<br>Week: ", Week,
                                    "<br>Reported cases: ", comma(Reported),
                                    "<br>YTD reported cases: ", comma(Reported_YTD),
                                    ifelse(is.na(Compliance), "",
                                           paste0("<br>Reporting compliance: ", Compliance, "%"))))
    }
    if (case_type %in% c("projected", "both")) {
      long_list[["Projected"]] <- d %>%
        transmute(Year, Week, Series = "Projected", Cases = Projected, Compliance,
                  tooltip = paste0("Year: ", Year, "<br>Week: ", Week,
                                    "<br>Projected total cases: ", comma(Projected),
                                    "<br>YTD projected cases: ", comma(round(Projected_YTD)),
                                    ifelse(is.na(Compliance), "",
                                           paste0("<br>Reporting compliance: ", Compliance, "%"))))
    }
    dl <- bind_rows(long_list) %>% filter(!is.na(Cases))
    validate(need(nrow(dl) > 0, "No data available for this combination of filters."))

    # Insert explicit NA rows for any week missing from a given Year/Series
    # (within the range of weeks that series actually has data for). A gap
    # in the underlying data isn't the same as zero cases, so the line
    # should break there rather than drawing a straight connector across
    # weeks with no report.
    dl <- dl %>%
      group_by(Year, Series) %>%
      complete(Week = full_seq(Week, 1)) %>%
      ungroup()

    dl <- dl %>% mutate(
      Year_f = factor(Year, levels = sort(unique(Year))),
      group_id = paste(Year, Series)
    )

    # Ticks for every week, but only every 5th week gets a text label
    # (keeps the axis readable across a full 52-week year).
    week_breaks <- seq(floor(min(dl$Week, na.rm = TRUE)), ceiling(max(dl$Week, na.rm = TRUE)), by = 1)
    week_labels <- ifelse(week_breaks %% 5 == 0, as.character(week_breaks), "")

    p <- ggplot(dl, aes(x = Week, y = Cases, group = group_id, color = Year_f,
                        linetype = Series, text = tooltip)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.4, na.rm = TRUE) +
      scale_color_manual(values = cols, name = "Year") +
      scale_linetype_manual(values = c(Reported = "solid", Projected = "dashed"), name = "Series") +
      scale_x_continuous(breaks = week_breaks, labels = week_labels) +
      scale_y_continuous(labels = comma) +
      labs(x = "Week", y = "Number of cases") +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "sans"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E6E7E8"),
        axis.text.x = element_text(size = 9),
        axis.ticks.x = element_line(color = "#8A8F94"),
        axis.ticks.length.x = unit(4, "pt")
      )

    gg <- ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", y = -0.35, title = list(text = "")),
        margin = list(b = 90),
        hoverlabel = list(bgcolor = "#FFFFFF", font = list(color = who_navy, family = "Source Sans Pro")),
        # Small tick mark at every week, but no full-height vertical
        # gridline for every week -- only every 5th week gets a text label
        # (set via week_labels above).
        xaxis = list(
          tickmode = "array", tickvals = week_breaks, ticktext = week_labels,
          ticks = "outside", ticklen = 4, tickwidth = 1, tickcolor = "#8A8F94",
          showgrid = FALSE
        )
      ) %>%
      config(displaylogo = FALSE)

    # ggplotly auto-generates legend labels like "(2026,Reported)" when both
    # colour and linetype vary -- tidy that up to "2026, Reported"
    for (i in seq_along(gg$x$data)) {
      nm <- gg$x$data[[i]]$name
      if (!is.null(nm)) {
        nm <- gsub("^\\(|\\)$", "", nm)
        nm <- gsub(",\\s*", ", ", nm)
        gg$x$data[[i]]$name <- nm
      }
    }
    gg
  })

  # ---------------- Map ----------------
  region_change_data <- reactive({
    req(input$disease, input$asof_week_viz)
    asof <- parse_asof(input$asof_week_viz)
    locs <- location_choices[location_choices != "National"]
    metric <- if (identical(input$case_type_map, "projected")) "Projected" else "Reported"

    region_list <- lapply(locs, function(loc) {
      s <- get_trend_data(input$disease, loc)
      if (nrow(s) == 0) return(data.frame(region = loc, z = NA_real_, status = "absent"))
      stats <- compute_cusum_stats_at(s, asof$year, asof$week, value_col = metric)
      data.frame(
        region = loc,
        z = if (stats$status == "ok") stats$z else NA_real_,
        status = stats$status
      )
    })
    bind_rows(region_list)
  })

  output$map_subtitle <- renderUI({
    req(input$disease, input$location, input$asof_week_viz)
    asof <- parse_asof(input$asof_week_viz)
    div(class = "viz-subtitle",
        paste0("Disease: ", input$disease, ", as of Week ", asof$week, ", ", asof$year))
  })

  output$region_map <- renderPlot({
    if (is.null(pak_regions_sf)) {
      return(
        ggplot() +
          annotate("text", x = 0, y = 0, label = "Map unavailable: Data/pakistan_admin1.geojson could not be read.",
                   color = who_red, size = 5) +
          theme_void()
      )
    }

    rc <- region_change_data()
    sf_map <- pak_regions_sf %>% left_join(rc, by = c("province" = "region"))
    sf_map$z_capped <- pmin(pmax(sf_map$z, -4), 4)

    # Label points guaranteed to sit inside each polygon (unlike a plain
    # centroid, which can fall outside oddly-shaped or multi-part regions)
    label_pts <- suppressWarnings(sf::st_point_on_surface(sf_map))
    coords <- sf::st_coordinates(label_pts)
    status_suffix <- case_when(
      sf_map$status == "ok" ~ paste0("\n(", round(sf_map$z, 1), " SD)"),
      sf_map$status == "no_report" ~ "\n(no reports)",
      sf_map$status == "absent" ~ "\n(not in this week's bulletin)",
      TRUE ~ "\n(insufficient baseline data)"
    )
    label_df <- data.frame(
      x = coords[, 1], y = coords[, 2],
      label = paste0(sf_map$province, status_suffix)
    )

    p <- ggplot(sf_map) +
      geom_sf(aes(fill = z_capped), color = "#8A8F94", linewidth = 0.3) +
      scale_fill_gradient2(
        low = "#1A9850", mid = "#FFFFFF", high = who_red_dark, midpoint = 0,
        limits = c(-4, 4), na.value = "#B7BCC2", name = "SD from\nbaseline",
        labels = function(x) paste0(x, " SD")
      ) +
      ggrepel::geom_text_repel(
        data = label_df, aes(x = x, y = y, label = label),
        size = 3.3, color = who_navy, fontface = "bold", 
        segment.color = who_navy, segment.size = 0.3,
        max.overlaps = Inf, box.padding = 0.4, seed = 42
      ) +
      theme_void(base_size = 13) +
      theme(
        legend.position = "right"
      )

    # Highlight whichever region matches the Location dropdown with a thin
    # navy outline (click-to-filter proved unreliable across environments,
    # so selection now flows one-way from the dropdown to the map instead)
    selected_geom <- sf_map[sf_map$province == input$location, ]
    if (nrow(selected_geom) > 0) {
      p <- p + geom_sf(data = selected_geom, fill = NA, color = who_navy, linewidth = 0.9)
    }

    p
  }, res = 96)

  # ---------------- Regional contribution stacked bar chart --------------
  output$stack_subtitle <- renderUI({
    req(input$disease, input$asof_week_viz)
    asof <- parse_asof(input$asof_week_viz)
    div(class = "viz-subtitle",
        paste0("Disease: ", input$disease, ", Year: ", asof$year, " (up to Week ", asof$week, ")"))
  })

  output$region_stack_plot <- renderPlotly({
    req(input$disease, input$asof_week_viz)
    asof <- parse_asof(input$asof_week_viz)
    metric <- if (identical(input$case_type_stack, "projected")) "Projected" else "Reported"
    locs <- location_choices[location_choices != "National"]

    region_list <- lapply(locs, function(loc) {
      s <- get_trend_data(input$disease, loc) %>% filter(Year == asof$year, Week <= asof$week)
      if (nrow(s) == 0) return(NULL)
      s %>% transmute(Region = loc, Week, Cases = .data[[metric]])
    })
    dstack <- bind_rows(region_list) %>% filter(!is.na(Cases))
    validate(need(nrow(dstack) > 0, "No data available for this selection."))

    dstack <- dstack %>% mutate(
      tooltip = paste0("Region: ", Region, "<br>Week: ", Week, "<br>Cases: ", comma(round(Cases)))
    )

    # Ticks for every week, but only every 5th week gets a text label
    # (same convention as the epi curve above).
    week_breaks <- seq(1L, asof$week, by = 1L)
    week_labels <- ifelse(week_breaks %% 5 == 0, as.character(week_breaks), "")

    p <- ggplot(dstack, aes(x = Week, y = Cases, fill = Region, text = tooltip)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = REGION_COLOR_MAP, name = "Region") +
      scale_x_continuous(breaks = week_breaks, labels = week_labels) +
      scale_y_continuous(labels = comma) +
      labs(x = "Week", y = "Number of cases") +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "sans"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#E6E7E8"),
        axis.text.x = element_text(size = 9)
      )

    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", y = -0.25, title = list(text = "")),
        margin = list(b = 60),
        xaxis = list(
          tickmode = "array", tickvals = week_breaks, ticktext = week_labels,
          ticks = "outside", ticklen = 4, tickwidth = 1, tickcolor = "#8A8F94",
          showgrid = FALSE
        )
      ) %>%
      config(displaylogo = FALSE)
  })

  # ---------------- Weekly table tab ----------------

  weekly_table_reactive <- reactive({
    req(input$location_tbl, input$n_weeks)
    series <- get_all_disease_series(input$location_tbl)
    validate(need(nrow(series) > 0, "No data available for this selection."))

    yr <- max(series$Year, na.rm = TRUE)
    weeks_available <- sort(unique(series$Week[series$Year == yr]))
    n_wk <- min(input$n_weeks, length(weeks_available))
    weeks_to_show <- tail(weeks_available, n_wk)
    diseases_here <- sort(unique(series$Disease))

    value_col <- if (identical(input$case_type_tbl, "projected")) "Projected" else "Reported"

    values_wide <- series %>%
      filter(Year == yr, Week %in% weeks_to_show) %>%
      select(Disease, Week, Value = all_of(value_col)) %>%
      complete(Disease = diseases_here, Week = weeks_to_show, fill = list(Value = NA_real_)) %>%
      mutate(Week_lab = paste0("Wk ", Week)) %>%
      select(Disease, Week_lab, Value)

    week_cols <- paste0("Wk ", weeks_to_show)
    values_wide_wide <- values_wide %>% pivot_wider(names_from = Week_lab, values_from = Value)
    values_wide_wide <- values_wide_wide[, c("Disease", week_cols)]

    sd_matrix <- sapply(weeks_to_show, function(w) {
      sapply(diseases_here, function(dis) {
        s <- series %>% filter(Disease == dis)
        cusum_z_at(s, yr, w, value_col = value_col)
      })
    })
    sd_wide <- as.data.frame(sd_matrix)
    colnames(sd_wide) <- paste0(week_cols, "_sd")

    list(
      display_values = values_wide_wide,
      sd = sd_wide,
      week_cols = week_cols,
      year = yr, n_wk = n_wk, diseases = diseases_here, weeks = weeks_to_show,
      value_col = value_col
    )
  })

  output$weekly_table <- renderDT({
    tbl <- weekly_table_reactive()

    display_df <- cbind(tbl$display_values, tbl$sd)
    sd_col_names <- paste0(tbl$week_cols, "_sd")
    sd_col_index <- which(names(display_df) %in% sd_col_names) - 1

    label <- if (tbl$value_col == "Projected") "projected total" else "reported"

    dt <- datatable(
      display_df,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible = FALSE, targets = sd_col_index))
      ),
      caption = paste0(
        "Weekly ", label, " cases by disease, ", input$location_tbl,
        ", most recent ", tbl$n_wk, " weeks of ", tbl$year
      )
    ) %>%
      formatRound(columns = tbl$week_cols, digits = 0)

    dt <- formatStyle(
      dt,
      columns = tbl$week_cols,
      valueColumns = sd_col_names,
      backgroundColor = styleInterval(SD_BREAKS, SD_BG_PAL),
      color = styleInterval(SD_BREAKS, SD_FONT_PAL)
    )
    dt
  })

  # ---------------- Alerts tab (single list, grouped by disease) ---------
  alerts_reactive <- reactive({
    req(input$asof_week_alerts)
    value_col <- if (identical(input$case_type_alerts, "projected")) "Projected" else "Reported"
    asof <- parse_asof(input$asof_week_alerts)
    compute_alerts_long(value_col = value_col, sel_year = asof$year, sel_week = asof$week)
  })

  output$alerts_output <- renderUI({
    result <- alerts_reactive()
    alerts  <- result$alerts
    missing <- result$missing
    label <- if (identical(input$case_type_alerts, "projected")) "projected total" else "reported"
    asof  <- parse_asof(input$asof_week_alerts)

    note <- p(
      style = "font-size: 12.5px; color:#555;",
      "Alerts use a CUSUM aberration detection method (rolling 9-week baseline, most recent ",
      "2 weeks dropped) applied to each location's own ", strong(label), " cases, as of ",
      strong(paste0("Week ", asof$week, ", ", asof$year)), "."
    )

    level_class <- c(`1` = "qv-box-l1", `2` = "qv-box-l2", `3` = "qv-box-l3")

    alert_section <- if (nrow(alerts) == 0) {
      div(class = "qv-none",
          "No disease currently has any location more than 2 standard deviations above its expected baseline.")
    } else {
      # One box per disease, ordered by its most severe (highest SD level,
      # then z-score) location. Within a box, National is always listed
      # first if it's one of the alerting locations, then the rest ordered
      # by severity -- so "an alert covers both National and a district"
      # always reads with National up front rather than wherever it happens
      # to fall in the severity ranking.
      disease_order <- alerts %>%
        group_by(Disease) %>%
        summarise(max_level = max(Level), .groups = "drop") %>%
        arrange(desc(max_level), Disease) %>%
        pull(Disease)

      lines <- lapply(disease_order, function(dis) {
        d <- alerts %>%
          filter(Disease == dis) %>%
          arrange(desc(Location == "National"), desc(Level), desc(Z))
        max_lvl <- max(d$Level)
        loc_text <- paste0(as.character(d$Location), " (", round(d$Z, 1), "SD)")
        div(class = paste("qv-box", level_class[as.character(max_lvl)]),
            strong(dis), ": ", paste(loc_text, collapse = ", "))
      })
      tagList(lines)
    }

    # Grey flags, grouped by location: diseases explicitly marked NR for
    # the selected week ("no_report"), and diseases with a real report but
    # no usable baseline to evaluate it against. A location that's NR
    # across EVERY disease it's normally evaluated for (i.e. it submitted
    # no report at all that week) collapses to a single line rather than
    # listing every disease name.
    missing_section <- NULL
    if (nrow(missing) > 0) {
      locs_here <- location_choices[location_choices %in% as.character(unique(missing$Location))]
      missing_boxes <- lapply(locs_here, function(loc) {
        d <- missing %>% filter(as.character(Location) == loc)
        no_report_d <- sort(unique(d$Disease[d$Status == "no_report"]))
        insuff_d    <- sort(unique(d$Disease[d$Status %in% c("insufficient_history", "insufficient_baseline")]))

        all_nr <- length(no_report_d) > 0 && identical(result$reported_counts[[loc]] %||% 1L, 0L)

        parts <- character(0)
        if (all_nr) {
          parts <- c(parts, "No reporting (all diseases)")
        } else if (length(no_report_d) > 0) {
          parts <- c(parts, paste0("No reports for ", paste(no_report_d, collapse = ", ")))
        }
        if (length(insuff_d) > 0) parts <- c(parts, paste0("Insufficient baseline data for ", paste(insuff_d, collapse = ", ")))
        div(class = "qv-box qv-box-grey", strong(loc), ": ", paste(parts, collapse = ". "))
      })
      missing_section <- tagList(
        h4("Data gaps", style = "font-size:15px; color:#555; margin-top:24px;"),
        p(style = "font-size: 12.5px; color:#555;",
          "List of locations and diseases where there is no reporting (NR) for the current week or there is ",
          "insufficient data from previous weeks to form a baseline. Diseases not in this week's bulletin are not shown."),
        missing_boxes
      )
    }

    tagList(note, alert_section, missing_section)
  })
}

shinyApp(ui, server)
