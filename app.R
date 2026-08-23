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
#   www/logo.png          
#   Data/PAK_IDSR_Data.csv
#   Data/PAK_IDSR_Compliance.csv
#   Data/PAK_IDSR_Data_District.csv
#   Data/PAK_IDSR_Compliance_District.csv
#   Data/pakistan_admin1.geojson             <- dissolved province boundaries (Data visualisation tab's map)
#   Data/pak_admin_boundaries/pak_admin1.geojson  <- province boundaries (District-level data tab's map outline)
#   Data/pak_admin_boundaries/pak_admin2_jaffarabad_split.geojson  <- district boundaries (District-level
#     data tab's map fill). This is pak_admin2.geojson with its single "Jaffarabad" polygon split into two,
#     built from the admin3 (tehsil) boundaries in Data/pak_admin_boundaries/pak_admin3.geojson: "Usta
#     Muhammad" (union of the Gandakha + Usta Mohammad tehsils) and "Jaffarabad" (the remaining Jhat Pat
#     tehsil). This lets IDSR district data reported separately for Jaffarabad and Usta Muhammad each match
#     their own polygon, instead of both only ever being able to match the old single combined polygon.
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
library(leaflet)

REGIONS_GEOJSON <- "Data/pakistan_admin1.geojson"

# Admin boundary files used by the District-level data tab's map -- distinct
# from REGIONS_GEOJSON above (which is a dissolved 7-province shape used by
# the Data visualisation tab's static province map). These are more detailed
# administrative boundary files, used at their adm2 ("district") level so
# every individual district can be coloured and hovered on the map; adm1 is
# used only to draw a thin province outline on top for visual clarity.
DISTRICT_ADMIN1_GEOJSON <- "Data/pak_admin_boundaries/pak_admin1.geojson"
# pak_admin2_jaffarabad_split.geojson is pak_admin2.geojson with the single "Jaffarabad" polygon replaced by
# two polygons -- "Usta Muhammad" (Gandakha + Usta Mohammad tehsils) and "Jaffarabad" (the remaining Jhat Pat
# tehsil) -- built from Data/pak_admin_boundaries/pak_admin3.geojson so that CSV rows for each district match
# their own polygon. See the file-inventory comment near the top of this file for how it was derived.
DISTRICT_ADMIN2_GEOJSON <- "Data/pak_admin_boundaries/pak_admin2_jaffarabad_split.geojson"

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

# ---- Load district-level data ----------------------------------------------
# District-level breakdowns of the same weekly bulletins, used by the
# "District-level data" tab. Note that district-level case data is
# currently only published/extracted for a subset of provinces (whichever
# ones the source bulletin breaks down by district that week) -- the tab
# naturally only shows whichever provinces/districts are present.
DISTRICT_DATA_PATH       <- "Data/PAK_IDSR_Data_District.csv"
DISTRICT_COMPLIANCE_PATH <- "Data/PAK_IDSR_Compliance_District.csv"

raw_district_data <- read_csv(DISTRICT_DATA_PATH, show_col_types = FALSE) %>%
  mutate(
    Disease  = as.character(Disease),
    Province = as.character(Province),
    District = as.character(District),
    Cases    = suppressWarnings(as.numeric(Cases)),
    Week     = as.integer(Week),
    Year     = as.integer(Year),
    Status   = as.character(Status)
  ) %>%
  filter(!is.na(Week), !is.na(Year), !is.na(Province), !is.na(District)) %>%
  # Same "keep a real report or an explicit non-report row, drop anything
  # else" rule as raw_data above. The district pipeline uses both "NR" and
  # "Missing" for a row that's present but has no usable case count -- both
  # are treated the same way (Cases NA, row kept) as "no report" downstream.
  filter(!is.na(Cases) | Status %in% c("NR", "Missing"))

# The extraction pipeline appends the source bulletin's own printed
# province "Total" row alongside the real district rows, as a pseudo-row
# with District == "Total" (see convert_district_cases_table() in
# 1_1_DownloadData_PAK_v4.R). It's split out here into its own table --
# used by get_province_disease_series() below as the authoritative
# province-level figure -- and removed from raw_district_data so it never
# shows up as a fake "district" in the per-district rows/choices built from
# that table.
raw_district_totals <- raw_district_data %>% filter(District == "Total")
raw_district_data    <- raw_district_data %>% filter(District != "Total")

# A district/week can have more than one row here -- e.g. a separate "IDSR
# Reporting Site" row and a "Tertiary Care Hospital" row for the same
# district and week, each with its own Total/Reported Sites and Compliance
# (%). These are combined into a single district/week compliance figure by
# summing sites across facility types and recomputing the percentage from
# those totals, rather than picking or averaging the individual rows'
# already-computed percentages (which would need a sites-weighted
# combination to be correct, and summing the raw counts gets there
# directly). Same division-by-zero-artefact guard as compliance_data above:
# a district/week with 0 or missing total sites gets Compliance = NA, never
# mistaken for 100%.
district_compliance_data <- read_csv(DISTRICT_COMPLIANCE_PATH, show_col_types = FALSE) %>%
  mutate(
    Region          = as.character(Region),
    District        = as.character(District),
    `Total Sites`    = suppressWarnings(as.numeric(`Total Sites`)),
    `Reported Sites` = suppressWarnings(as.numeric(`Reported Sites`)),
    Week = as.integer(Week),
    Year = as.integer(Year)
  ) %>%
  filter(!is.na(Region), !is.na(District)) %>%
  group_by(Region, District, Week, Year) %>%
  summarise(
    `Total Sites`    = sum(`Total Sites`, na.rm = TRUE),
    `Reported Sites` = sum(`Reported Sites`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Compliance = ifelse(`Total Sites` <= 0, NA_real_, 100 * `Reported Sites` / `Total Sites`)
  ) %>%
  select(Province = Region, District, Week, Year, Compliance)

district_disease_choices <- sort(unique(raw_district_data$Disease))

# ---- Which provinces actually have district-level data -------------------
# Used to grey out provinces on the District-level data map, and to note near
# the top of that tab which provinces are/aren't covered.
provinces_with_district_data    <- sort(unique(raw_district_data$Province))
provinces_without_district_data <- setdiff(location_choices[location_choices != "National"], provinces_with_district_data)

# ---- Matching CSV district names onto admin-boundary polygons ------------
# Data/pak_admin_boundaries/pak_admin2.geojson is an independently-sourced
# boundary file, so a district's name there doesn't always spell/order
# identically to how it appears in the IDSR bulletins (which is what
# PAK_IDSR_Data_District.csv's District column preserves verbatim). This
# normalises both sides onto the same simplified key (lower-case, letters
# only, parenthetical alternate names dropped, a leading "SD " sub-division
# marker dropped) so most districts line up automatically; a short alias
# table below covers the handful of remaining mismatches (transliteration/
# word-order differences, or a district split into multiple CSV reporting
# sub-units -- e.g. KP's "L & C Kurram" and "Upper Kurram" -- that share a
# single boundary polygon). A district that still doesn't match anything
# simply won't be found on the map; it remains fully present in the table
# regardless, which is the authoritative view.
normalize_dist_name <- function(x) {
  x <- tolower(x)
  x <- gsub("\\([^)]*\\)", "", x)   # drop "(Bolan)"-style parenthetical alt names
  x <- trimws(x)
  x <- sub("^sd\\s+", "", x)         # drop a leading "SD " (sub-division) marker
  x <- gsub("[^a-z]", "", x)         # strip everything but letters (spaces, ., &, -)
  x
}

DISTRICT_NAME_ALIASES <- c(
  dirlower    = "lowerdir",
  dirupper    = "upperdir",
  kolaipalas  = "kolaipalaskohistan",
  lckurram    = "kurram",
  upperkurram = "kurram",
  naseerabad  = "nasirabad",
  lasbella    = "lasbela",
  # CSV "Battagram" vs. boundary "Batagram"
  battagram          = "batagram",
  # CSV "Kamber Shadadkot" vs. boundary "Kambar Shahdad Kot"
  kambershadadkot    = "kambarshahdadkot",
  # CSV "Naushero Feroze" vs. boundary "Naushahro Feroze"
  nausheroferoze     = "naushahroferoze",
  # Karachi's 6 CSV sub-districts are worded "Karachi <X>"; the boundary
  # file words them "<X> Karachi"
  karachimalir       = "malirkarachi",
  karachieast        = "eastkarachi",
  karachikorangi     = "korangikarachi",
  karachicentral     = "centralkarachi",
  karachisouth       = "southkarachi",
  karachiwest        = "westkarachi",
  # ---- Map-only concordances (aggregated onto a neighbouring polygon; the
  # reporting table/elsewhere keeps these as their own separate districts) --
  # the boundary file has no polygon of its own for either of these, so
  # their disease counts are folded into the polygon of the district they
  # sit within/adjoin, purely for the purposes of colouring the map.
  hub                = "lasbela",             # Hub -> Lasbela
  karachikeamari     = "southkarachi"         # Karachi Keamari -> South Karachi
)

resolve_dist_key <- function(x) {
  k <- normalize_dist_name(x)
  aliased <- unname(DISTRICT_NAME_ALIASES[k])
  ifelse(is.na(aliased), k, aliased)
}

# ---- Map-only concordances worth narrating on the District map's ----------
# boundary-adjustment note (see district_map_boundary_note_reactive() in the
# server below). Kept as its own small, explicit list rather than trying to
# auto-detect "genuine merge into a different district" vs. "same district,
# alternate spelling" from every entry in DISTRICT_NAME_ALIASES above (e.g.
# "Battagram"/"Batagram" is still fundamentally the same district and isn't
# the kind of adjustment worth calling out to the user here).
DISTRICT_MAP_CONCORDANCES_TO_NARRATE <- list(
  list(district = "Hub",              included_in = "Lasbela"),
  list(district = "Karachi Keamari",  included_in = "South Karachi")
)

# ---- Province code <-> admin-boundary adm1_name ---------------------------
PROVINCE_ADM1_NAME_MAP <- c(
  AJK = "Azad Kashmir", Balochistan = "Balochistan", GB = "Gilgit Baltistan",
  ICT = "Islamabad", KP = "Khyber Pakhtunkhwa", Punjab = "Punjab", Sindh = "Sindh"
)
ADM1_NAME_PROVINCE_MAP <- setNames(names(PROVINCE_ADM1_NAME_MAP), PROVINCE_ADM1_NAME_MAP)

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

# ---- Helper: the N weeks leading up to (and including) an "as of" week ----
# Used by the Weekly summary table and District-level data tabs' "As of
# week" selector. Walks back through `week_calendar`'s global chronological
# week_idx -- the same mechanism compute_cusum_stats_at() above uses for its
# baseline lookback -- rather than a per-year tail(), so the window is
# correct even when it reaches back across a year boundary (e.g. "as of"
# Week 3, 2026 with 8 weeks requested pulls in Weeks 48-52, 2025 too). If
# the as-of week has fewer than n_weeks of history behind it, the window is
# simply shorter (starts at week_idx 1) rather than erroring.
# Returns a data frame with one row per week, in chronological order:
# Year, Week, and week_lab (the display label -- "Wk N", or "Wk N 'YY" when
# the window spans more than one year, so columns stay unambiguous).
weeks_up_to <- function(asof, n_weeks) {
  target_idx_v <- week_calendar$week_idx[week_calendar$Year == asof$year & week_calendar$Week == asof$week]
  target_idx <- if (length(target_idx_v) > 0) target_idx_v[1] else max(week_calendar$week_idx)
  start_idx  <- max(1, target_idx - n_weeks + 1)

  wc <- week_calendar[week_calendar$week_idx >= start_idx & week_calendar$week_idx <= target_idx, c("Year", "Week")]
  wc <- wc[order(wc$Year, wc$Week), ]

  multi_year <- length(unique(wc$Year)) > 1
  wc$week_lab <- if (multi_year) paste0("Wk ", wc$Week, " '", substr(wc$Year, 3, 4)) else paste0("Wk ", wc$Week)
  wc
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
    summarise(
      Cases = if (all(is.na(Cases))) NA_real_ else sum(Cases, na.rm = TRUE),
      # A blank case count is only ever shown to the user as "NR" when it's a
      # genuine, explicit Not-Reported submission -- i.e. every row
      # contributing to this Disease/Year/Week is itself marked NR (and so
      # has no case count at all). This is deliberately Status-based rather
      # than just "Cases is NA", since Cases can also be NA for OTHER
      # reasons (e.g. Projected being unavailable because compliance data is
      # missing) that aren't a reporting gap and shouldn't be labelled NR.
      IsNR = is.na(Cases) && all(Status == "NR"),
      .groups = "drop"
    ) %>%
    mutate(join_key = if (location == "National") "National" else location) %>%
    left_join(compliance_data, by = c("join_key" = "Region", "Week" = "Week", "Year" = "Year")) %>%
    mutate(Projected = ifelse(is.na(Compliance) | Compliance <= 0, NA_real_, Cases / (Compliance / 100))) %>%
    select(Disease, Year, Week, Reported = Cases, Projected, Compliance, IsNR)
}

# ---- Helper: district-level weekly series for a single disease, across ----
# every province/district that has district-level data for it. Projected
# uses the matching district's own compliance (Province + District + Week +
# Year), the same reported/(compliance/100) formula as get_trend_data().
get_district_disease_series <- function(disease) {
  raw_district_data %>%
    filter(Disease == disease) %>%
    group_by(Province, District, Year, Week) %>%
    # See get_trend_data() above for why this isn't a plain sum(na.rm=TRUE).
    summarise(
      Cases = if (all(is.na(Cases))) NA_real_ else sum(Cases, na.rm = TRUE),
      # Only a genuine, explicit "NR" is ever shown as "NR" -- NOT the
      # district pipeline's separate "Missing" status (see raw_district_data's
      # loading comment above), which stays blank like any other gap. See
      # get_all_disease_series() above for why this checks Cases as well.
      IsNR = is.na(Cases) && all(Status == "NR"),
      .groups = "drop"
    ) %>%
    left_join(district_compliance_data, by = c("Province", "District", "Week", "Year")) %>%
    mutate(Projected = ifelse(is.na(Compliance) | Compliance <= 0, NA_real_, Cases / (Compliance / 100))) %>%
    select(Province, District, Year, Week, Reported = Cases, Projected, Compliance, IsNR)
}

# ---- Helper: province-level totals for the District-level data tab ------
# Used for the parent (province) rows, and for CUSUM shading of those
# province totals.
#
# Reported comes directly from the province's own printed "Total" row in
# the source bulletin (raw_district_totals, split out of raw_district_data
# above) -- NOT a sum of whichever individual district rows this pipeline
# managed to extract that week. Summing is only an approximation of the
# true total, since it silently undercounts whenever even one district's
# row failed to parse or wasn't captured; the bulletin's own printed Total
# is authoritative. A province/week with no usable printed Total (the
# bulletin's table had none, or it failed to extract) shows NA here --
# deliberately never falls back to a computed sum. IsNR reflects the Total
# row's own printed status, the same way get_district_disease_series()
# reflects a district's own NR -- not derived from the individual district
# rows.
#
# Projected has no equivalent bulletin-printed figure to draw on (it's
# this pipeline's own compliance-adjusted estimate, not something the
# bulletin prints), so it's still summed from the district-level Projected
# values, same as before -- missing districts are simply excluded from the
# sum, not treated as zero, consistent with the aggregation rule used
# throughout (see get_trend_data()/get_all_disease_series() above).
get_province_disease_series <- function(district_series, disease) {
  totals <- raw_district_totals %>%
    filter(Disease == disease) %>%
    group_by(Province, Year, Week) %>%
    summarise(
      Reported = if (all(is.na(Cases))) NA_real_ else sum(Cases, na.rm = TRUE),
      IsNR     = is.na(Reported) && all(Status == "NR"),
      .groups = "drop"
    )

  projected <- district_series %>%
    group_by(Province, Year, Week) %>%
    summarise(
      Projected = if (all(is.na(Projected))) NA_real_ else sum(Projected, na.rm = TRUE),
      .groups = "drop"
    )

  full_join(totals, projected, by = c("Province", "Year", "Week")) %>%
    # A province/week present only on the projected side (no printed Total
    # row at all) is missing, not NR -- IsNR specifically means the
    # bulletin printed "NR" for that Total, which requires a Total row to
    # exist in the first place.
    mutate(IsNR = ifelse(is.na(IsNR), FALSE, IsNR)) %>%
    select(Province, Year, Week, Reported, Projected, IsNR)
}

# ---- Helper: district series aggregated onto MAP POLYGONS -----------------
# A handful of CSV "District" values are administrative sub-units that share
# a single polygon in the admin2 boundary file (see resolve_dist_key()'s
# comment above) -- these are summed together (same NA-handling rule as
# get_province_disease_series() above) before computing a single CUSUM
# z-score for that polygon, rather than trying to average multiple
# already-computed z-scores (which would not be statistically meaningful).
get_district_polygon_series <- function(district_series) {
  district_series %>%
    mutate(dist_key = resolve_dist_key(District)) %>%
    group_by(Province, dist_key, Year, Week) %>%
    summarise(
      Reported  = if (all(is.na(Reported)))  NA_real_ else sum(Reported,  na.rm = TRUE),
      Projected = if (all(is.na(Projected))) NA_real_ else sum(Projected, na.rm = TRUE),
      IsNR      = is.na(Reported) && all(IsNR),
      .groups = "drop"
    )
}

# ---- Helper: one row per map polygon with its CUSUM stats at a given week -
# Used by the District-level data tab's map. `asof` is a list(year, week) as
# returned by parse_asof(). Returns Province, dist_key, z (NA unless status is
# "ok"), status, Reported, Projected at that week -- compute_cusum_stats_at()
# is defined further below in this file, which is fine since this function
# body isn't evaluated until it's actually called at render time.
get_district_map_data <- function(disease, asof, value_col = "Reported") {
  district_series <- get_district_disease_series(disease)
  if (nrow(district_series) == 0) {
    return(data.frame(Province = character(), dist_key = character(), z = numeric(),
                       status = character(), Reported = numeric(), Projected = numeric(),
                       stringsAsFactors = FALSE))
  }
  poly_series <- get_district_polygon_series(district_series)
  keys <- poly_series %>% distinct(Province, dist_key)

  rows <- lapply(seq_len(nrow(keys)), function(i) {
    p  <- keys$Province[i]
    dk <- keys$dist_key[i]
    s  <- poly_series %>% filter(Province == p, dist_key == dk) %>% arrange(Year, Week)
    stats <- compute_cusum_stats_at(s, asof$year, asof$week, value_col = value_col)
    cur <- s[s$Year == asof$year & s$Week == asof$week, ]
    data.frame(
      Province = p, dist_key = dk,
      z = if (stats$status == "ok") stats$z else NA_real_,
      status = stats$status,
      Reported  = if (nrow(cur) > 0) cur$Reported[1]  else NA_real_,
      Projected = if (nrow(cur) > 0) cur$Projected[1] else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  bind_rows(rows)
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

# ---- Shared continuous diverging colour scale for SD-from-baseline -------
# Same CUSUM/C2 method as the Alerts tab (rolling 9-week window, 2-week
# guard band, 7-week baseline). This is the SAME scale used by the Data
# visualisation tab's region map (scale_fill_gradient2(low = SD_SCALE_LOW,
# mid = SD_SCALE_MID, high = SD_SCALE_HIGH, midpoint = 0, limits = c(-lim,
# lim))) -- kept as shared constants so the Weekly summary table, the
# District-level data table, and the District-level data map all read on
# exactly the same green-white-red gradient, clamped at +-4 SD.
SD_SCALE_LOW   <- "#1A9850"   # 4+ SD below baseline
SD_SCALE_MID   <- "#FFFFFF"   # at baseline
SD_SCALE_HIGH  <- who_red_dark  # 4+ SD above baseline
SD_SCALE_LIMIT <- 4

# R-side continuous colour function (used by the District-level data map's
# leaflet fill). A smooth 3-point linear interpolation across RGB space,
# clamped at +-SD_SCALE_LIMIT.
sd_continuous_colour <- local({
  ctrl_x   <- c(-SD_SCALE_LIMIT, 0, SD_SCALE_LIMIT)
  ctrl_col <- c(SD_SCALE_LOW, SD_SCALE_MID, SD_SCALE_HIGH)
  ctrl_rgb <- grDevices::col2rgb(ctrl_col)
  function(z) {
    out <- rep(NA_character_, length(z))
    ok  <- !is.na(z)
    if (any(ok)) {
      zc <- pmin(pmax(z[ok], -SD_SCALE_LIMIT), SD_SCALE_LIMIT)
      r <- approx(ctrl_x, ctrl_rgb["red", ],   xout = zc)$y
      g <- approx(ctrl_x, ctrl_rgb["green", ], xout = zc)$y
      b <- approx(ctrl_x, ctrl_rgb["blue", ],  xout = zc)$y
      out[ok] <- grDevices::rgb(r, g, b, maxColorValue = 255)
    }
    out
  }
})

# R-side twin of sd_continuous_font_js() below -- same luminance rule,
# used wherever a cell's background is computed in R (via
# sd_continuous_colour() above) rather than client-side by DT, e.g. the
# Alerts tab's per-alert week-by-week row.
sd_continuous_font_colour <- function(z) {
  bg  <- sd_continuous_colour(z)
  out <- rep(NA_character_, length(bg))
  ok  <- !is.na(bg)
  if (any(ok)) {
    rgb_vals <- grDevices::col2rgb(bg[ok])
    lum <- (0.299 * rgb_vals["red", ] + 0.587 * rgb_vals["green", ] + 0.114 * rgb_vals["blue", ]) / 255
    out[ok] <- ifelse(lum < 0.55, "#FFFFFF", "#111111")
  }
  out
}

# ---- Helper: a small, static (non-DT) week-by-week table for the Alerts -
# tab's per-disease dropdown -- a plain styled HTML table (rather than
# standing up a dynamic DT output per disease) shaded with the exact same
# continuous SD scale as the Weekly summary table / District-level data
# table (sd_continuous_colour()/sd_continuous_font_colour() above are the
# R-side twins of that table's client-side JS versions).
#
# One series' week-by-week <td> cells (values + SD shading), shared by
# every row of build_alert_region_table() below.
build_alert_week_cells <- function(series, weeks_cal, value_col) {
  cell_at <- function(i) {
    r <- series[series$Year == weeks_cal$Year[i] & series$Week == weeks_cal$Week[i], ]
    list(
      value = if (nrow(r) == 0) NA_real_ else r[[value_col]][1],
      is_nr = if (nrow(r) == 0) FALSE else isTRUE(r$IsNR[1])
    )
  }
  cells <- lapply(seq_len(nrow(weeks_cal)), cell_at)
  zs    <- sapply(seq_len(nrow(weeks_cal)), function(i) cusum_z_at(series, weeks_cal$Year[i], weeks_cal$Week[i], value_col = value_col))
  bgs   <- sd_continuous_colour(zs)
  fts   <- sd_continuous_font_colour(zs)

  lapply(seq_along(cells), function(i) {
    c_ <- cells[[i]]
    txt <- if (c_$is_nr) {
      tags$span(style = "color:#888; font-style:italic;", "NR")
    } else if (is.na(c_$value)) {
      ""
    } else {
      format(round(c_$value), big.mark = ",")
    }
    cell_bg <- if (is.na(bgs[i])) "#FFFFFF" else bgs[i]
    cell_ft <- if (is.na(fts[i])) "#111111" else fts[i]
    tags$td(
      style = sprintf(
        "padding:4px 8px; font-size:12.5px; text-align:center; background-color:%s; color:%s; border-bottom:1px solid #F0F2F4;",
        cell_bg, cell_ft
      ),
      txt
    )
  })
}

# ---- Helper: one disease's alert dropdown -- a single table with one ----
# row per alerted region. Used by the Alerts tab: each disease box has ONE
# dropdown (not one per region), and expanding it shows every alerted
# region's week-by-week case counts side by side, shaded with the same
# continuous SD scale used elsewhere. Each region's own label cell also
# carries the "Data Visualisation" link (posts {disease, location} via
# Shiny.setInputValue(), picked up by the alerts_goto observer in
# server()) so the two are visually tied to that specific region rather
# than living in a separate row/element.
build_alert_region_table <- function(dis, locs, asof, value_col, n_weeks = 12) {
  weeks_cal <- weeks_up_to(asof, n_weeks)

  header_cells <- c(
    list(tags$th(style = "padding:4px 8px; font-size:11px; color:#555; font-weight:600; background-color:#FFFFFF; border-bottom:1px solid #DDE1E4; text-align:left;", "Region")),
    lapply(weeks_cal$week_lab, function(wl) {
      tags$th(style = "padding:4px 8px; font-size:11px; color:#555; font-weight:600; background-color:#FFFFFF; border-bottom:1px solid #DDE1E4;", wl)
    })
  )

  body_rows <- lapply(locs, function(loc) {
    series <- get_all_disease_series(loc) %>% filter(Disease == dis)
    value_cells <- build_alert_week_cells(series, weeks_cal, value_col)
    goto_payload <- jsonlite::toJSON(list(disease = dis, location = loc), auto_unbox = TRUE)

    label_cell <- tags$td(
      style = "padding:4px 8px; font-size:12.5px; white-space:nowrap; vertical-align:top; background-color:#FFFFFF; border-bottom:1px solid #F0F2F4;",
      div(strong(loc)),
      tags$a(
        href = "#",
        style = "font-size:11px; color:var(--who-blue); font-weight:600; text-decoration:none;",
        onclick = sprintf(
          "Shiny.setInputValue('alerts_goto', %s, {priority:'event'}); return false;",
          goto_payload
        ),
        "Data Visualisation →"
      )
    )
    tags$tr(c(list(label_cell), value_cells))
  })

  # Table and legend wrapped together in one white bordered card, so the
  # colour-scale key reads as directly attached to the table it explains
  # rather than a separate floating element.
  tags$div(
    style = "background-color:#FFFFFF; border:1px solid #E5E8EB; border-radius:5px; overflow:hidden;",
    div(
      style = "overflow-x:auto;",
      tags$table(
        style = "border-collapse:collapse; width:100%; background-color:#FFFFFF; margin:0;",
        tags$thead(tags$tr(header_cells)),
        tags$tbody(body_rows)
      )
    ),
    div(
      style = "padding:10px 14px; background-color:#FFFFFF; border-top:1px solid #EEF1F4;",
      tags$p(
        style = "font-size:12px; color:#555; margin:0 0 8px 0;",
        "Each week's cell is coloured using the CUSUM aberration method based on the 9-week baseline ",
        strong(style = "color:inherit;", "leading up to"),
        " that week (with the two closest weeks dropped)."
      ),
      sd_gradient_legend_ui(show_no_data = FALSE)
    )
  )
}

# JS-side equivalents of the same 3-point gradient, for continuous cell
# shading in the Weekly summary table and District-level data table (DT
# tables shade client-side, so the colour math needs a JS twin of
# sd_continuous_colour() above). These build a JS *expression* -- not a
# full function -- meant to be dropped into DT::formatStyle(), where the
# variable `value` is already in scope for whichever cell is being styled.
# `value` is NULL for weeks with no SD (no report / insufficient baseline),
# in which case no background/font colour override is applied (the cell
# keeps its default look; those weeks already read "NR" in italics).
sd_continuous_bg_js <- function() {
  JS(sprintf(
    "(function() {
       if (value === null || value === undefined || isNaN(value)) return '';
       var lim = %s, lo = '%s', mid = '%s', hi = '%s';
       var z = Math.max(-lim, Math.min(lim, value));
       function hex2rgb(h) { return [parseInt(h.substr(1,2),16), parseInt(h.substr(3,2),16), parseInt(h.substr(5,2),16)]; }
       var a = z <= 0 ? hex2rgb(lo) : hex2rgb(mid);
       var b = z <= 0 ? hex2rgb(mid) : hex2rgb(hi);
       var t = z <= 0 ? (z + lim) / lim : z / lim;
       var r = Math.round(a[0] + (b[0]-a[0])*t);
       var g = Math.round(a[1] + (b[1]-a[1])*t);
       var bl = Math.round(a[2] + (b[2]-a[2])*t);
       return 'rgb(' + r + ',' + g + ',' + bl + ')';
     })()",
    SD_SCALE_LIMIT, SD_SCALE_LOW, SD_SCALE_MID, SD_SCALE_HIGH
  ))
}
sd_continuous_font_js <- function() {
  JS(sprintf(
    "(function() {
       if (value === null || value === undefined || isNaN(value)) return '';
       var lim = %s, lo = '%s', mid = '%s', hi = '%s';
       var z = Math.max(-lim, Math.min(lim, value));
       function hex2rgb(h) { return [parseInt(h.substr(1,2),16), parseInt(h.substr(3,2),16), parseInt(h.substr(5,2),16)]; }
       var a = z <= 0 ? hex2rgb(lo) : hex2rgb(mid);
       var b = z <= 0 ? hex2rgb(mid) : hex2rgb(hi);
       var t = z <= 0 ? (z + lim) / lim : z / lim;
       var r = a[0] + (b[0]-a[0])*t, g = a[1] + (b[1]-a[1])*t, bl = a[2] + (b[2]-a[2])*t;
       var lum = (0.299*r + 0.587*g + 0.114*bl) / 255;
       return lum < 0.55 ? '#FFFFFF' : '#111111';
     })()",
    SD_SCALE_LIMIT, SD_SCALE_LOW, SD_SCALE_MID, SD_SCALE_HIGH
  ))
}

# Reusable UI: a horizontal colour-bar legend for the shared SD scale above,
# used on the Weekly summary table tab, the District-level data table, and
# the District-level data map, so all three explain the same continuous
# scale the same way. `show_no_data` adds a grey swatch for "no data
# available" (used on the map, where whole provinces/districts can be
# shaded grey; not needed on the tables, where a missing week just reads
# "NR" in text rather than being shaded).
sd_gradient_legend_ui <- function(show_no_data = FALSE) {
  tagList(
    div(
      style = sprintf(
        "height:14px; border-radius:3px; border:1px solid #C7CBD1; background:linear-gradient(to right, %s 0%%, %s 50%%, %s 100%%);",
        SD_SCALE_LOW, SD_SCALE_MID, SD_SCALE_HIGH
      )
    ),
    div(
      style = "display:flex; justify-content:space-between; font-size:11px; color:#555; margin-top:3px;",
      span(paste0("-", SD_SCALE_LIMIT, " SD")), span("-2 SD"), span("0"), span("+2 SD"), span(paste0("+", SD_SCALE_LIMIT, " SD"))
    ),
    if (show_no_data) {
      div(
        style = "display:flex; align-items:center; gap:6px; margin-top:8px;",
        span(style = paste0(
          "display:inline-block; width:16px; height:16px; border-radius:3px; border:1px solid #C9CDD1; flex-shrink:0; background-color:",
          DISTRICT_NO_DATA_COLOUR, ";"
        )),
        span(style = "font-size:12.5px; color:#333;", "No data available")
      )
    }
  )
}

# Fill colour for districts/provinces with no usable value on the map --
# one shared grey for BOTH "a whole province with no subnational reporting
# at all" and "a district within a covered province with no report / no
# usable value this week", so the map reads with a single, consistent
# "no data" colour rather than two similar-but-different greys.
DISTRICT_NO_DATA_COLOUR              <- "#E8ECEF"
DISTRICT_PROVINCE_NOT_COVERED_COLOUR <- DISTRICT_NO_DATA_COLOUR

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

# ---- Helper: district-map boundary files (adm1 outline + adm2 fill) -------
load_pak_boundary_file <- function(path) {
  tryCatch({
    if (!file.exists(path)) stop("boundary file not found at ", path)
    sf_obj <- sf::st_read(path, quiet = TRUE)
    sf::st_make_valid(sf_obj)
  }, error = function(e) {
    message("Could not load boundaries from ", path, ": ", conditionMessage(e))
    NULL
  })
}

pak_district_admin1_sf <- load_pak_boundary_file(DISTRICT_ADMIN1_GEOJSON)
pak_district_admin2_sf <- load_pak_boundary_file(DISTRICT_ADMIN2_GEOJSON)

if (!is.null(pak_district_admin2_sf)) {
  # The boundary file's own polygon for this district is labelled with its
  # FORMER administrative name, "Shaheed Sikandarabad" -- current bulletins
  # report it as "Surab", so the polygon is relabelled here (map-wide,
  # wherever adm2_name is displayed) before dist_key is derived from it,
  # which also makes CSV "Surab" resolve to this polygon directly with no
  # separate alias needed (see normalize_dist_name()/DISTRICT_NAME_ALIASES
  # above).
  pak_district_admin2_sf$adm2_name[pak_district_admin2_sf$adm2_name == "Shaheed Sikandarabad"] <- "Surab"

  pak_district_admin2_sf$province_code <- unname(ADM1_NAME_PROVINCE_MAP[pak_district_admin2_sf$adm1_name])
  pak_district_admin2_sf$dist_key      <- normalize_dist_name(pak_district_admin2_sf$adm2_name)
}
if (!is.null(pak_district_admin1_sf)) {
  pak_district_admin1_sf$province_code <- unname(ADM1_NAME_PROVINCE_MAP[pak_district_admin1_sf$adm1_name])
}

# ---- Helper: standard Web Mercator lng/lat -> global pixel coordinates --
# at a given zoom (the same slippy-map tile projection Leaflet itself
# uses internally). Used by declutter_label_offsets() below to reason
# about label spacing in genuine on-screen pixels.
lonlat_to_pixel <- function(lng, lat, zoom) {
  siny <- sin(lat * pi / 180)
  siny <- pmin(pmax(siny, -0.9999), 0.9999)
  x <- 256 * (0.5 + lng / 360) * 2^zoom
  y <- 256 * (0.5 - log((1 + siny) / (1 - siny)) / (4 * pi)) * 2^zoom
  cbind(x, y)
}

# ---- Helper: crude ggrepel-style label decluttering for leaflet markers -
# Leaflet has no built-in label-collision avoidance (and ggrepel itself
# only works with static ggplot2 output, not an interactive htmlwidget),
# so this is a small hand-rolled stand-in for the province leaflet map's
# permanent on-map labels: project each label's lng/lat anchor to screen
# pixels at a reference zoom, estimate each label's rough on-screen box
# from its text length, and run a few iterations of simple pairwise
# repulsion -- any two overlapping boxes get pushed apart along whichever
# axis has the smaller overlap (the standard cheap AABB-declutter
# heuristic), same spirit as ggrepel's box-repulsion but computed by hand.
# Returns one (dx, dy) PIXEL offset per point, meant for
# labelOptions(offset = ...) -- a fixed screen-space shift from the
# marker's true anchor, so it stays put across pan/zoom even though it
# was only computed for one reference zoom.
declutter_label_offsets <- function(lng, lat, name_lines, sub_lines, zoom = 5, iterations = 60) {
  px <- lonlat_to_pixel(lng, lat, zoom)
  n  <- nrow(px)
  # Rough box half-extents in pixels, from text length -- a bold ~12px
  # name line and a slightly smaller status line, both centred.
  half_w <- pmax(nchar(name_lines), nchar(sub_lines)) * 3.4 + 6
  half_h <- rep(16, n)

  pos <- px
  if (n > 1) {
    for (iter in seq_len(iterations)) {
      moved <- FALSE
      for (i in seq_len(n - 1)) {
        for (j in (i + 1):n) {
          dx <- pos[j, 1] - pos[i, 1]
          dy <- pos[j, 2] - pos[i, 2]
          overlap_x <- (half_w[i] + half_w[j]) - abs(dx)
          overlap_y <- (half_h[i] + half_h[j]) - abs(dy)
          if (overlap_x > 0 && overlap_y > 0) {
            moved <- TRUE
            if (overlap_x < overlap_y) {
              shift <- overlap_x / 2 + 0.5
              s <- if (dx == 0) 1 else sign(dx)
              pos[i, 1] <- pos[i, 1] - s * shift
              pos[j, 1] <- pos[j, 1] + s * shift
            } else {
              shift <- overlap_y / 2 + 0.5
              s <- if (dy == 0) 1 else sign(dy)
              pos[i, 2] <- pos[i, 2] - s * shift
              pos[j, 2] <- pos[j, 2] + s * shift
            }
          }
        }
      }
      if (!moved) break
    }
  }
  pos - px
}

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
          style = "max-width: 1200px; margin: 0; padding-left: 24px;",
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
                    "given week, most likely because there were no cases to report. In this case, we also treat the values ",
                    "as missing rather than assuming they are zero.")
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
          div(
            style = "font-size: 12.5px; color: #555;",
            p(style = "margin-bottom: 6px;",
              strong("Calculations:")),
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
        ),
        fluidRow(
          column(
            width = 6,
            div(
              class = "viz-panel",
              style = "background-color:#FFFFFF; border:1px solid #E0E4E8; border-radius:8px; padding:16px 18px 12px 18px; box-shadow:0 1px 3px rgba(0,0,0,0.06); margin-top:16px;",
              h4("Deviation from expected baseline (SD), by region: interactive map", style = "margin-top:0; margin-bottom:2px; font-size:17px;"),
              uiOutput("map_subtitle"),
              leafletOutput("region_map_leaflet", height = "500px"),
              div(
                class = "viz-panel-controls",
                style = "margin-top:10px; padding-top:10px; border-top:1px solid #EEF1F4;",
                sd_gradient_legend_ui(show_no_data = TRUE),
                radioButtons(
                  "case_type_map_leaflet", "Case counts to colour the map by",
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
          selectInput("asof_week_tbl", "As of week", choices = WEEK_CHOICES, selected = LATEST_WEEK_CHOICE),
          numericInput("n_weeks", "Number of recent weeks to show", value = 8, min = 4, max = 20, step = 1),
          radioButtons(
            "case_type_tbl", "Case counts to display",
            choices = c("Reported cases" = "reported", "Projected total cases" = "projected"),
            selected = "reported"
          ),
          tags$p(
            style = "font-size: 12px; color: #555;",
            strong("Projected total cases"), " estimates total cases by dividing the number of reported ",
            "cases by the compliance percentage. For example, 20 reported cases and a compliance of 50% gives ",
            "a projected total case estimate of 40."
          ),
          tags$hr(),
          div(
            style = "font-weight:600; color:var(--who-navy); font-size:13px; margin-bottom:8px;",
            "Cell shading"
          ),
          sd_gradient_legend_ui(show_no_data = FALSE),
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
      "District-level data",
      div(
        style = "padding: 14px 24px;",
        div(
          class = "info-card",
          style = "border-left: 6px solid #F4A81D; background-color: #FEF7E8; margin: 0 0 14px 0; padding: 10px 16px;",
          icon("triangle-exclamation", style = "color:#B0121A;"),
          strong(" Work in progress: "),
          "District-level data extraction is still being refined and may contain gaps or errors. Cross-check figures ",
          "against the source IDSR bulletins (see the ", strong("References"), " tab) before relying on them."
        ),

        # ---- Top banner: Disease / As of week / Case counts to display ----
        div(
          style = "background-color:#F4F5F6; border:1px solid #DDE1E4; border-radius:6px; padding:8px 18px; margin-bottom:10px; display:flex; gap:24px; align-items:flex-end;",
          div(style = "flex: 1 1 0;",
              selectInput("disease_district_tbl", "Disease", choices = district_disease_choices,
                          selected = district_disease_choices[1], width = "100%")),
          div(style = "flex: 1 1 0;",
              selectInput("asof_week_district_tbl", "As of week", choices = WEEK_CHOICES,
                          selected = LATEST_WEEK_CHOICE, width = "100%")),
          div(style = "flex: 1 1 0;",
              radioButtons("case_type_district_tbl", "Case counts to display",
                           choices = c("Reported cases" = "reported", "Projected total cases" = "projected"),
                           selected = "reported", inline = TRUE))
        ),

        # ---- Geographic coverage note (computed once from the data at
        # start-up -- not session-specific, so this is plain UI markup
        # rather than a renderUI/uiOutput) --------------------------------
        div(
          class = "info-disclosure",
          tags$p(
            style = "font-size: 12.5px; color: #555; margin: 0;",
            strong("Geographic coverage: "),
            sprintf(
              "district-level data is currently only reported for %s. No district-level breakdown is available for %s -- these provinces are shown in grey on the map below, and don't appear in the table.",
              paste(provinces_with_district_data, collapse = ", "),
              if (length(provinces_without_district_data) > 0) paste(provinces_without_district_data, collapse = ", ") else "none"
            )
          ),
          tags$p(
            style = "font-size: 12.5px; color: #555; margin: 6px 0 0 0;",
            strong("Projected total cases"), " estimates total cases by dividing the number of reported ",
            "cases by the compliance percentage. For example, 20 reported cases and a compliance of 50% gives ",
            "a projected total case estimate of 40."
          )
        ),

        # ---- Weekly case trend (left) + map (right) -------------------------
        # display:flex on the row stretches both viz-panel cards (which are
        # height:100%) to match the taller one, so the two boxes are always
        # the same size even though the left card carries an extra
        # Province/District selector row that the map card doesn't have.
        fluidRow(
          style = "display:flex; flex-wrap:wrap;",
          column(
            width = 6,
            div(
              class = "viz-panel",
              h4("Weekly case trend"),
              uiOutput("district_trend_subtitle"),
              div(
                style = "display:flex; gap:16px; margin-bottom:10px;",
                div(style = "flex:1;",
                    selectInput("district_curve_province", "Province",
                                choices = provinces_with_district_data,
                                selected = provinces_with_district_data[1], width = "100%")),
                div(style = "flex:1;", uiOutput("district_curve_district_ui"))
              ),
              plotlyOutput("district_trend_plot", height = "360px"),
              div(
                class = "viz-panel-controls",
                style = "min-height: 60px;",
                uiOutput("district_year_selector")
              )
            )
          ),
          column(
            width = 6,
            div(
              class = "viz-panel",
              h4("District map"),
              uiOutput("district_map_subtitle"),
              leafletOutput("district_map", height = "360px"),
              div(
                class = "viz-panel-controls",
                style = "min-height: 60px;",
                sd_gradient_legend_ui(show_no_data = TRUE),
                tags$p(
                  style = "font-size: 12px; color: #555; margin: 8px 0 0 0;",
                  "SD shows how many standard deviations a week's case count is from its expected baseline, ",
                  "using a CUSUM aberration detection method (rolling 9-week baseline, most recent 2 weeks dropped). ",
                  "See the ", strong("Home"), " tab for full details."
                ),
                uiOutput("district_map_boundary_note")
              )
            )
          )
        ),

        # ---- Table, with "recent weeks" + cell shading to its left --------
        # Boxed the same way as the plots above (viz-panel), per user request
        # to give the table and its controls a matching card style.
        div(
          class = "viz-panel",
          style = "margin-top: 16px;",
          fluidRow(
            column(
              width = 3,
              numericInput("n_weeks_district", "Number of recent weeks to show", value = 8, min = 4, max = 20, step = 1),
              tags$hr(),
              div(
                style = "font-weight:600; color:var(--who-navy); font-size:13px; margin-bottom:8px;",
                "Cell shading"
              ),
              sd_gradient_legend_ui(show_no_data = FALSE),
              tags$p(
                style = "font-size: 12px; color: #555; margin-top: 8px;",
                "Standard deviations are based on the CUSUM aberration detection method (rolling 9-week baseline, most recent 2 weeks dropped). See the ", strong("Home"), " tab for details."
              ),
              tags$p(
                style = "font-size: 12px; color: #555; margin-top: 8px;",
                strong(style = "color:inherit;", "NR"), " means NR is explicitly listed on the bulletin's data table for that week. ",
                "A blank cell means the value is either missing from the bulletin's table, or could not be ",
                "read reliably during extraction (e.g. the district's individual values didn't sum to the ",
                "table's printed total, or the table was unusually formatted)."
              )
            ),
            column(
              width = 9,
              p(
                style = "font-size: 13px; color:#555;",
                icon("circle-info"), " Click the arrow on a province row to expand district-level case counts within it."
              ),
              DTOutput("district_table")
            )
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
          style = "max-width: 1000px; margin: 0; padding-left: 24px;",
          tabsetPanel(
            id = "references_tabs",
            type = "tabs",

            tabPanel(
              "Sources & methodology",
              div(
                style = "padding-top: 18px;",
                h4("Primary data source"),
                p("Weekly IDSR bulletins published by Pakistan's National Institute of Health (NIH):"),
                tags$a(href = "https://www.nih.org.pk/phb/weekly-bulletin", target = "_blank",
                       "https://www.nih.org.pk/phb/weekly-bulletin"),
                h4("District-level admin boundaries", style = "margin-top: 20px;"),
                p("The district outlines used on the ", strong("District-level data"), " tab's map come from the ",
                  "UN OCHA Common Operational Datasets (COD) for Pakistan administrative boundaries:"),
                tags$a(href = "https://data.humdata.org/dataset/cod-ab-pak", target = "_blank",
                       "https://data.humdata.org/dataset/cod-ab-pak"),
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
            ),

            tabPanel(
              "Download data",
              div(
                style = "padding-top: 18px; max-width: 640px;",
                h4("Download the underlying data"),
                p(style = "color:#555; font-size: 13.5px;",
                  "These are the raw CSV files this dashboard is built from, exactly as loaded at app start-up."),

                div(
                  style = "border:1px solid #C9DEF3; border-radius:6px; padding:16px; margin-bottom:14px; background-color:#FFFFFF;",
                  h5("IDSR case data", style = "margin-top:0; color:var(--who-navy);"),
                  p(style = "font-size: 12.5px; color:#555; margin-bottom:10px;",
                    "Weekly reported case counts by disease, province, and reporting status."),
                  downloadButton("download_idsr_data", "Download PAK_IDSR_Data.csv", class = "btn-who-download")
                ),

                div(
                  style = "border:1px solid #C9DEF3; border-radius:6px; padding:16px; margin-bottom:14px; background-color:#FFFFFF;",
                  h5("Reporting compliance data", style = "margin-top:0; color:var(--who-navy);"),
                  p(style = "font-size: 12.5px; color:#555; margin-bottom:10px;",
                    "Weekly reporting compliance (%) by region."),
                  downloadButton("download_idsr_compliance", "Download PAK_IDSR_Compliance.csv", class = "btn-who-download")
                ),

                div(
                  style = "border:1px solid #C9DEF3; border-radius:6px; padding:16px; margin-bottom:14px; background-color:#FFFFFF;",
                  h5("District-level IDSR case data", style = "margin-top:0; color:var(--who-navy);"),
                  p(style = "font-size: 12.5px; color:#555; margin-bottom:10px;",
                    "Weekly reported case counts by disease, province, and district."),
                  downloadButton("download_idsr_data_district", "Download PAK_IDSR_Data_District.csv", class = "btn-who-download")
                ),

                div(
                  style = "border:1px solid #C9DEF3; border-radius:6px; padding:16px; background-color:#FFFFFF;",
                  h5("District-level reporting compliance data", style = "margin-top:0; color:var(--who-navy);"),
                  p(style = "font-size: 12.5px; color:#555; margin-bottom:10px;",
                    "Weekly reporting compliance (%) by province and district."),
                  downloadButton("download_idsr_compliance_district", "Download PAK_IDSR_Compliance_District.csv", class = "btn-who-download")
                )
              )
            )
          )
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
  # Shared, non-reactive computation behind both region_change_data() (static
  # ggplot map, driven by the "case_type_map" control) and
  # region_change_data_leaflet() (interactive leaflet map, driven by its own
  # "case_type_map_leaflet" control below) -- the two maps show the same kind
  # of data but can now be coloured by reported/projected cases independently
  # of one another, since they live in separate panels.
  compute_region_change_data <- function(dis, asof, metric) {
    locs <- location_choices[location_choices != "National"]
    region_list <- lapply(locs, function(loc) {
      s <- get_trend_data(dis, loc)
      if (nrow(s) == 0) return(data.frame(region = loc, z = NA_real_, status = "absent"))
      stats <- compute_cusum_stats_at(s, asof$year, asof$week, value_col = metric)
      data.frame(
        region = loc,
        z = if (stats$status == "ok") stats$z else NA_real_,
        status = stats$status
      )
    })
    bind_rows(region_list)
  }

  region_change_data <- reactive({
    req(input$disease, input$asof_week_viz)
    asof <- parse_asof(input$asof_week_viz)
    metric <- if (identical(input$case_type_map, "projected")) "Projected" else "Reported"
    compute_region_change_data(input$disease, asof, metric)
  })

  region_change_data_leaflet <- reactive({
    req(input$disease, input$asof_week_viz)
    asof <- parse_asof(input$asof_week_viz)
    metric <- if (identical(input$case_type_map_leaflet, "projected")) "Projected" else "Reported"
    compute_region_change_data(input$disease, asof, metric)
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
    sf_map$z_capped <- pmin(pmax(sf_map$z, -SD_SCALE_LIMIT), SD_SCALE_LIMIT)

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
        low = SD_SCALE_LOW, mid = SD_SCALE_MID, high = SD_SCALE_HIGH, midpoint = 0,
        limits = c(-SD_SCALE_LIMIT, SD_SCALE_LIMIT), na.value = "#B7BCC2", name = "SD from\nbaseline",
        labels = function(x) paste0(x, " SD")
      ) +
      ggrepel::geom_text_repel(
        data = label_df, aes(x = x, y = y, label = label),
        size = 3.3, color = who_navy, fontface = "bold", 
        segment.color = who_navy, segment.size = 0.3, min.segment.length=0.05,force_pull=10, force=0.01,
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

  # ---- Same province-level SD-from-baseline data as region_map above, --
  # but rendered on an interactive leaflet map (province admin1
  # boundaries, same source as the District-level data map -- see the
  # References tab) instead of the static ggplot one. Province names are
  # shown as permanent on-map labels via addLabelOnlyMarkers(noHide =
  # TRUE) rather than a hover tooltip, so the SD reading for every
  # province is visible at once without having to mouse over each one.
  output$region_map_leaflet <- renderLeaflet({
    validate(need(!is.null(pak_district_admin1_sf), "Map unavailable: district boundary file could not be read."))

    rc <- region_change_data_leaflet()
    sf_map <- pak_district_admin1_sf %>% left_join(rc, by = c("province_code" = "region"))

    matched_ok <- !is.na(sf_map$status) & sf_map$status == "ok"
    fill_col <- rep(DISTRICT_NO_DATA_COLOUR, nrow(sf_map))
    fill_col[matched_ok] <- sd_continuous_colour(sf_map$z[matched_ok])

    status_line <- ifelse(
      is.na(sf_map$status), "No data available",
      ifelse(sf_map$status == "ok", paste0(round(sf_map$z, 1), " SD from baseline"),
      ifelse(sf_map$status == "no_report", "No report this week",
      ifelse(sf_map$status == "absent", "Not in this week's bulletin",
             "Insufficient baseline data")))
    )
    label_html <- lapply(
      paste0("<strong>", sf_map$adm1_name, "</strong><br>", status_line),
      htmltools::HTML
    )

    # Label points guaranteed to sit inside each polygon (unlike a plain
    # centroid, which can fall outside oddly-shaped or multi-part
    # regions) -- same technique as region_map's ggrepel labels above.
    label_pts <- suppressWarnings(sf::st_point_on_surface(sf_map))
    coords <- sf::st_coordinates(label_pts)

    # ggrepel only works on static ggplot2 output, not an interactive
    # leaflet htmlwidget -- declutter_label_offsets() (defined near the
    # top of this file) is a small hand-rolled stand-in that nudges any
    # overlapping province labels apart in pixel space, applied per
    # marker below via labelOptions(offset = ...).
    offsets <- declutter_label_offsets(coords[, 1], coords[, 2], sf_map$adm1_name, status_line, zoom = 5)

    m <- leaflet(sf_map, options = leafletOptions(minZoom = 4, maxZoom = 9)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = fill_col, fillOpacity = 0.85,
        color = "#6B7280", weight = 0.8, opacity = 0.8,
        highlightOptions = highlightOptions(weight = 2.5, color = who_navy, bringToFront = TRUE)
      )

    # One addLabelOnlyMarkers() call per province rather than one
    # vectorised call for all of them, since each needs its OWN
    # declutter offset -- labelOptions() is a single shared options
    # object per call, so a varying per-marker offset needs a separate
    # call per marker (only 7 provinces, so this is cheap).
    for (i in seq_len(nrow(sf_map))) {
      m <- m %>% addLabelOnlyMarkers(
        lng = coords[i, 1], lat = coords[i, 2],
        label = label_html[[i]],
        labelOptions = labelOptions(
          noHide = TRUE, direction = "center", textOnly = TRUE,
          offset = c(offsets[i, 1], offsets[i, 2]),
          style = list(
            "font-weight" = "600", "font-size" = "12px", color = who_navy, "text-align" = "center",
            "text-shadow" = "-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff"
          )
        )
      )
    }

    m %>% setView(lng = 69.5, lat = 30.3, zoom = 5)
  })

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
    req(input$location_tbl, input$n_weeks, input$asof_week_tbl)
    series <- get_all_disease_series(input$location_tbl)
    validate(need(nrow(series) > 0, "No data available for this selection."))

    asof      <- parse_asof(input$asof_week_tbl)
    weeks_cal <- weeks_up_to(asof, input$n_weeks)
    n_wk      <- nrow(weeks_cal)
    week_cols <- weeks_cal$week_lab
    diseases_here <- sort(unique(series$Disease))

    value_col <- if (identical(input$case_type_tbl, "projected")) "Projected" else "Reported"

    # Joined against weeks_cal (rather than filtered on a single Year) so the
    # window is correct even when it crosses a year boundary -- see
    # weeks_up_to()'s comment above.
    values_wide <- series %>%
      inner_join(weeks_cal, by = c("Year", "Week")) %>%
      select(Disease, week_lab, Value = all_of(value_col)) %>%
      complete(Disease = diseases_here, week_lab = week_cols, fill = list(Value = NA_real_)) %>%
      select(Disease, week_lab, Value)

    values_wide_wide <- values_wide %>% pivot_wider(names_from = week_lab, values_from = Value)
    values_wide_wide <- values_wide_wide[, c("Disease", week_cols)]

    sd_matrix <- sapply(seq_len(nrow(weeks_cal)), function(i) {
      yy <- weeks_cal$Year[i]; ww <- weeks_cal$Week[i]
      sapply(diseases_here, function(dis) {
        s <- series %>% filter(Disease == dis)
        cusum_z_at(s, yy, ww, value_col = value_col)
      })
    })
    sd_wide <- as.data.frame(sd_matrix)
    colnames(sd_wide) <- paste0(week_cols, "_sd")

    # One TRUE/FALSE per disease/week: was this specifically a genuine NR
    # (Not Reported) week, as opposed to just no row at all (a disease not
    # in that week's bulletin, which is not shown as NR -- see IsNR's
    # definition in get_all_disease_series() above).
    nr_matrix <- sapply(seq_len(nrow(weeks_cal)), function(i) {
      yy <- weeks_cal$Year[i]; ww <- weeks_cal$Week[i]
      sapply(diseases_here, function(dis) {
        r <- series[series$Disease == dis & series$Year == yy & series$Week == ww, ]
        if (nrow(r) == 0) FALSE else isTRUE(r$IsNR[1])
      })
    })
    nr_wide <- as.data.frame(nr_matrix)
    colnames(nr_wide) <- paste0(week_cols, "_nr")

    list(
      display_values = values_wide_wide,
      sd = sd_wide,
      nr = nr_wide,
      week_cols = week_cols,
      year = asof$year, week = asof$week, n_wk = n_wk, diseases = diseases_here, weeks_cal = weeks_cal,
      value_col = value_col
    )
  })

  output$weekly_table <- renderDT({
    tbl <- weekly_table_reactive()

    display_df <- cbind(tbl$display_values, tbl$sd, tbl$nr)
    sd_col_names <- paste0(tbl$week_cols, "_sd")
    nr_col_names <- paste0(tbl$week_cols, "_nr")
    hidden_idx <- which(names(display_df) %in% c(sd_col_names, nr_col_names)) - 1

    label <- if (tbl$value_col == "Projected") "projected total" else "reported"

    # A custom render per week column (rather than DT::formatRound()) so a
    # genuine NR week can show the text "NR" instead of a number -- each
    # column's render only reads ITS OWN hidden "_nr" companion column, via
    # the full raw row array DataTables passes as the 3rd render argument.
    week_column_defs <- lapply(tbl$week_cols, function(wc) {
      col_idx <- which(names(display_df) == wc) - 1
      nr_idx  <- which(names(display_df) == paste0(wc, "_nr")) - 1
      list(
        targets = col_idx,
        render = JS(sprintf(
          "function(data, type, row) {
             if (type !== 'display') return data;
             if (row[%d]) return '<span style=\"color:#888; font-style:italic;\">NR</span>';
             if (data === null) return '';
             return Math.round(data).toLocaleString();
           }", nr_idx
        ))
      )
    })

    dt <- datatable(
      display_df,
      rownames = FALSE,
      escape = FALSE,
      selection = "none",   # no built-in row click-select (and its blue highlight) -- this table has no use for it
      options = list(
        pageLength = 25,
        columnDefs = c(list(list(visible = FALSE, targets = hidden_idx)), week_column_defs)
      ),
      caption = paste0(
        "Weekly ", label, " cases by disease, ", input$location_tbl,
        ", ", tbl$n_wk, " week(s) up to and including Week ", tbl$week, ", ", tbl$year
      )
    )

    dt <- formatStyle(
      dt,
      columns = tbl$week_cols,
      valueColumns = sd_col_names,
      backgroundColor = sd_continuous_bg_js(),
      color = sd_continuous_font_js()
    )
    dt
  })

  # ---------------- District-level data tab -------------------------------
  # A single flat table: one row per province (bold, always visible) is
  # immediately followed by one row per district within it (indented,
  # hidden until that province's arrow is clicked). Because province and
  # district rows are literal rows of the SAME table -- not a separate
  # nested table -- the week columns always line up exactly with the
  # header, and the usual formatStyle() SD-shading pipeline (identical to
  # the Weekly summary table) applies to every row uniformly. Each week
  # column also gets a custom render (see output$district_table below) so a
  # genuine NR week displays as "NR" text rather than a blank/zero.

  district_table_reactive <- reactive({
    req(input$disease_district_tbl, input$n_weeks_district, input$asof_week_district_tbl)

    district_series <- get_district_disease_series(input$disease_district_tbl)
    validate(need(nrow(district_series) > 0, "No district-level data available for this disease."))

    province_series <- get_province_disease_series(district_series, input$disease_district_tbl)

    asof      <- parse_asof(input$asof_week_district_tbl)
    weeks_cal <- weeks_up_to(asof, input$n_weeks_district)
    n_wk      <- nrow(weeks_cal)
    week_cols <- weeks_cal$week_lab
    sd_cols   <- paste0(week_cols, "_sd")
    nr_cols   <- paste0(week_cols, "_nr")

    value_col <- if (identical(input$case_type_district_tbl, "projected")) "Projected" else "Reported"

    provinces_here <- sort(unique(district_series$Province))

    # Pulls the week_cols/sd_cols/nr_cols values for one (sub-)series -- a
    # single province's or district's own Year/Week rows -- as a one-row
    # data frame, in the exact column order/names used throughout this table.
    row_for_series <- function(s, label) {
      vals <- sapply(seq_len(nrow(weeks_cal)), function(i) {
        r <- s[s$Year == weeks_cal$Year[i] & s$Week == weeks_cal$Week[i], ]
        if (nrow(r) == 0) NA_real_ else r[[value_col]][1]
      })
      sds <- sapply(seq_len(nrow(weeks_cal)), function(i) {
        cusum_z_at(s, weeks_cal$Year[i], weeks_cal$Week[i], value_col = value_col)
      })
      nrs <- sapply(seq_len(nrow(weeks_cal)), function(i) {
        r <- s[s$Year == weeks_cal$Year[i] & s$Week == weeks_cal$Week[i], ]
        if (nrow(r) == 0) FALSE else isTRUE(r$IsNR[1])
      })
      # check.names = FALSE: week_cols/sd_cols/nr_cols contain spaces (e.g.
      # "Wk 24") which as.data.frame() would otherwise silently mangle into
      # "Wk.24", breaking every downstream reference to these column names.
      row <- as.data.frame(c(
        list(Toggle = "", Location = label),
        setNames(as.list(vals), week_cols),
        setNames(as.list(sds), sd_cols),
        setNames(as.list(nrs), nr_cols)
      ), stringsAsFactors = FALSE, check.names = FALSE)
      row
    }

    rows <- list()
    for (p in provinces_here) {
      s_prov <- province_series %>% filter(Province == p) %>% arrange(Year, Week)
      prov_row <- row_for_series(s_prov, p)
      prov_row$RowType <- "province"
      prov_row$Group   <- p
      rows[[length(rows) + 1]] <- prov_row

      districts_here <- sort(unique(district_series$District[district_series$Province == p]))
      for (dis in districts_here) {
        s_dist <- district_series %>% filter(Province == p, District == dis) %>% arrange(Year, Week)
        dist_row <- row_for_series(s_dist, dis)
        dist_row$RowType <- "district"
        dist_row$Group   <- p
        rows[[length(rows) + 1]] <- dist_row
      }
    }

    display_df <- bind_rows(rows)

    list(
      display_df = display_df,
      week_cols = week_cols,
      sd_cols = sd_cols,
      nr_cols = nr_cols,
      year = asof$year, week = asof$week, n_wk = n_wk,
      value_col = value_col,
      disease = input$disease_district_tbl
    )
  })

  output$district_table <- renderDT({
    tbl <- district_table_reactive()
    df  <- tbl$display_df
    week_cols <- tbl$week_cols
    sd_cols   <- tbl$sd_cols
    nr_cols   <- tbl$nr_cols

    toggle_idx   <- which(names(df) == "Toggle")   - 1
    location_idx <- which(names(df) == "Location") - 1
    sd_idx       <- which(names(df) %in% sd_cols)  - 1
    nr_idx       <- which(names(df) %in% nr_cols)  - 1
    rowtype_idx  <- which(names(df) == "RowType")  - 1
    group_idx    <- which(names(df) == "Group")    - 1

    label <- if (tbl$value_col == "Projected") "projected total" else "reported"

    # A custom render per week column (rather than DT::formatRound()) so a
    # genuine NR week can show the text "NR" instead of a number -- each
    # column's render only reads ITS OWN hidden "_nr" companion column, via
    # the full raw row array DataTables passes as the 3rd render argument.
    # Same technique as the Weekly summary table's output$weekly_table above.
    week_column_defs <- lapply(week_cols, function(wc) {
      col_idx <- which(names(df) == wc) - 1
      wc_nr_idx <- which(names(df) == paste0(wc, "_nr")) - 1
      list(
        targets = col_idx,
        render = JS(sprintf(
          "function(data, type, row) {
             if (type !== 'display') return data;
             if (row[%d]) return '<span style=\"color:#888; font-style:italic;\">NR</span>';
             if (data === null) return '';
             return Math.round(data).toLocaleString();
           }", wc_nr_idx
        ))
      )
    })

    dt <- datatable(
      df,
      rownames = FALSE,
      escape = FALSE,
      selection = "none",   # no built-in row click-select (and its blue highlight) -- expand/collapse uses its own click handler below, independent of DT's selection
      colnames = c(" " = "Toggle", "Province / District" = "Location"),
      options = list(
        paging = FALSE, ordering = FALSE, searching = FALSE, info = FALSE, dom = "t",
        columnDefs = c(
          list(
            list(
              targets = toggle_idx, className = "details-control", width = "18px",
              render = JS(sprintf(
                "function(data, type, row) { return row[%d] === 'province' ? '&#9654;' : ''; }", rowtype_idx
              ))
            ),
            list(
              targets = location_idx,
              render = JS(sprintf(
                "function(data, type, row) {
                   if (type !== 'display') return data;
                   return row[%d] === 'district'
                     ? '<span style=\"padding-left:26px; color:#333;\">' + data + '</span>'
                     : '<strong>' + data + '</strong>';
                 }", rowtype_idx
              ))
            ),
            list(visible = FALSE, targets = c(sd_idx, nr_idx, rowtype_idx, group_idx))
          ),
          week_column_defs
        ),
        # Hide every district row up front, and re-hide it on any redraw
        # (createdRow only fires once per row here since paging is off, but
        # this keeps behaviour correct if that ever changes).
        createdRow = JS(sprintf(
          "function(row, data, dataIndex) {
             if (data[%d] === 'district') { $(row).hide(); }
             $(row).attr('data-group', data[%d]);
           }", rowtype_idx, group_idx
        ))
      ),
      callback = JS(
        "table.column(0).nodes().to$().css({cursor: 'pointer'});",
        "table.on('click', 'td.details-control', function() {",
        "  var tr  = $(this).closest('tr');",
        "  var row = table.row(tr);",
        sprintf("  if (row.data()[%d] !== 'province') return;", rowtype_idx),
        sprintf("  var grp = row.data()[%d];", group_idx),
        "  var $children = $(table.table().body()).find('tr[data-group=\"' + grp + '\"]').not(tr);",
        "  var willShow = !$children.first().is(':visible');",
        "  $children.toggle(willShow);",
        "  $(this).html(willShow ? '&#9660;' : '&#9654;');",
        "});"
      ),
      caption = paste0(
        "District-level weekly ", label, " cases of ", tbl$disease, " by province, ",
        tbl$n_wk, " week(s) up to and including Week ", tbl$week, ", ", tbl$year,
        ". Click the arrow on a province row to expand its districts."
      )
    )

    dt <- formatStyle(
      dt,
      columns = week_cols,
      valueColumns = sd_cols,
      backgroundColor = sd_continuous_bg_js(),
      color = sd_continuous_font_js()
    )
    dt
  })

  # ---------------- District-level data tab: epi curve --------------------
  # Cascading Province -> District dropdowns. District choices depend on
  # which province is selected, so this is a renderUI (rather than a fixed
  # selectInput in the UI above) populated from whichever districts actually
  # have data for that province; an "All districts" option shows the
  # province's own total (the same total used for its row in the table).
  output$district_curve_district_ui <- renderUI({
    req(input$district_curve_province)
    dists <- sort(unique(raw_district_data$District[raw_district_data$Province == input$district_curve_province]))
    choices <- c("All districts (province total)" = "__ALL__", setNames(dists, dists))
    selectInput("district_curve_district", "District", choices = choices, selected = "__ALL__", width = "100%")
  })

  # Only 2025 and 2026 are offered here (the Data visualisation tab's own
  # "Years to show" selector still offers every year in the data) -- fixed
  # rather than derived from the data, per the brief.
  output$district_year_selector <- renderUI({
    checkboxGroupInput("district_years_selected", "Years to show",
                        choices = c(2026, 2025), selected = c(2026, 2025), inline = TRUE)
  })

  district_curve_data_all <- reactive({
    req(input$disease_district_tbl, input$district_curve_province)
    ds <- get_district_disease_series(input$disease_district_tbl) %>%
      filter(Province == input$district_curve_province)
    sel_dist <- input$district_curve_district %||% "__ALL__"
    if (identical(sel_dist, "__ALL__")) {
      get_province_disease_series(ds, input$disease_district_tbl) %>%
        filter(Province == input$district_curve_province) %>%
        mutate(Compliance = NA_real_) %>%
        select(Year, Week, Reported, Projected, Compliance)
    } else {
      ds %>%
        filter(District == sel_dist) %>%
        select(Year, Week, Reported, Projected, Compliance)
    }
  })

  output$district_trend_subtitle <- renderUI({
    req(input$disease_district_tbl, input$district_curve_province)
    sel_dist <- input$district_curve_district %||% "__ALL__"
    loc_label <- if (identical(sel_dist, "__ALL__")) {
      paste0(input$district_curve_province, " (province total)")
    } else {
      paste0(sel_dist, ", ", input$district_curve_province)
    }
    div(class = "viz-subtitle", paste0("Disease: ", input$disease_district_tbl, ", Location: ", loc_label))
  })

  output$district_trend_plot <- renderPlotly({
    req(input$asof_week_district_tbl)
    d <- district_curve_data_all()
    validate(need(nrow(d) > 0, "No data available for this selection."))

    years_selected <- if (is.null(input$district_years_selected)) c(2025, 2026) else as.integer(input$district_years_selected)
    d <- d %>% filter(Year %in% years_selected)
    validate(need(nrow(d) > 0, "No years selected."))

    asof <- parse_asof(input$asof_week_district_tbl)
    d <- d %>% filter(Year < asof$year | (Year == asof$year & Week <= asof$week))
    validate(need(nrow(d) > 0, "No data available up to the selected week."))

    value_col <- if (identical(input$case_type_district_tbl, "projected")) "Projected" else "Reported"
    label_txt <- if (value_col == "Projected") "Projected total cases" else "Reported cases"
    cols <- YEAR_COLOR_MAP[as.character(sort(unique(d$Year)))]

    d <- d %>%
      arrange(Year, Week) %>%
      group_by(Year) %>%
      mutate(Cases = .data[[value_col]],
             YTD = cumsum(ifelse(is.na(Cases), 0, Cases))) %>%
      ungroup()

    d <- d %>% mutate(
      tooltip = paste0("Year: ", Year, "<br>Week: ", Week,
                        "<br>", label_txt, ": ", comma(round(Cases)),
                        "<br>YTD ", tolower(label_txt), ": ", comma(round(YTD)),
                        ifelse(is.na(Compliance), "", paste0("<br>Reporting compliance: ", round(Compliance), "%")))
    )
    d <- d %>% filter(!is.na(Cases))
    validate(need(nrow(d) > 0, "No data available for this combination of filters."))

    # Insert explicit NA rows for any missing week (within each year's own
    # range) so the line breaks across a gap rather than joining it up --
    # same convention as the Data visualisation tab's epi curve.
    d <- d %>%
      group_by(Year) %>%
      complete(Week = full_seq(Week, 1)) %>%
      ungroup() %>%
      mutate(Year_f = factor(Year, levels = sort(unique(Year))))

    week_breaks <- seq(floor(min(d$Week, na.rm = TRUE)), ceiling(max(d$Week, na.rm = TRUE)), by = 1)
    week_labels <- ifelse(week_breaks %% 5 == 0, as.character(week_breaks), "")

    p <- ggplot(d, aes(x = Week, y = Cases, group = Year_f, color = Year_f, text = tooltip)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.4, na.rm = TRUE) +
      scale_color_manual(values = cols, name = "Year") +
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

    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", y = -0.3, title = list(text = "")),
        margin = list(b = 70),
        hoverlabel = list(bgcolor = "#FFFFFF", font = list(color = who_navy, family = "Source Sans Pro")),
        xaxis = list(
          tickmode = "array", tickvals = week_breaks, ticktext = week_labels,
          ticks = "outside", ticklen = 4, tickwidth = 1, tickcolor = "#8A8F94",
          showgrid = FALSE
        )
      ) %>%
      config(displaylogo = FALSE)
  })

  # ---------------- District-level data tab: map ---------------------------
  district_map_data <- reactive({
    req(input$disease_district_tbl, input$asof_week_district_tbl, input$case_type_district_tbl)
    asof <- parse_asof(input$asof_week_district_tbl)
    value_col <- if (identical(input$case_type_district_tbl, "projected")) "Projected" else "Reported"
    get_district_map_data(input$disease_district_tbl, asof, value_col)
  })

  output$district_map_subtitle <- renderUI({
    req(input$disease_district_tbl, input$asof_week_district_tbl)
    asof <- parse_asof(input$asof_week_district_tbl)
    div(class = "viz-subtitle",
        paste0("Disease: ", input$disease_district_tbl, ", as of Week ", asof$week, ", ", asof$year))
  })

  # ---- Dynamic boundary-adjustment note, below the District map -----------
  # Lists only the specific adjustments actually relevant to the disease/week
  # currently on screen -- i.e. only a concordance (see
  # DISTRICT_MAP_CONCORDANCES_TO_NARRATE above) or an unmatched district that
  # genuinely has data somewhere in the displayed week or its 9-week CUSUM
  # baseline, not the full static list of every adjustment that exists
  # anywhere in the data regardless of whether it's relevant right now.
  district_map_boundary_note_reactive <- reactive({
    req(input$disease_district_tbl, input$asof_week_district_tbl)
    if (is.null(pak_district_admin2_sf)) return(NULL)

    asof      <- parse_asof(input$asof_week_district_tbl)
    weeks_cal <- weeks_up_to(asof, 9)   # the displayed week + its 9-week CUSUM baseline

    district_series <- get_district_disease_series(input$disease_district_tbl)
    in_window <- district_series %>% semi_join(weeks_cal, by = c("Year", "Week"))
    districts_with_data <- sort(unique(in_window$District))
    if (length(districts_with_data) == 0) return(NULL)

    boundary_keys <- pak_district_admin2_sf$dist_key

    concordance_names <- vapply(DISTRICT_MAP_CONCORDANCES_TO_NARRATE, `[[`, character(1), "district")
    concordance_items <- vapply(DISTRICT_MAP_CONCORDANCES_TO_NARRATE, function(cc) {
      if (cc$district %in% districts_with_data) {
        sprintf("disease counts for %s have been included in the %s District", cc$district, cc$included_in)
      } else NA_character_
    }, character(1))
    concordance_items <- concordance_items[!is.na(concordance_items)]

    # Any OTHER district with data in this window that has no boundary
    # polygon at all -- excluding the ones already named above via the
    # explicit concordance list, so the same district is never listed twice
    # under two different phrasings.
    other_districts    <- setdiff(districts_with_data, concordance_names)
    unmatched_districts <- other_districts[!(resolve_dist_key(other_districts) %in% boundary_keys)]
    unmatched_items <- if (length(unmatched_districts) > 0) {
      sprintf("no district match found for %s", paste(unmatched_districts, collapse = ", "))
    } else character(0)

    items <- c(concordance_items, unmatched_items)
    if (length(items) == 0) return(NULL)
    # First item capitalised (it starts the sentence); the rest already
    # lower-case as written above, joined into one flowing sentence.
    items[1] <- paste0(toupper(substr(items[1], 1, 1)), substr(items[1], 2, nchar(items[1])))
    items
  })

  output$district_map_boundary_note <- renderUI({
    items <- district_map_boundary_note_reactive()
    if (is.null(items)) return(NULL)
    tags$p(
      style = "font-size: 12px; color: #555; margin: 8px 0 0 0;",
      "In some cases, the districts on the bulletin do not align with the mapped administrative boundaries. ",
      "For this map, the following adjustments have been made: ",
      paste0(paste(items, collapse = ", "), "."),
      tags$br(), tags$br(),
      "For a merged district, the map's projected total and compliance-adjusted figures are calculated by ",
      "summing the underlying reported case counts and projected totals of the districts combined into it, ",
      "rather than by averaging their individual compliance percentages."
    )
  })

  output$district_map <- renderLeaflet({
    validate(need(!is.null(pak_district_admin2_sf), "Map unavailable: district boundary file could not be read."))

    md <- district_map_data()
    sf_map <- pak_district_admin2_sf %>%
      left_join(md, by = c("province_code" = "Province", "dist_key" = "dist_key"))

    covered      <- sf_map$province_code %in% provinces_with_district_data
    has_match    <- covered & !is.na(sf_map$status)
    matched_ok   <- has_match & sf_map$status == "ok"

    fill_col <- rep(DISTRICT_PROVINCE_NOT_COVERED_COLOUR, nrow(sf_map))
    fill_col[covered] <- DISTRICT_NO_DATA_COLOUR
    fill_col[matched_ok] <- sd_continuous_colour(sf_map$z[matched_ok])

    status_txt <- ifelse(
      !covered, "No district-level data currently reported for this province.",
      ifelse(!has_match, "No matching district-level data for this district.",
      ifelse(sf_map$status == "ok", paste0(round(sf_map$z, 1), " SD from baseline"),
      ifelse(sf_map$status == "no_report", "No report this week",
      ifelse(sf_map$status == "absent", "Not in this week's bulletin",
             "Insufficient baseline data")))))

    cases_txt <- ifelse(
      has_match & !is.na(sf_map$Reported),
      paste0(
        "<br>Reported cases: ", comma(round(sf_map$Reported)),
        ifelse(is.na(sf_map$Projected), "",
               paste0("<br>Projected total cases: ", comma(round(sf_map$Projected))))
      ),
      ""
    )

    tooltip <- paste0(
      "<strong>", sf_map$adm2_name, "</strong> (", sf_map$adm1_name, ")",
      cases_txt, "<br>", status_txt
    )

    m <- leaflet(sf_map, options = leafletOptions(minZoom = 4, maxZoom = 9)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = fill_col, fillOpacity = 0.85,
        color = "#6B7280", weight = 0.5, opacity = 0.8,
        label = lapply(tooltip, htmltools::HTML),
        labelOptions = labelOptions(direction = "auto", textsize = "12px"),
        highlightOptions = highlightOptions(weight = 2.5, color = who_navy, bringToFront = TRUE)
      ) %>%
      setView(lng = 69.5, lat = 30.3, zoom = 5)

    if (!is.null(pak_district_admin1_sf)) {
      m <- m %>% addPolylines(data = pak_district_admin1_sf, color = who_navy, weight = 1.3, opacity = 0.7)
    }
    m
  })

  # ---------------- Alerts tab (single list, grouped by disease) ---------
  alerts_reactive <- reactive({
    req(input$asof_week_alerts)
    value_col <- if (identical(input$case_type_alerts, "projected")) "Projected" else "Reported"
    asof <- parse_asof(input$asof_week_alerts)
    compute_alerts_long(value_col = value_col, sel_year = asof$year, sel_week = asof$week)
  })

  # Fired by an alert's "Data Visualisation" link (see output$alerts_output
  # below) -- a plain JS onclick posts {disease, location} via
  # Shiny.setInputValue() with priority "event" (rather than binding a
  # separate Shiny input per alert row, which would need dynamically
  # (re)registered observers every time the alert list changes). Jumps to
  # the Data visualisation tab with that disease/region already selected.
  observeEvent(input$alerts_goto, {
    req(input$alerts_goto$disease, input$alerts_goto$location)
    updateSelectInput(session, "disease", selected = input$alerts_goto$disease)
    updateSelectInput(session, "location", selected = input$alerts_goto$location)
    updateNavbarPage(session, "who_nav", selected = "Data visualisation")
  })

  output$alerts_output <- renderUI({
    result <- alerts_reactive()
    alerts  <- result$alerts
    missing <- result$missing
    label     <- if (identical(input$case_type_alerts, "projected")) "projected total" else "reported"
    value_col <- if (identical(input$case_type_alerts, "projected")) "Projected" else "Reported"
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

        # Single row per disease (as before), but now with one dropdown
        # for the whole disease -- expanding it reveals a single table
        # with one row per alerted region, each showing that region's
        # week-by-week case counts (shaded with the same SD colour scale
        # used elsewhere) and, in that region's own label cell, a link
        # over to the Data visualisation tab already filtered to this
        # disease/region. Purely client-side to expand/collapse (a plain
        # div visibility toggle, since the detail content is already
        # fully rendered server-side); only the "Data visualisation"
        # link needs a round-trip to the server (it has to change other
        # tabs' inputs).
        row_id <- paste0("alert_detail_", gsub("[^A-Za-z0-9]+", "_", dis))

        div(
          class = paste("qv-box", level_class[as.character(max_lvl)]),
          tags$div(
            class = "alert-row-header",
            style = "cursor:pointer; display:flex; align-items:center; gap:6px;",
            onclick = sprintf(
              "var el=document.getElementById('%s'); el.style.display=(el.style.display==='none'||el.style.display==='')?'block':'none';",
              row_id
            ),
            icon("caret-down", style = "font-size:11px; color:#888;"),
            span(strong(dis), ": ", paste(loc_text, collapse = ", "))
          ),
          tags$div(
            id = row_id,
            style = "display:none; margin:8px 0 4px 20px;",
            build_alert_region_table(dis, as.character(d$Location), asof, value_col, n_weeks = 12)
          )
        )
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

  # ---------------- References tab: raw data downloads --------------------
  # Serve the original CSV files as-is (not the filtered/cleaned in-memory
  # versions), so what's downloaded matches exactly what ships in Data/.

  output$download_idsr_data <- downloadHandler(
    filename = function() basename(DATA_PATH),
    content = function(file) file.copy(DATA_PATH, file)
  )

  output$download_idsr_compliance <- downloadHandler(
    filename = function() basename(COMPLIANCE_PATH),
    content = function(file) file.copy(COMPLIANCE_PATH, file)
  )

  output$download_idsr_data_district <- downloadHandler(
    filename = function() basename(DISTRICT_DATA_PATH),
    content = function(file) file.copy(DISTRICT_DATA_PATH, file)
  )

  output$download_idsr_compliance_district <- downloadHandler(
    filename = function() basename(DISTRICT_COMPLIANCE_PATH),
    content = function(file) file.copy(DISTRICT_COMPLIANCE_PATH, file)
  )
}

shinyApp(ui, server)
