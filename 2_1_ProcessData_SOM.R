# ================================================================
# Somalia IDSR data pipeline.
# ----------------------------------------------------------------
# Reads the Somalia IDSR case-count workbook and reshapes it into the same
# long-format schema the dashboard already expects from the Pakistan
# pipeline (raw_data / compliance_data / raw_district_data /
# district_compliance_data -- see app.R's own header comment), then
# encrypts the result into Data/SOM_IDSR_Data.enc so it can be committed to
# the (public) GitHub repo without the underlying case data being readable
# there. app.R decrypts this file at startup using the SAME
# SOMALIA_DATA_KEY passphrase, supplied on Posit Connect Cloud as a secret
# environment variable (never committed).
#
# Run this LOCALLY, by hand, whenever you have an updated Somalia
# workbook -- it is NOT part of app.R's own startup (unlike Pakistan's
# pipeline, this isn't wired up to a scheduled GitHub Action, since the
# source file isn't a public download). After running it:
#   1. `git add Data/SOM_IDSR_Data.enc` (this is the only new file that
#      needs to be committed -- never commit the raw workbook itself).
#   2. Push/redeploy as normal.
#
# Before running:
#   - install.packages(c("readxl", "dplyr", "tidyr", "openssl")) if you
#     don't already have them (readxl in particular is NOT one of the
#     app's own runtime dependencies, so it won't already be in renv.lock
#     -- that's fine, this script never runs on Posit Connect Cloud).
#   - Put the raw workbook somewhere OUTSIDE the repo, or inside the
#     Somalia_Raw/ folder (already listed in .gitignore so it's never
#     accidentally committed even if you drop it there), and point
#     SOM_RAW_XLSX at it below.
#   - Set the SOMALIA_DATA_KEY environment variable (e.g. in your
#     ~/.Renviron) to whatever passphrase you've also set as a Posit
#     Connect Cloud secret for this app. Anyone without that exact
#     passphrase cannot decrypt Data/SOM_IDSR_Data.enc, even with full
#     read access to the public repo.
# ================================================================

library(dplyr)
library(tidyr)
if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("Install readxl first: install.packages('readxl')")
}
source("encryption_utils.R")

SOM_RAW_XLSX  <- "Somalia_Raw/IDSR_data.xlsx"
SOM_OUT_PATH  <- "Data/SOM_IDSR_Data.enc"
SOM_SHEET     <- "IDSR data"

passphrase <- Sys.getenv("SOMALIA_DATA_KEY")
if (!nzchar(passphrase)) {
  stop("SOMALIA_DATA_KEY is not set. Set it (e.g. in .Renviron) before running this script -- ",
       "it must match the SOMALIA_DATA_KEY secret configured on Posit Connect Cloud.")
}

cat("Reading", SOM_RAW_XLSX, "...\n")
raw <- readxl::read_excel(SOM_RAW_XLSX, sheet = SOM_SHEET)

raw <- raw %>%
  mutate(
    State    = trimws(as.character(State)),
    Region   = trimws(as.character(Region)),
    District = trimws(as.character(District)),
    Week     = as.integer(Week),
    Year     = as.integer(Year)
  ) %>%
  filter(!is.na(State), !is.na(District), !is.na(Week), !is.na(Year))

# ---- Identify the disease case columns -------------------------------
# The workbook pairs each disease with its own "<Disease> case"/"<Disease>
# death" columns (with two exceptions -- "Neonatal Tetanus" and
# "Dengue Fever " have no " case" suffix/have trailing whitespace). This
# dashboard only tracks case counts (matching Pakistan's own schema, which
# has no separate death series) -- so every column NOT ending in "death"
# (after trimming) is treated as a case-count column.
all_cols     <- names(raw)
meta_cols    <- c("State", "Region", "District", "Week", "Month", "Year")
disease_cols <- setdiff(all_cols, meta_cols)
case_cols    <- disease_cols[!grepl("death\\s*$", disease_cols, ignore.case = TRUE)]

.clean_disease_name <- function(col) {
  nm <- trimws(col)
  nm <- sub("\\s*[Cc]ase\\s*$", "", nm)
  nm <- trimws(nm)
  # Capitalise only the first character; leave the rest as-is (several
  # names contain intentional casing/parentheses, e.g. "(ILI)").
  paste0(toupper(substr(nm, 1, 1)), substr(nm, 2, nchar(nm)))
}
disease_name_map <- setNames(vapply(case_cols, .clean_disease_name, character(1)), case_cols)
cat("Diseases tracked (cases only; death columns dropped):\n  ",
    paste(unname(disease_name_map), collapse = ", "), "\n")

# ---- Long format at DISTRICT/WEEK/DISEASE grain ------------------------
long_raw <- raw %>%
  select(State, Region, District, Year, Week, all_of(case_cols)) %>%
  pivot_longer(cols = all_of(case_cols), names_to = "SrcCol", values_to = "Cases") %>%
  mutate(Disease = disease_name_map[SrcCol], Cases = suppressWarnings(as.numeric(Cases))) %>%
  select(State, Region, District, Disease, Year, Week, Cases)

# ---- Collapse duplicate District/Week submissions -----------------------
# A small number of District/Year/Week combinations have more than one
# source row (two facility-level submissions rolled into the same
# district/week, never explained by a further column in the workbook).
# Per instruction: take the MAX of the duplicates for each disease
# independently, rather than summing them (summing risked double-counting
# if the duplicates are actually the same submission entered twice).
n_before <- long_raw %>% distinct(State, Region, District, Year, Week) %>% nrow()
dup_pairs <- raw %>% count(State, Region, District, Year, Week) %>% filter(n > 1) %>% nrow()
cat("Duplicate District/Year/Week combinations collapsed via max():", dup_pairs, "\n")

long_raw <- long_raw %>%
  group_by(State, Region, District, Disease, Year, Week) %>%
  summarise(Cases = if (all(is.na(Cases))) NA_real_ else max(Cases, na.rm = TRUE), .groups = "drop")

# ---- Which District/Year/Week rows were actually submitted -------------
# A row's mere presence (regardless of which disease cells it filled in)
# is what "this district reported something that week" means here -- there
# is no separate reporting-status column in the source workbook.
submitted_weeks <- raw %>% distinct(State, Region, District, Year, Week)

# ---- Continuous week index, so "gap" can mean "between two submitted
# weeks", not just "any missing Year/Week combo" (which would wrongly
# flag every week before a district joined the reporting system, or every
# week after the data extract ends, as non-reporting). ---------------------
all_years <- sort(unique(raw$Year))
full_calendar <- expand.grid(Year = all_years, Week = 1:52) %>%
  arrange(Year, Week) %>%
  mutate(week_idx = row_number())

submitted_weeks <- submitted_weeks %>%
  left_join(full_calendar, by = c("Year", "Week"))

district_span <- submitted_weeks %>%
  group_by(State, Region, District) %>%
  summarise(min_idx = min(week_idx), max_idx = max(week_idx), .groups = "drop")

# Every (district, week) pair within that district's OWN observed
# reporting span (its first submitted week through its last), whether or
# not a row actually exists there.
district_full_grid <- district_span %>%
  rowwise() %>%
  reframe(
    State = State, Region = Region, District = District,
    week_idx = seq(min_idx, max_idx)
  ) %>%
  left_join(full_calendar, by = "week_idx") %>%
  select(State, Region, District, Year, Week)

cat("District/week combinations within each district's own reporting span:", nrow(district_full_grid), "\n")
cat("...of which actually submitted:", nrow(submitted_weeks), "\n")
cat("...i.e. implied non-reporting gaps:", nrow(district_full_grid) - nrow(submitted_weeks), "\n")

# ---- Build the full District x Disease x Week grid, with Status --------
diseases <- unname(disease_name_map)
district_disease_grid <- district_full_grid %>%
  tidyr::crossing(Disease = diseases)

raw_district_data <- district_disease_grid %>%
  left_join(long_raw, by = c("State", "Region", "District", "Disease", "Year", "Week")) %>%
  left_join(
    submitted_weeks %>% mutate(row_submitted = TRUE) %>% select(State, Region, District, Year, Week, row_submitted),
    by = c("State", "Region", "District", "Year", "Week")
  ) %>%
  mutate(
    row_submitted = ifelse(is.na(row_submitted), FALSE, row_submitted),
    # A disease is "reported" only if its own row was submitted AND that
    # disease's cell in it was non-blank. Everything else (row never
    # submitted at all, or submitted but this disease's cell was blank) is
    # NR -- matching how the dashboard already treats Pakistan's district
    # pipeline (Status %in% c("NR","Missing") both collapse to "no report").
    Status = ifelse(row_submitted & !is.na(Cases), "reported", "NR"),
    Cases  = ifelse(Status == "reported", Cases, NA_real_)
  ) %>%
  select(Province = State, Region, District, Disease, Cases, Status, Week, Year) %>%
  arrange(Province, Region, District, Disease, Year, Week)

cat("raw_district_data rows:", nrow(raw_district_data), "\n")

# ---- Roll up District -> State ("Province") -----------------------------
# A State/Disease/Week row only exists where at least one of its districts
# has a row in raw_district_data for that week (i.e. is within ITS OWN
# reporting span) -- a State with zero active districts that week gets no
# row at all here, rather than being marked NR, since there's no basis for
# claiming non-reporting before any district in it had joined the system.
raw_data_state <- raw_district_data %>%
  group_by(Province, Disease, Year, Week) %>%
  summarise(
    Cases  = if (all(is.na(Cases))) NA_real_ else sum(Cases, na.rm = TRUE),
    Status = if (all(Status == "NR")) "NR" else "reported",
    .groups = "drop"
  )

# ---- National "Total" rollup --------------------------------------------
raw_data_national <- raw_data_state %>%
  group_by(Disease, Year, Week) %>%
  summarise(
    Cases  = if (all(is.na(Cases))) NA_real_ else sum(Cases, na.rm = TRUE),
    Status = if (all(Status == "NR")) "NR" else "reported",
    .groups = "drop"
  ) %>%
  mutate(Province = "Total")

raw_data <- bind_rows(raw_data_state, raw_data_national) %>%
  select(Disease, Province, Cases, Status, Week, Year) %>%
  arrange(Province, Disease, Year, Week)

cat("raw_data rows (State + Total):", nrow(raw_data), "\n")
cat("States:", paste(sort(unique(raw_data_state$Province)), collapse = ", "), "\n")

# ---- Compliance data: none yet -------------------------------------------
# Empty but correctly-shaped, so every downstream join against it (which
# already tolerates missing/NA compliance everywhere) resolves to
# Compliance = NA rather than erroring -- the "Reported / Projected total
# cases" toggle is already wired up dashboard-wide and will start working
# for Somalia the moment a real compliance file replaces this empty one.
# Written already in the FINAL shape app.R expects (Region, Week, Year,
# Compliance) -- unlike Pakistan's compliance_data, which app.R derives
# from a raw "Expected/Received Reports" CSV via its own mutate() step,
# this bundle is loaded directly, so there's no separate derivation step
# for it to go through.
compliance_data <- tibble(
  Region = character(0), Week = integer(0), Year = integer(0), Compliance = numeric(0)
)
district_compliance_data <- tibble(
  Province = character(0), District = character(0),
  Week = integer(0), Year = integer(0), Compliance = numeric(0)
)

# ---- Encrypt + write ------------------------------------------------------
bundle <- list(
  raw_data                  = raw_data,
  compliance_data           = compliance_data,
  raw_district_data         = raw_district_data,
  district_compliance_data  = district_compliance_data,
  built_at                  = Sys.time()
)

dir.create("Data", showWarnings = FALSE)
encrypt_object_to_file(bundle, SOM_OUT_PATH, passphrase)
cat("Wrote encrypted bundle to", SOM_OUT_PATH, "(", file.size(SOM_OUT_PATH), "bytes )\n")
cat("Remember: `git add", SOM_OUT_PATH, "` and commit -- never commit", SOM_RAW_XLSX, "itself.\n")
