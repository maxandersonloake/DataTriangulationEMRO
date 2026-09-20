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
#   - install.packages(c("readxl", "dplyr", "tidyr", "openssl", "stringdist"))
#     if you don't already have them (readxl and stringdist in particular
#     are NOT among the app's own runtime dependencies, so they won't
#     already be in renv.lock -- that's fine, this script never runs on
#     Posit Connect Cloud). stringdist powers the fuzzy State/Region/
#     District name matching below, mirroring Pakistan's own
#     1_1_DownloadData_PAK_v4.R ingestion pipeline.
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
if (!requireNamespace("stringdist", quietly = TRUE)) {
  stop("Install stringdist first: install.packages('stringdist')")
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

# Title-Case helper for State/Region/District -- the raw workbook mixes ALL
# CAPS and Title Case for what's very often the exact same place (e.g.
# "BADHAN" and "Badhan" both appear as separate rows). Normalising casing
# at ingestion, before ANY grouping happens below, means every downstream
# group_by() (duplicate-row collapse, district reporting span, District x
# Disease x Week grid, State rollup) automatically treats these as the same
# place instead of silently fragmenting one real State/Region/District's
# case counts across two "different" ones. This only fixes case/whitespace
# variants -- genuine alternate SPELLINGS of the same place are handled
# separately, by the fuzzy-matching system below.
.title_case <- function(x) {
  x <- tolower(trimws(x))
  gsub("(?:^|(?<=[\\s'-]))([a-z])", "\\U\\1", x, perl = TRUE)
}

# ================================================================
# State / Region / District canonical-name lookups + fuzzy matching
# ----------------------------------------------------------------
# Ports the region_lookup / district_region_lookup + build_matcher() /
# fuzzy_match() / is_subsequence() system from Pakistan's own
# 1_1_DownloadData_PAK_v4.R ingestion pipeline. Rather than a short manual
# alias table covering only the handful of misspellings someone happened
# to notice, every place name we know about lives in one of the three
# lookups below as a canonical spelling plus a list of known variant
# spellings the raw workbook has actually used for it; anything not in
# that variant list still gets a chance to match via the same fuzzy
# distance/subsequence tiers Pakistan's pipeline uses, so a fresh, not-yet-
# catalogued misspelling in a future workbook update still has a good
# chance of being consolidated automatically instead of silently creating
# a brand-new "district".
#
# The 139 canonical District entries below were derived from the raw
# workbook's 171 case/whitespace-normalised District spellings via a
# systematic pairwise edit-distance scan (distance <= 2) across all of
# them, manually triaged one pair at a time against domain knowledge (e.g.
# same-state proximity alone was NOT treated as sufficient evidence --
# "Barawe" and "Bardale" are both real, distinct South West towns despite
# being edit-distance 2 apart in the same state; cross-state look-alikes
# such as "Hudun"/"Hudur" or "Gardo"/"Bahdo" were left as separate
# districts, since a place name resembling another one in a DIFFERENT
# state is much more likely to be two different real places than a
# transcription slip). Nest-by-State is used only to organise this list
# for editing and to derive district_to_state_som below -- matching itself
# is still done against the full flattened District variant list, exactly
# like Pakistan's own district_region_lookup.
# ================================================================

# ---- Generic helpers (ported from 1_1_DownloadData_PAK_v4.R) ------------

# TRUE if every character of `needle` appears in `haystack`, in the same
# order (not necessarily contiguously). Used only as fuzzy_match()'s
# last-resort tier below.
is_subsequence <- function(needle, haystack) {
  if (nchar(needle) == 0) return(TRUE)
  hp <- 1L
  hn <- nchar(haystack)
  for (i in seq_len(nchar(needle))) {
    ch <- substr(needle, i, i)
    found <- FALSE
    while (hp <= hn) {
      if (substr(haystack, hp, hp) == ch) { found <- TRUE; hp <- hp + 1L; break }
      hp <- hp + 1L
    }
    if (!found) return(FALSE)
  }
  TRUE
}

# Flattens a list(canonical = c(variant1, variant2, ...)) lookup into a
# flat named character vector variant -> canonical (lower-cased,
# whitespace-trimmed on both sides, canonical spelling itself always
# included as a variant of itself) -- the form fuzzy_match() expects.
build_matcher <- function(lookup) {
  out <- character(0)
  for (canonical in names(lookup)) {
    variants <- unique(c(tolower(trimws(canonical)), tolower(trimws(lookup[[canonical]]))))
    for (v in variants) out[[v]] <- canonical
  }
  out
}

# 4-tier match of a raw string against a variant_map (as built by
# build_matcher()):
#   1. Exact match (case-insensitive/trimmed).
#   2. Unambiguous prefix match (raw string is >= 4 chars and an
#      unambiguous startsWith() match against exactly one canonical name's
#      variants) -- stop()s if the prefix is ambiguous, since that means
#      the variant list needs a manual disambiguating entry, not a guess.
#   3. Edit-distance match (stringdist's OSA method), tolerance scaled by
#      input length (<=4 chars: 0, 5-7 chars: 1, >=8 chars: 2, capped at
#      max_dist) -- stop()s on an ambiguous tie, same reasoning as above.
#   4. Subsequence match (last resort): only considered when the
#      candidate's no-space length is within 0-3 characters of the input's,
#      and the input is at least half the candidate's length (to avoid a
#      short fragment matching almost everything); an ambiguous subsequence
#      match is NOT an error -- it message()s and returns NA_character_,
#      leaving the row to fall back to its own raw spelling rather than
#      guessing between two real places.
# Returns NA_character_ if nothing matches at any tier.
fuzzy_match <- function(raw, variant_map, max_dist = 2) {
  if (is.na(raw) || !nzchar(trimws(raw))) return(NA_character_)
  x <- tolower(trimws(raw))

  # 1. Exact
  if (x %in% names(variant_map)) return(unname(variant_map[[x]]))

  # 2. Unambiguous prefix
  if (nchar(x) >= 4) {
    hits <- variant_map[startsWith(names(variant_map), x)]
    if (length(hits) > 0) {
      canon <- unique(unname(hits))
      if (length(canon) == 1) return(canon)
      stop("Ambiguous prefix match for '", raw, "': could be ", paste(canon, collapse = " / "))
    }
  }

  # 3. Edit distance (OSA), tolerance scaled by input length
  tol <- if (nchar(x) <= 4) 0L else if (nchar(x) <= 7) 1L else 2L
  tol <- min(tol, max_dist)
  if (tol > 0) {
    dists <- stringdist::stringdist(x, names(variant_map), method = "osa")
    hits <- names(variant_map)[dists <= tol]
    if (length(hits) > 0) {
      canon <- unique(unname(variant_map[hits]))
      if (length(canon) == 1) return(canon)
      stop("Ambiguous fuzzy match for '", raw, "': could be ", paste(canon, collapse = " / "))
    }
  }

  # 4. Subsequence match (last resort)
  x_nospace <- gsub("\\s", "", x)
  hits <- character(0)
  for (cand in names(variant_map)) {
    cand_nospace <- gsub("\\s", "", cand)
    len_diff <- nchar(cand_nospace) - nchar(x_nospace)
    if (len_diff >= 0 && len_diff <= 3 &&
        nchar(x_nospace) >= nchar(cand_nospace) / 2 &&
        is_subsequence(x_nospace, cand_nospace)) {
      hits <- c(hits, cand)
    }
  }
  if (length(hits) > 0) {
    canon <- unique(unname(variant_map[hits]))
    if (length(canon) == 1) return(canon)
    message("Ambiguous subsequence match for '", raw, "': could be ",
            paste(canon, collapse = " / "), " -- leaving unmatched.")
    return(NA_character_)
  }

  NA_character_
}

# ---- Base State lookup ---------------------------------------------------
SOM_STATE_LOOKUP <- list(
  "Banadir"      = c("banadir"),
  "Galmudug"     = c("galmudug"),
  "Hir-Shabelle" = c("hir-shabelle", "hirshabelle", "hir shabelle"),
  "Jubaland"     = c("jubaland"),
  "North East"   = c("north east", "northeast", "north-east"),
  "Puntland"     = c("puntland"),
  "Somaliland"   = c("somaliland"),
  "South West"   = c("south west", "southwest", "south-west")
)

# ---- Base Region lookup ---------------------------------------------------
# Canonical spelling matched against the official boundary file's
# adm1_name (Data/som_admin_boundaries/som_admin1.geojson) where one of the
# 18 exists; a handful of raw Region values are local/historical
# sub-region names with no single corresponding official ADM1 region
# (Karkaar, Ras-Asayr, Sahil, South Mudug, Ayn) and are kept as their own
# distinct canonical entries rather than forced onto a boundary region
# they don't actually match. "Marodi Jeh" is Somaliland's own name for the
# region the old national system (and the boundary file) calls "Woqooyi
# Galbeed" -- kept as the workbook's own name (same convention as the
# District lookup below), with "Woqooyi Galbeed" listed as a variant in
# case a future workbook switches to it. Region isn't currently surfaced
# anywhere in the dashboard UI, but is still worth canonicalising here: it
# feeds district_span/district_full_grid's group_by() below alongside
# State and District, so an inconsistently-spelled Region for the same
# real district could otherwise silently fragment that district's own
# reporting-span calculation.
SOM_REGION_LOOKUP <- list(
  "Awdal"           = c("awdal"),
  "Ayn"             = c("ayn", "cayn"),
  "Bakool"          = c("bakool", "bakol"),
  "Banadir"         = c("banadir"),
  "Bari"            = c("bari"),
  "Bay"             = c("bay"),
  "Galgaduud"       = c("galgaduud", "galgadud"),
  "Gedo"            = c("gedo"),
  "Hiraan"          = c("hiraan", "hiran"),
  "Karkaar"         = c("karkaar", "karkar"),
  "Lower Juba"      = c("lower juba"),
  "Lower Shabelle"  = c("lower shabelle"),
  "Marodi Jeh"      = c("marodi jeh", "maroodi jeex", "woqooyi galbeed"),
  "Middle Shabelle" = c("middle shabelle"),
  "Mudug"           = c("mudug"),
  "Nugaal"          = c("nugaal", "nugal"),
  "Ras-Asayr"       = c("ras-asayr", "ras asayr"),
  "Sahil"           = c("sahil"),
  "Sanaag"          = c("sanaag", "sanaq"),
  "Sool"            = c("sool"),
  "South Mudug"     = c("south mudug"),
  "Togdheer"        = c("togdheer", "togdher")
)

# ---- Base District lookup, nested by State --------------------------------
SOM_DISTRICT_STATE_LOOKUP <- list(
  Banadir = list(
    "Abdul Aziz" = c("abdul aziz"),
    "Bondere" = c("bondere", "bondheere"),
    "Danyile" = c("danyile", "deynile"),
    "Darusalam" = c("darusalam"),
    "Dharkeynley" = c("dharkenly", "dharkeynley"),
    "Garasbaley" = c("garasbaaley", "garasbaley", "gubadleey"),
    "Gubadley" = c("gubadley"),
    "Hamar Jabjab" = c("hamar jabjab"),
    "Hamar Weyn" = c("hamar wayne", "hamar weyn"),
    "Hawal Wadag" = c("hawal wadag"),
    "Heliwa" = c("heliwa", "heliwaa"),
    "Hodan" = c("hodan"),
    "Kahda" = c("kahda"),
    "Karan" = c("karan"),
    "Madina" = c("madina"),
    "Shangani" = c("shangani"),
    "Shibis" = c("shibis"),
    "Waberi" = c("waberi"),
    "Wadajir" = c("wadajir"),
    "Wardegly" = c("wardegly"),
    "Warta Nabada" = c("warta nabada"),
    "Yaqshid" = c("yaqshid")
  ),
  Galmudug = list(
    "Abudwaq" = c("abudwak", "abudwaq"),
    "Adado" = c("adado"),
    "Afbarwaqo" = c("afbarwaqo"),
    "Bahdo" = c("bahdo"),
    "Balanbale" = c("balanbale"),
    "Bandiridley" = c("bandiridley"),
    "Celgaras" = c("celgaras"),
    "Dhabad" = c("dhabad"),
    "Dhuusamarreb" = c("dhuusamarreb"),
    "Dusamreb" = c("dusamreb"),
    "El Bur" = c("el bur"),
    "El Dhere" = c("el dhere", "eldheer"),
    "Elgula" = c("elgula"),
    "Gadoon" = c("gadoon"),
    "Galcad" = c("galcad"),
    "Galinsoor" = c("galinsoor", "galinsor"),
    "Galkacyo" = c("galkacyo"),
    "Galkayu South" = c("galkayu south"),
    "Godinlabe" = c("godinlabe"),
    "Guriel" = c("guriel"),
    "Harardheere" = c("haradhere", "harardheere"),
    "Heraale" = c("heraale"),
    "Herodhagley" = c("herodhagahley", "herodhagley"),
    "Hobyo" = c("hobyo"),
    "Wisil" = c("wisil")
  ),
  `Hir-Shabelle` = list(
    "Adale" = c("adale"),
    "Aden Yabal" = c("aden yabal"),
    "Balad" = c("balad"),
    "Belet Weyne" = c("belet weyne"),
    "Buloburte" = c("bulo burti", "buloburte"),
    "Jalalaqsi" = c("jalalaqsi"),
    "Jowhar" = c("jowhar"),
    "Mahaday" = c("mahaday"),
    "Mahas" = c("mahas"),
    "Mataban" = c("mataban"),
    "Raaga Celle" = c("raaga celle"),
    "Runingod" = c("runingod"),
    "Warsheikh" = c("warsheikh")
  ),
  Jubaland = list(
    "Afmadow" = c("afmadow"),
    "Badhadhe" = c("badhadhe"),
    "Bardera" = c("bardera"),
    "Beled Hawo" = c("beled hawo"),
    "Belet Hawa" = c("belet hawa"),
    "Buurdhubo" = c("burdubo", "buurdhubo"),
    "Ceel Waaq" = c("ceel waaq"),
    "Dhobley" = c("dhobely", "dhobley"),
    "Dolow" = c("dolo", "dolow"),
    "El Wak" = c("el wak"),
    "Garbaharey" = c("garbaharey"),
    "Jamame" = c("jamame"),
    "Kismayo" = c("kismayo"),
    "Luuq" = c("luuq"),
    "Xagar" = c("hagar", "xagar")
  ),
  `North East` = list(
    "Las Anod" = c("laasaanod", "las anod", "lasanod")
  ),
  Puntland = list(
    "Alula" = c("alula"),
    "Badhan" = c("badhan"),
    "Bender Bayla" = c("bender bayla", "benderbayla"),
    "Bossaso" = c("bossaso"),
    "Buhodle" = c("buhodle"),
    "Burtinle" = c("burtinle"),
    "Carmo" = c("carmo"),
    "Dangoroyo" = c("dangoroyo"),
    "Dhahar" = c("dahar", "dhahar"),
    "Eyl" = c("eyl"),
    "Galkaio" = c("galkaio"),
    "Galkayu North" = c("galkayu north"),
    "Gardo" = c("gardo"),
    "Garowe" = c("garowe"),
    "Goldogob" = c("goldogob"),
    "Horufadhi" = c("horufadhi"),
    "Jariiban" = c("jariban", "jariiban"),
    "Qardho" = c("qardho"),
    "Taleh" = c("taleh"),
    "Ufeyn" = c("ufain", "ufeyn"),
    "Waaciya" = c("waaciya", "waciya"),
    "Widhwidh" = c("widhwidh"),
    "Xingalool" = c("xingalol", "xingalool")
  ),
  Somaliland = list(
    "Aynabo" = c("ainabo", "aynabo"),
    "Baki" = c("baki"),
    "Baligubadle" = c("baligubadle"),
    "Berbera" = c("berbera"),
    "Borama" = c("borama"),
    "Burco" = c("burco", "buroa"),
    "Buuhoodle" = c("buuhoodle"),
    "El Afweyn" = c("el afweyn", "el-afweyn"),
    "Erigavo" = c("erigavo"),
    "Gabiley" = c("gabiley"),
    "Garadag" = c("garadag"),
    "Hargeisa" = c("hargeisa"),
    "Hudun" = c("hudun"),
    "Las-Qoray" = c("las-qoray", "las-qoreh"),
    "Lughaya" = c("lughaya"),
    "Odwayne" = c("odwayne", "odweine"),
    "Sheikh" = c("sheikh"),
    "Taleeh" = c("taleeh"),
    "Zeila" = c("zeila")
  ),
  `South West` = list(
    "Afgoi" = c("afgoi"),
    "Afgooye" = c("afgooye"),
    "Awdhegle" = c("awdhegle"),
    "Baidoa" = c("baidoa", "baidoba"),
    "Barawe" = c("barawe"),
    "Bardale" = c("bardale"),
    "Berdalle" = c("berdalle"),
    "Brava" = c("brava"),
    "Burhakaba" = c("burhakaba"),
    "Diinsor" = c("diinsor", "dinsor"),
    "El Barde" = c("el barde", "el-barde"),
    "Hudur" = c("hudur"),
    "Kurtunwaarey" = c("kurtunwaarey", "kurtunwarey"),
    "Marka" = c("marka"),
    "Qansaxdhere" = c("qansahdhere", "qansaxdhere"),
    "Qoryoley" = c("qoryoley", "qoryoolay"),
    "Quracjome" = c("quracjome"),
    "Rabdhure" = c("rabdhure", "rabdure"),
    "Tiyeglow" = c("tiyeglo", "tiyeglow"),
    "Wajid" = c("wajid"),
    "Wanla Weyn" = c("wanla weyn")
  )
)

# ---- Flatten the nested District lookup into the flat forms matching
# needs: district_lookup_som (canonical -> variants, ungrouped by State,
# mirroring Pakistan's own flat district_lookup) and district_to_state_som
# (canonical District -> its State, mirroring Pakistan's
# district_to_region), then build the variant -> canonical matchers for
# all three levels.
district_lookup_som <- list()
district_to_state_som <- character(0)
for (state_name in names(SOM_DISTRICT_STATE_LOOKUP)) {
  state_districts <- SOM_DISTRICT_STATE_LOOKUP[[state_name]]
  for (canonical_district in names(state_districts)) {
    district_lookup_som[[canonical_district]] <- state_districts[[canonical_district]]
    district_to_state_som[[canonical_district]] <- state_name
  }
}

state_variant_map        <- build_matcher(SOM_STATE_LOOKUP)
region_variant_map_som    <- build_matcher(SOM_REGION_LOOKUP)
district_variant_map_som  <- build_matcher(district_lookup_som)

match_state_name       <- function(x) fuzzy_match(x, state_variant_map, max_dist = 2)
match_region_name_som  <- function(x) fuzzy_match(x, region_variant_map_som, max_dist = 2)
match_district_name_som <- function(x) fuzzy_match(x, district_variant_map_som, max_dist = 2)

# ---- Apply the matchers at ingestion, BEFORE any grouping below, so case
# counts consolidate under one canonical State/Region/District rather than
# fragmenting across spelling variants. Mirrors Pakistan's own
# match-then-coalesce-to-raw pattern exactly: a value with no fuzzy match
# at any tier (fuzzy_match() returns NA) falls back to its own
# title-cased raw spelling rather than being dropped or erroring, so a
# genuinely new/unrecognised place name still makes it through as its own
# (uncorrected) entry instead of silently disappearing from the data.
raw <- raw %>%
  mutate(
    # State was previously left as-is (only trimws()'d) -- the workbook has
    # it ALL CAPS for 7 of the 8 states ("JUBALAND", "PUNTLAND", ...) but
    # already Title Case for the 8th ("North East"), so State needed the
    # same .title_case() treatment as Region/District to read consistently.
    State_raw    = .title_case(as.character(State)),
    Region_raw   = .title_case(as.character(Region)),
    District_raw = .title_case(as.character(District)),
    State    = vapply(State_raw, match_state_name, character(1)),
    Region   = vapply(Region_raw, match_region_name_som, character(1)),
    District = vapply(District_raw, match_district_name_som, character(1)),
    State    = coalesce(State, State_raw),
    Region   = coalesce(Region, Region_raw),
    District = coalesce(District, District_raw),
    Week     = as.integer(Week),
    Year     = as.integer(Year)
  ) %>%
  filter(!is.na(State), !is.na(District), !is.na(Week), !is.na(Year))

# ---- Report anything that fell back to its own raw spelling -------------
# A value counts as "unmatched" only if it stayed equal to its own raw
# (title-cased) spelling AND that raw spelling isn't itself already one of
# our canonical names -- i.e. genuinely nothing in the corresponding
# lookup matched it, at any tier. Visibility without blocking: these rows
# are NOT dropped, they just kept their own spelling, so a fresh
# misspelling (or a genuinely new place) is easy to spot in the console
# output of a future run and, if it's a duplicate spelling, added to the
# relevant lookup above.
unmatched_states <- raw %>%
  filter(State == State_raw, !(State %in% names(SOM_STATE_LOOKUP))) %>%
  distinct(State_raw) %>% pull(State_raw)
if (length(unmatched_states) > 0) {
  warning("Somalia State values with no fuzzy match (kept as their own raw spelling): ",
          paste(unmatched_states, collapse = ", "))
}

unmatched_regions <- raw %>%
  filter(Region == Region_raw, !(Region %in% names(SOM_REGION_LOOKUP))) %>%
  distinct(Region_raw) %>% pull(Region_raw)
if (length(unmatched_regions) > 0) {
  warning("Somalia Region values with no fuzzy match (kept as their own raw spelling): ",
          paste(unmatched_regions, collapse = ", "))
}

unmatched_districts <- raw %>%
  filter(District == District_raw, !(District %in% names(district_lookup_som))) %>%
  distinct(District_raw) %>% pull(District_raw)
if (length(unmatched_districts) > 0) {
  warning("Somalia District values with no fuzzy match (kept as their own raw spelling): ",
          paste(unmatched_districts, collapse = ", "))
}

raw <- raw %>% select(-State_raw, -Region_raw, -District_raw)

# The full set of District spellings we're left with after the above --
# i.e. the "base" district list this run of the pipeline is actually
# working with (a subset of the full SOM_DISTRICT_STATE_LOOKUP catalogue
# above if this particular workbook doesn't have data for every known
# district, plus any genuinely unmatched raw spellings reported above).
# Printed for visibility, the same way the disease list below is.
SOM_BASE_DISTRICTS <- sort(unique(raw$District))
cat("Base districts after case/spelling clean-up + fuzzy matching (", length(SOM_BASE_DISTRICTS), "):\n  ",
    paste(SOM_BASE_DISTRICTS, collapse = ", "), "\n", sep = "")

# ---- Identify the disease case AND death columns ------------------------
# The workbook pairs each disease with its own "<Disease> case"/"<Disease>
# death" columns (with two exceptions -- "Neonatal Tetanus" and
# "Dengue Fever " have no " case" suffix/have trailing whitespace). Deaths
# are exposed to the dashboard as their own selectable pseudo-disease --
# "<Disease> (Deaths)" -- alongside the case-count "<Disease>" entry,
# rather than as a separate value type next to Reported/Projected. This
# means every existing chart/table/map, which all key off a generic
# Disease + Cases pair, works for deaths with no further changes: picking
# "Malaria (Deaths)" in any disease dropdown just flows the death counts
# through exactly the same Cases column and codepath as any other disease.
all_cols     <- names(raw)
meta_cols    <- c("State", "Region", "District", "Week", "Month", "Year")
disease_cols <- setdiff(all_cols, meta_cols)
case_cols    <- disease_cols[!grepl("death\\s*$", disease_cols, ignore.case = TRUE)]
death_cols   <- disease_cols[grepl("death\\s*$", disease_cols, ignore.case = TRUE)]

.clean_disease_name <- function(col) {
  nm <- trimws(col)
  nm <- sub("\\s*[Cc]ase\\s*$", "", nm)
  nm <- trimws(nm)
  # Capitalise only the first character; leave the rest as-is (several
  # names contain intentional casing/parentheses, e.g. "(ILI)").
  paste0(toupper(substr(nm, 1, 1)), substr(nm, 2, nchar(nm)))
}
disease_name_map <- setNames(vapply(case_cols, .clean_disease_name, character(1)), case_cols)

# Pair each death column up with its own case column by a shared "base"
# key (both sides' " case"/" death" suffix stripped, lower-cased) rather
# than cleaning each death column's name independently -- the raw
# workbook's death columns don't always match their case column's casing
# exactly (e.g. "Neonatal Tetanus" vs "Neonatal tetanus death"), so pairing
# by key and labelling from the CASE side keeps "<Disease> (Deaths)"
# consistently capitalised with its own "<Disease>" case entry. A death
# column with no matching case column (or vice versa) is left out, with a
# console note, rather than guessed at.
.base_key <- function(col) {
  nm <- trimws(col)
  nm <- sub("\\s*[Cc]ase\\s*$", "", nm)
  nm <- sub("\\s*[Dd]eath\\s*$", "", nm)
  tolower(trimws(nm))
}
case_base_keys  <- setNames(vapply(case_cols, .base_key, character(1)), case_cols)
death_base_keys <- setNames(vapply(death_cols, .base_key, character(1)), death_cols)

death_name_map <- character(0)
for (dcol in death_cols) {
  matched_case_col <- names(case_base_keys)[case_base_keys == death_base_keys[[dcol]]]
  if (length(matched_case_col) == 1) {
    death_name_map[dcol] <- paste0(disease_name_map[[matched_case_col]], " (Deaths)")
  } else {
    cat("Note: death column '", dcol, "' has no matching case column -- left out.\n", sep = "")
  }
}
unmatched_death_cols <- setdiff(death_cols, names(death_name_map))
if (length(unmatched_death_cols) > 0) death_cols <- setdiff(death_cols, unmatched_death_cols)

all_value_cols   <- c(case_cols, death_cols)
all_name_map     <- c(disease_name_map, death_name_map)

cat("Diseases tracked (cases):\n  ", paste(unname(disease_name_map), collapse = ", "), "\n")
cat("Diseases tracked (deaths):\n  ", paste(unname(death_name_map), collapse = ", "), "\n")

# ---- Long format at DISTRICT/WEEK/DISEASE grain ------------------------
long_raw <- raw %>%
  select(State, Region, District, Year, Week, all_of(all_value_cols)) %>%
  pivot_longer(cols = all_of(all_value_cols), names_to = "SrcCol", values_to = "Cases") %>%
  mutate(Disease = all_name_map[SrcCol], Cases = suppressWarnings(as.numeric(Cases))) %>%
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
diseases <- unname(all_name_map)
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
