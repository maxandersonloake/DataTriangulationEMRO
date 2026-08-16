# Perform NIH Pakistan Weekly Bulletin extraction
#   To Do:
#    - better identification of table of interest (rather than relying on it being labelled table 1)
#    - email mandersonloake@gmail.com with any unexpected outputs (or if output is good)
#    - fix up dates matching each week (currently assumes week 1 = 01-01 to 01-07)
#    - add data validation step

# Questions:
#    - Rubella same as rubella (crs)?
#    - Diphtheria (probable) same as diphtheria?
#    - Punjab delayed reporting
#    - Week on x axis or date?
#    - Compliance table ideas
#    - Interpretation of none reported (leave off plot? - currently have as 0s on table)
#    - 30% increase colouring?


# TO DO:
#Meeting 8/4/2026 - Dashboard Review:

# Where to use % vs CUSUM method
# Map with neighbouring countries as well?
# Include greens in weekly summary
# - Alerts national and regional split?
#   - How to handle missing data punjab
# - Other next steps
# 
# - Add other countries neighbouring in map
# - Need 3+ weeks with available data to calculate mean and sd, otherwise flag
# - If missing data in line plot, remove line between and swap to scatter plot
# - Make it clear which week the data is showing (most recent week)
# - Add description about compliance calculation on visualisation page
# - Differentiate between late data and NR
# - Week in alert tab
# - Functionality to change map and alerts pages to previous week or other week
# - Stacked bar for how the number of cases in each district is contributing to total
# - Disclaimer: Not replacing PHI. Watermark. 
# - Alerted last 3 weeks, or X times this year


library(xml2)
library(rvest)
library(stringr)
library(dplyr)
library(purrr)
library(pdftools)
library(stringdist)
library(tidyr)
library(readr)
library(fs)

# ---- Lookups ----------------------------------------------------------

# Some PDF fonts drop the "ti" ligature glyph entirely during text
# extraction (a poppler/pdftools quirk with certain embedded font subsets),
# either removing it outright or leaving a single space in its place --
# e.g. "National" extracts as "Na onal", "Baltistan" as "Bal stan". Which
# words are affected seems to depend on which font subset that block of
# text happens to use, so rather than loosening fuzzy-match distance
# tolerances everywhere (risking confusion between genuinely different
# short terms elsewhere), any lookup term containing "ti" gets defensive
# extra variants added so a broken extraction still resolves via an
# exact/near-exact match.
add_ligature_variants <- function(lookup){
  lapply(lookup, function(variants){
    extra <- unlist(lapply(variants, function(v){
      if(!grepl("ti", v, fixed = TRUE)) return(character(0))
      c(
        str_squish(gsub("ti", " ", v, fixed = TRUE)),  # ligature dropped, space left behind
        gsub("ti", "", v, fixed = TRUE)                # ligature dropped entirely
      )
    }))
    unique(c(variants, extra))
  })
}

# Flexible lookup in case column names change
# (Capitalisation does not matter)
column_lookup <- list(
  Disease      = c("disease", "diseases"),
  AJK          = c("ajk", "azad jammu kashmir", "azad jammu & kashmir"),
  Balochistan  = c("balochistan"),
  GB           = c("gb", "gilgit baltistan", "gilgit-baltistan"),
  ICT          = c("ict", "islamabad", "islamabad capital territory"),
  KP           = c("kp", "kpk", "khyber pakhtunkhwa"),
  Punjab       = c("punjab"),
  Sindh        = c("sindh"),
  Total        = c("total", "totals", "grand total")
)
column_lookup <- column_lookup |> lapply(tolower) |> add_ligature_variants()

# Flexible lookup in case disease names change spelling/punctuatuion
disease_lookup <- list(
  "AD (Non-Cholera)"        = c("ad (non-cholera)", "ad non cholera", "acute diarrhea (non-cholera)",
                                "acute diarrhoea (non-cholera)", "ad non-cholera"),
  "Malaria"                 = c("malaria"),
  "ILI"                     = c("ili", "influenza like illness", "influenza-like illness"),
  "TB"                      = c("tb", "tuberculosis"),
  "ALRI < 5 years"          = c("alri < 5 years", "alri under 5", "alri<5",
                                "acute lower respiratory infection < 5 years", "alri (<5 years)"),
  "Animal / Dog Bite"       = c("animal / dog bite", "animal/dog bite", "animal bite", "dog bite", "animal & dog bite"),
  "B. Diarrhea"             = c("b. diarrhea", "b diarrhea", "bloody diarrhea", "bloody diarrhoea", "b. diarrhoea"),
  "VH (B, C & D)"           = c("vh (b, c & d)", "vh b c d", "viral hepatitis (b, c & d)", "vh (b,c&d)"),
  "VH (B & C)"             = c("vh (b & c)", "vh (b&c)", "vh b&c", "vh b c", "viral hepatitis (b & c)", "viral hepatitis (b&c)"),
  "Typhoid"                 = c("typhoid", "typhoid fever"),
  "SARI"                    = c("sari", "severe acute respiratory infection", "severe acute respiratory illness"),
  "CL"                      = c("cl", "cutaneous leishmaniasis"),
  "AVH (A & E)"             = c("avh (a & e)", "avh a e", "acute viral hepatitis (a & e)", "avh (a&e)"),
  "Measles"                 = c("measles"),
  "Mumps"                   = c("mumps"),
  "Chickenpox/ Varicella"   = c("chickenpox/ varicella", "chickenpox / varicella", "chicken pox / varicella",
                                "chickenpox varicella", "varicella"),
  "AWD (S. Cholera)"        = c("awd (s. cholera)", "awd s cholera", "acute watery diarrhea (suspected cholera)",
                                "awd (suspected cholera)"),
  "Dengue"                  = c("dengue", "dengue fever"),
  "Meningitis"              = c("meningitis"),
  "AFP"                     = c("afp", "acute flaccid paralysis"),
  "Pertussis"               = c("pertussis", "whooping cough"),
  "HIV/AIDS"                = c("hiv/aids", "hiv / aids", "hiv aids", "hiv"),
  "Gonorrhea"               = c("gonorrhea"),
  "Syphilis"                = c("syphilis"),
  "VL"                      = c("vl", "visceral leishmaniasis", "kala azar", "kala-azar"),
  "Leprosy"                 = c("leprosy"),
  "NT"                      = c("nt", "neonatal tetanus"),
  "Brucellosis"             = c("brucellosis"),
  "Diphtheria (Probable)"   = c("diphtheria (probable)", "diphtheria probable", "diphtheria"),
  "CCHF"                    = c("cchf", "crimean congo hemorrhagic fever", "crimean-congo haemorrhagic fever"),
  "Rubella (CRS)"           = c("rubella (crs)", "rubella crs", "congenital rubella syndrome", "rubella"),
  "Rabies"                  = c("rabies"),
  "COVID-19"                = c("covid-19", "covid 19", "covid19", "sars-cov-2"),
  "Mpox"                    = c("mpox", "monkeypox"),
  "Anthrax"                 = c("anthrax"),
  "Chikungunya"             = c("chikungunya")
)
disease_lookup <- disease_lookup |> lapply(tolower) |> add_ligature_variants()

# Flexible lookup for the region names used in the compliance table.
# NOTE: the compliance table never appears to include Punjab (see the
# "Punjab delayed reporting" question above) but Punjab is included here
# anyway in case a future bulletin does report it.
region_lookup <- list(
  AJK          = c("ajk", "azad jammu kashmir", "azad jammu & kashmir", "azad jammu and kashmir"),
  Balochistan  = c("balochistan"),
  GB           = c("gb", "gilgit baltistan", "gilgit-baltistan"),
  ICT          = c("ict", "islamabad", "islamabad capital territory"),
  KP           = c("kp", "kpk", "khyber pakhtunkhwa"),
  Punjab       = c("punjab"),
  Sindh        = c("sindh"),
  National     = c("national", "pakistan", "overall")
)
region_lookup <- region_lookup |> lapply(tolower) |> add_ligature_variants()

get_bulletin_links <- function(url) {
  # Retrieve bulletin PDF links and their displayed titles from the
  # NIH Pakistan weekly bulletin webpage.
  # Args:
  #   url: URL of the weekly bulletin webpage.
  # Returns:
  #   A data frame containing:
  #      - title: the anchor text (e.g. 'IDSRS Week 26 Bulletin (2026)')
  #      - link: the link to the bulletin PDF
  
  page <- read_html(url)
  
  nodes <- page |>
    html_elements(".list-pdf a")
  
  tibble(
    title = nodes |> html_text2(),
    link = nodes |> html_attr("href")
  ) |>
    mutate(
      link = str_trim(link),
      link = str_replace(link, "\\?.*$", ""),
      # The site's markup occasionally has stray characters trailing the
      # real href (e.g. literal "\r\n" sequences turning up as extra path
      # segments after the file extension). Since every genuine link here
      # is a PDF, the robust fix is to truncate right after ".pdf" rather
      # than trying to strip out each possible flavour of trailing junk.
      link = if_else(
        str_detect(link, regex("\\.pdf", ignore_case = TRUE)),
        str_extract(link, regex("^.*\\.pdf", ignore_case = TRUE)),
        link
      ),
      link = if_else(
        str_detect(link, "^http"),
        link,
        paste0("https://www.nih.org.pk", link)
      )
    )
}

extract_report_date <- function(link_df) {
  # ------------------------------------------------------------------------------
  # Extract the reporting year and week from bulletin titles.
  #
  # Bulletin titles are expected to follow the format:
  #   "IDSRS Week WW Bulletin (YYYY)"
  # for example:
  #   "IDSRS Week 26 Bulletin (2026)"
  #
  # Assumptions:
  #   - The week number immediately follows the word "Week".
  #   - The reporting year is the four-digit number enclosed in parentheses at
  #     the end of the title.
  #
  # Args:
  #   link_df: Data frame returned by get_bulletin_links() containing columns
  #            'title' and 'link'.
  #
  # Returns:
  #   The input data frame with additional columns:
  #     - year: Reporting year.
  #     - week: Week number.
  # ------------------------------------------------------------------------------
  
  link_df |>
    mutate(
      week = as.numeric(
        str_extract(title, "(?<=Week\\s)\\d+")
      ),
      year = as.numeric(
        str_extract(title, "(?<=\\()\\d{4}(?=\\)$)")
      )
    )
}

download_pdf <- function(url) {
  # Download a PDF and extract both its text and its word-level layout data.
  # Args:
  #   url: URL of the PDF.
  # Returns:
  #   A list with:
  #     - text: character vector, the text of each page (as from
  #       pdftools::pdf_text()) -- used for the disease-counts table, which
  #       has a variable row count and is well suited to line-based parsing.
  #     - data: a list of data frames, one per page, each with one row per
  #       word and its x/y bounding-box position (as from
  #       pdftools::pdf_data()) -- used for the compliance table, which has
  #       a fixed, known row set but whose columns can drift onto separate
  #       text lines in ways that defeat line-based heuristics (see
  #       extract_compliance_rows()).
  #   Returns NULL if the download or extraction fails.
  
  # Get url into correct format for reading, e.g., replaces spaces with %20
  url <- utils::URLencode(url)
  
  # Temporary file for the downloaded PDF
  tmp <- tempfile(fileext = ".pdf")
  
  tryCatch({
    
    download.file(
      url = url,
      destfile = tmp,
      mode = "wb",
      quiet = TRUE
    )
    
    list(
      text = pdftools::pdf_text(tmp),
      data = pdftools::pdf_data(tmp)
    )
    
  }, error = function(e) {
    
    warning("Failed to process PDF: ", url)
    
    NULL
    
  }, finally = {
    
    if (file.exists(tmp)) {
      unlink(tmp)
    }
    
  })
}

# ---- Generic fuzzy matcher builder -------------------------------------

build_matcher <- function(lookup){
  variant_to_canonical <- unlist(
    lapply(names(lookup), function(nm) {
      setNames(rep(nm, length(lookup[[nm]])), lookup[[nm]])
    })
  )
  variant_to_canonical
}

column_variant_map   <- build_matcher(column_lookup)
disease_variant_map  <- build_matcher(disease_lookup)
region_variant_map   <- build_matcher(region_lookup)

# TRUE if every character of `needle` appears in `haystack`, in the same
# left-to-right order (not necessarily contiguous). Both arguments should
# already be lowercase with whitespace stripped.
is_subsequence <- function(needle, haystack){
  nn <- nchar(needle)
  if(nn == 0) return(nchar(haystack) == 0)
  ni <- 1L
  for(i in seq_len(nchar(haystack))){
    if(substr(haystack, i, i) == substr(needle, ni, ni)){
      ni <- ni + 1L
      if(ni > nn) return(TRUE)
    }
  }
  FALSE
}

fuzzy_match <- function(raw, variant_map, max_dist = 2){
  # Match disease/column/region names to the standard values
  # Permits:
  #   1. Exact matches, or matches provided in the previous lookups
  #   2. Prefixes, where the prefix is at least 4 characters and is the exact start of value in lookup
  #               e.g. Baloch. or baloch matches to balochistan
  #   3. Fuzzy matching: <= 4 chars : must match exactly, 5-7 chars : 1 difference in string allowed,  8+ chars : 2 errors allowed
  
  clean <- tolower(trimws(raw))
  
  # ---- 1. Exact match (silent) --------------------------------------------
  if(clean %in% names(variant_map)){
    return(unname(variant_map[clean]))
  }
  
  # ---- 2. Prefix match (flagged) ------------------------------------------
  if(nchar(clean) >= 4){
    
    prefix_hits <- which(startsWith(names(variant_map), clean))
    
    if(length(prefix_hits) > 0){
      
      canonicals <- unique(unname(variant_map[prefix_hits]))
      
      if(length(canonicals) == 1){
        
        message(sprintf('Prefix matched "%s" -> "%s" (canonical: "%s")',
                        raw, names(variant_map)[prefix_hits[1]], canonicals))
        
        return(canonicals)
      }
      
      if(length(canonicals) > 1){
        stop(sprintf('Ambiguous prefix match for "%s": matches multiple different values (%s)',
                     raw, paste(canonicals, collapse = ", ")))
      }
    }
  }
  
  # ---- 3. Distance-based fuzzy match (flagged) ----------------------------
  n <- nchar(clean)
  tol <- dplyr::case_when(
    n <= 4 ~ 0,
    n <= 7 ~ 1,
    TRUE   ~ 2
  )
  tol <- min(tol, max_dist)
  
  dists <- stringdist(clean, names(variant_map), method = "osa")
  best_dist <- min(dists)
  
  if(best_dist <= tol){
    
    best_idx   <- which(dists == best_dist)
    canonicals <- unique(unname(variant_map[best_idx]))
    
    if(length(canonicals) > 1){
      stop(sprintf('Ambiguous fuzzy match for "%s": matches multiple different values (%s) at distance %d',
                   raw, paste(canonicals, collapse = ", "), best_dist))
    }
    
    message(sprintf('Fuzzy matched "%s" -> "%s" (canonical: "%s", distance: %d)',
                    raw, names(variant_map)[best_idx[1]], canonicals, best_dist))

    return(canonicals)
  }

  # ---- 4. Subsequence match (flagged) --------------------------------------
  # Handles this pipeline's most stubborn recurring extraction defect: some
  # PDF font subsets drop individual glyphs during text extraction --
  # sometimes leaving a single space behind, sometimes removing them
  # outright -- and it isn't limited to any one letter or ligature (see the
  # "ti" case handled above via add_ligature_variants(); this generalises
  # to ANY missing character or combination, e.g. "Dir Lower" -> "ir ower",
  # "Upper" -> "U er", "Malakand" -> "alakan"). Because the SURVIVING
  # characters stay in their original left-to-right order, a name mangled
  # this way is always a SUBSEQUENCE of its true canonical form once
  # whitespace is stripped from both sides. This is only tried once every
  # other tier above has failed, and only accepted when it's both
  # unambiguous (resolves to exactly one canonical value, breaking ties by
  # preferring the fewest missing characters) and reasonably strong (misses
  # at most 3 characters, and the candidate covers at least half of the
  # canonical name's length) -- guardrails against a short garbled
  # fragment coincidentally subsequence-matching an unrelated short name.
  clean_nospace <- gsub("\\s+", "", clean)
  if(nchar(clean_nospace) >= 4){

    targets         <- names(variant_map)
    targets_nospace <- gsub("\\s+", "", targets)
    gaps            <- nchar(targets_nospace) - nchar(clean_nospace)
    candidate_idx   <- which(gaps >= 0 & gaps <= 3 & nchar(clean_nospace) >= nchar(targets_nospace) / 2)

    if(length(candidate_idx) > 0){

      is_hit <- vapply(candidate_idx, function(i) is_subsequence(clean_nospace, targets_nospace[i]), logical(1))
      hits   <- candidate_idx[is_hit]

      if(length(hits) > 0){

        best_gap   <- min(gaps[hits])
        best_hits  <- hits[gaps[hits] == best_gap]
        canonicals <- unique(unname(variant_map[best_hits]))

        if(length(canonicals) > 1){
          message(sprintf('Ambiguous subsequence match for "%s": matches multiple different values (%s) -- declining to guess, kept as unmatched.',
                       raw, paste(canonicals, collapse = ", ")))
          return(NA_character_)
        }

        message(sprintf('Subsequence matched "%s" -> "%s" (canonical: "%s", %d character(s) missing)',
                        raw, targets[best_hits[1]], canonicals, best_gap))

        return(canonicals)
      }
    }
  }

  NA_character_
}

match_column_name  <- function(x) fuzzy_match(x, column_variant_map,  max_dist = 2)
match_disease_name <- function(x) fuzzy_match(x, disease_variant_map, max_dist = 2)
match_region_name  <- function(x) fuzzy_match(x, region_variant_map,  max_dist = 2)

is_exact_disease_match <- function(raw, variant_map = disease_variant_map){
  tolower(trimws(raw)) %in% names(variant_map)
}

# ---- Coordinate-free repair for genuinely blank (unmarked) data cells ----
#
# merge_wrapped_lines() (below) can only recover a value that's missing
# from a data line when it prints on its own separate physical line
# elsewhere (the "stray value" mechanism) -- a cell that's simply BLANK on
# the SAME line as the rest of its row (no "0", no "NR", nothing at all)
# leaves no such trace: every value after it silently shifts one column to
# the left, and without correction gets attributed to the wrong disease
# entirely.
#
# pdftools::pdf_text() still preserves each column's visual position via
# character spacing though (values are right-padded/aligned within their
# column's width), so the gap left by a blank cell is still visible in the
# RAW line text as a character offset that doesn't line up with where the
# next real value's column should be. estimate_column_end_offsets() learns
# each data column's typical end-of-token character offset from every
# COMPLETE row in the table (i.e. rows with no blank cells to confuse the
# measurement), and resolve_blank_column() then checks a short-by-one-or-
# -more row's own token offsets against those to work out exactly which
# column(s) are blank.
estimate_column_end_offsets <- function(lines, n_cols){

  get_tokens_coarse <- function(x){
    t <- str_split(str_trim(x), "\\s{2,}")[[1]]
    t[nzchar(t)]
  }
  is_numeric_tok <- function(tok) grepl("^(NR|[0-9,]+)$", tok)

  n_data_cols   <- n_cols - 1
  offsets_by_col <- vector("list", n_data_cols)

  for(ln in lines){

    coarse_tokens <- get_tokens_coarse(ln)
    if(length(coarse_tokens) == 0) next
    is_num      <- is_numeric_tok(coarse_tokens)
    data_tokens <- coarse_tokens[is_num]
    if(length(data_tokens) != n_data_cols) next   # only learn from COMPLETE rows

    m <- gregexpr("\\S+", ln)[[1]]
    if(m[1] == -1) next
    ends  <- as.integer(m) + attr(m, "match.length") - 1
    words <- regmatches(ln, gregexpr("\\S+", ln))[[1]]

    num_ends <- ends[is_numeric_tok(words)]
    if(length(num_ends) != n_data_cols) next   # e.g. a numeric-looking name token

    for(i in seq_len(n_data_cols)){
      offsets_by_col[[i]] <- c(offsets_by_col[[i]], num_ends[i])
    }
  }

  # Require at least 3 complete rows' worth of evidence per column before
  # trusting the learned offsets at all.
  if(any(vapply(offsets_by_col, length, integer(1)) < 3)) return(NULL)

  vapply(offsets_by_col, median, numeric(1))
}

resolve_blank_column <- function(ln, col_end_offsets, row_name = NULL, col_names = NULL){

  is_numeric_tok <- function(tok) grepl("^(NR|[0-9,]+)$", tok)

  m <- gregexpr("\\S+", ln)[[1]]
  if(m[1] == -1) return(NULL)
  ends  <- as.integer(m) + attr(m, "match.length") - 1
  words <- regmatches(ln, gregexpr("\\S+", ln))[[1]]

  is_num    <- is_numeric_tok(words)
  num_words <- words[is_num]
  num_ends  <- ends[is_num]

  n_data_cols <- length(col_end_offsets)
  if(length(num_words) == 0 || length(num_words) >= n_data_cols) return(NULL)

  assigned <- vapply(num_ends, function(e) which.min(abs(col_end_offsets - e)), integer(1))
  if(length(unique(assigned)) != length(assigned)) return(NULL)   # collision -- ambiguous
  if(is.unsorted(assigned, strictly = TRUE)) return(NULL)          # must stay left-to-right

  missing_cols <- setdiff(seq_len(n_data_cols), assigned)

  # The LAST column is deliberately left unresolved here even when it's
  # among the missing ones: a value missing from the last column can
  # legitimately mean it printed on its own separate physical line
  # elsewhere (see merge_wrapped_lines' stray-value handling below), which
  # only ever looks at the LAST slot of a row -- resolving it here would
  # pre-empt that and lose a real value. Every OTHER missing column has no
  # such fallback, so is safe to mark directly.
  resolvable_missing <- setdiff(missing_cols, n_data_cols)
  if(length(resolvable_missing) == 0) return(NULL)   # nothing new resolved here

  # A genuinely blank cell is deliberately marked with its own distinct
  # "MISSING" placeholder rather than reusing "NR" -- "NR" means the
  # bulletin explicitly reported "not reported" for that cell, whereas
  # this is this pipeline's own inference that a cell was blank, which is
  # a different (and less certain) kind of gap and shouldn't be recorded
  # as if the source had said so itself. convert_cases_table()/
  # convert_district_cases_table() turn this into Status = "Missing",
  # Cases = NA, distinct from both "reported" and "NR" rows.
  out <- rep(NA_character_, n_data_cols)
  out[assigned]            <- num_words
  out[resolvable_missing]  <- "MISSING"

  blank_names <- if(!is.null(col_names) && length(col_names) == n_data_cols){
    col_names[resolvable_missing]
  } else {
    paste("column", resolvable_missing)
  }
  warning(sprintf(
    'Blank cell (no value at all -- not "0", not "NR") for %s: %s -- recorded as Missing rather than guessed.',
    if(!is.null(row_name) && nzchar(row_name)) sprintf('"%s"', row_name) else "(unnamed row)",
    paste(blank_names, collapse = ", ")
  ))

  out
}

merge_wrapped_lines <- function(lines, n_cols, variant_map = disease_variant_map, column_names = NULL){

  # column_names, when supplied, is the full matched_cols vector for this
  # table (row-label column first, then data columns in the same
  # left-to-right order) -- used only to name which specific column was
  # blank in the warning resolve_blank_column() raises below.
  data_col_names <- if(!is.null(column_names) && length(column_names) == n_cols) column_names[-1] else NULL

  # Names sometimes wrap across two (or three) lines in the source PDF.
  # Pure-text (non-data) lines are tokenized at the WORD level (not just on
  # double-spaces), since footnote/disclaimer text is often single-spaced and
  # glued to a genuine name continuation on the same line (e.g.
  # "(Probable) Punjab Data delayed due to non-reporting by HF" needs to
  # split into "(Probable)" + separate footnote words to be handled correctly).
  #
  # In addition, the last data column (e.g. Total, or Compliance %) is
  # frequently NOT baseline-aligned with the rest of its row -- it looks
  # like it's vertically centered against the full (possibly multi-line)
  # row height, so it can print on its own line, landing before, between,
  # or after the row's other lines in the extracted text stream. These
  # "stray value" lines have exactly one numeric token and no name text.
  #
  # This function is shared between the main disease-counts table (where
  # variant_map = disease_variant_map) and the compliance table (where
  # variant_map = region_variant_map) -- the wrapping/stray-value problems
  # are identical, only the set of "valid names" to match against differs.
  #
  # Approach:
  #   Pass 1: data lines anchor rows as before. A data line with n_cols-1
  #           tokens is a complete row. A data line short one or more
  #           values first goes through resolve_blank_column() (see above),
  #           which uses character-offset evidence from the table's other
  #           complete rows to work out exactly which column(s) are
  #           genuinely blank -- if it can, those get a "MISSING" placeholder
  #           immediately (with a warning identifying the row/column) rather
  #           than being silently misattributed to the wrong disease. Any
  #           shortfall it can't resolve (chiefly: a value missing from the
  #           LAST column, which may simply be printed on its own separate
  #           line elsewhere -- see Pass 3) falls back to the previous
  #           behaviour of treating the row as missing its last value.
  #           A line with a single bare numeric token and no name text is
  #           queued as a stray value rather than forced into a row.
  #           Non-data lines contribute word-level tokens to a "floater run"
  #           between two rows (name-wrap handling, unchanged).
  #   Pass 2: for a run between two rows, find the single split point
  #           minimising combined match score of both sides (as before).
  #           For a TRAILING run (nothing after it), search for the
  #           shortest prefix that best improves the previous row's match
  #           score; anything beyond that point is discarded as noise.
  #   Pass 3: resolve stray values -- each is assigned to whichever
  #           value-missing row is nearest to it (by row index / "look
  #           above or below"), preferring the row immediately preceding it.
  
  get_tokens_coarse <- function(x){
    t <- str_split(str_trim(x), "\\s{2,}")[[1]]
    t[nzchar(t)]
  }
  get_tokens_fine <- function(x){
    t <- str_split(str_trim(x), "\\s+")[[1]]
    t[nzchar(t)]
  }
  is_numeric_tok <- function(tok) grepl("^(NR|[0-9,]+)$", tok)
  bracket_open   <- function(text) str_count(text, "\\(") > str_count(text, "\\)")

  # Occasionally two (or more) adjacent numeric values on the same data
  # line are separated by only a SINGLE space rather than the 2+ spaces
  # normally used between columns (observed so far only in a table's
  # "Total" checksum row, where wide numbers appear to get tighter
  # kerning) -- since get_tokens_coarse() above only splits on 2+ spaces,
  # such values get picked up as one glued-together, non-numeric-looking
  # token (e.g. "2,331 1,358"), undercounting the row's real value count
  # and misclassifying that chunk as name/label text. Any coarse token
  # that ISN'T itself a valid value but explodes into two or more valid
  # values when split on ANY whitespace is safe to expand back into
  # separate tokens -- a genuine row-label never looks like that.
  split_falsely_merged_numeric_tokens <- function(coarse_tokens){
    unlist(lapply(coarse_tokens, function(tok){
      if(is_numeric_tok(tok)) return(tok)
      parts <- str_split(tok, "\\s+")[[1]]
      parts <- parts[nzchar(parts)]
      if(length(parts) > 1 && all(is_numeric_tok(parts))) parts else tok
    }))
  }
  
  score_name <- function(name, vmap = variant_map){
    clean <- tolower(trimws(name))
    if(nchar(clean) == 0) return(Inf)
    if(clean %in% names(vmap)) return(0)
    dists <- stringdist(clean, names(vmap), method = "osa")
    min(dists)
  }
  
  # ---- Pass 1: data-anchored rows + name-only floater runs -----------------
  rows <- list()
  runs <- list()
  stray_values <- list()
  
  current_run <- character(0)
  
  flush_run <- function(){
    if(length(current_run) > 0){
      runs[[length(runs) + 1]] <<- list(after_row = length(rows), tokens = current_run)
      current_run <<- character(0)
    }
  }
  
  row_missing_last <- function(r){
    n <- length(r$data_tokens)
    n > 0 && is.na(r$data_tokens[n])
  }

  # Learned once per table, from every complete (no blank cells) data row --
  # see estimate_column_end_offsets() above. NULL when there isn't enough
  # clean evidence to trust (falls back to the old last-column assumption
  # everywhere below).
  col_end_offsets <- estimate_column_end_offsets(lines, n_cols)

  for(ln in lines){

    coarse_tokens <- get_tokens_coarse(ln)
    if(length(coarse_tokens) == 0) next
    coarse_tokens <- split_falsely_merged_numeric_tokens(coarse_tokens)

    is_num <- is_numeric_tok(coarse_tokens)

    if(any(is_num)){
      leading_name <- coarse_tokens[!is_num]
      data_tokens  <- coarse_tokens[is_num]

      if(length(leading_name) == 0 && length(data_tokens) == 1){
        # A lone numeric token with no name text -- almost certainly a
        # value that has drifted onto its own line. Queue it; it gets
        # attached to a row in the resolution pass below rather than
        # being forced in here (we don't yet reliably know which row it
        # belongs to).
        stray_values[[length(stray_values) + 1]] <- list(
          after_row = length(rows),
          value     = data_tokens
        )
        next
      }

      flush_run()

      if(length(data_tokens) == n_cols - 1){

        rows[[length(rows) + 1]] <- list(name_parts = leading_name, data_tokens = data_tokens)

      } else if(length(data_tokens) < n_cols - 1){

        # Short by one or more values. First, try to work out exactly which
        # column(s) are genuinely BLANK on this same line, via character-
        # offset comparison against the table's other complete rows (see
        # resolve_blank_column() above) -- this correctly handles a blank
        # cell in the MIDDLE of a row, which would otherwise get silently
        # misattributed to the wrong column entirely. That repair
        # deliberately leaves the LAST column alone even if it looks
        # blank, since a missing last value can legitimately mean it
        # printed on its own separate line elsewhere (handled below/in the
        # stray-value pass) -- so falling through to the old "assume the
        # last column is missing" placeholder is still correct/needed for
        # that case, and as a last-resort fallback if the offset repair
        # isn't confident enough to say anything at all.
        repaired <- if(!is.null(col_end_offsets)){
          resolve_blank_column(
            ln, col_end_offsets,
            row_name  = paste(leading_name, collapse = " "),
            col_names = data_col_names
          )
        } else NULL

        if(!is.null(repaired)){

          rows[[length(rows) + 1]] <- list(name_parts = leading_name, data_tokens = repaired)

        } else if(length(data_tokens) == n_cols - 2){

          # Missing exactly one value -- almost always the last column, which
          # (per the note above) often prints on a separate line. Create the
          # row now with a placeholder NA; it gets filled in by the
          # stray-value resolution pass.
          rows[[length(rows) + 1]] <- list(
            name_parts  = leading_name,
            data_tokens = c(data_tokens, NA_character_)
          )

        } else {
          stop("Data line has ", length(data_tokens), " value(s), expected ",
               n_cols - 1, " (or ", n_cols - 2, " with the last value on its own line): ",
               paste(coarse_tokens, collapse = " "))
        }

      } else {
        stop("Data line has ", length(data_tokens), " value(s), expected ",
             n_cols - 1, " (more than expected): ",
             paste(coarse_tokens, collapse = " "))
      }
      
    } else {
      # Pure-text line -- use WORD-level tokens so any footnote text glued
      # to a genuine name fragment can be separated later.
      current_run <- c(current_run, get_tokens_fine(ln))
    }
  }
  flush_run()
  
  # ---- Pass 2: resolve each name-wrap run --------------------------------
  for(run in runs){
    
    left_idx  <- run$after_row
    right_idx <- run$after_row + 1
    tok       <- run$tokens
    m         <- length(tok)
    
    has_left  <- left_idx  >= 1 && left_idx  <= length(rows)
    has_right <- right_idx >= 1 && right_idx <= length(rows)
    
    left_base  <- if(has_left)  paste(rows[[left_idx]]$name_parts,  collapse = " ") else ""
    right_base <- if(has_right) paste(rows[[right_idx]]$name_parts, collapse = " ") else ""
    
    if(!has_left && !has_right){
      next  # nothing to attach to either side -- drop (shouldn't normally happen)
    }
    
    if(!has_left){
      # Nothing before the first row -- attach everything to the right.
      rows[[right_idx]]$name_parts <- c(tok, rows[[right_idx]]$name_parts)
      next
    }
    
    if(!has_right){
      
      # TRAILING run: search every possible prefix length. If ANY prefix
      # produces an exact match against the lookup, take the shortest such
      # prefix immediately (an exact match is always preferable to a merely
      # "improved" fuzzy score). Otherwise fall back to whichever prefix
      # gives the best (lowest) fuzzy score. Anything beyond the chosen
      # split point is discarded as noise (footnote/disclaimer text).
      
      best_s     <- 0
      best_score <- score_name(left_base)
      exact_found <- (best_score == 0)
      
      if(nchar(trimws(left_base)) == 0){
        # This row has no name at all yet (a "numbers first, name second"
        # row -- see the both-sides branch below for how that arises).
        # Any text is strictly better than an empty name, so claim the
        # entire trailing run rather than searching for a fuzzy match.
        best_s <- m
      } else {
        
        for(s in seq_len(m)){
          
          candidate <- paste(c(left_base, tok[seq_len(s)]), collapse = " ")
          sc <- score_name(candidate)
          
          if(sc == 0){
            best_s      <- s
            best_score  <- 0
            exact_found <- TRUE
            break
          }
          
          if(!exact_found && sc < best_score){
            best_score <- sc
            best_s     <- s
          }
        }
      }
      
      if(best_s > 0){
        rows[[left_idx]]$name_parts <- c(rows[[left_idx]]$name_parts, tok[seq_len(best_s)])
      }
      if(best_s < m){
        warning("Discarding trailing text as noise: ", paste(tok[(best_s + 1):m], collapse = " "))
      }
      next
    }
    
    # ---- Both sides present ------------------------------------------------
    # First, check whether attaching the ENTIRE run to one side gives an
    # exact match. This matters because summing score_name(left) +
    # score_name(right) breaks down when one side is still completely
    # nameless (e.g. a "numbers first, name second" table layout, where a
    # run sits between two number-only rows that haven't received their own
    # dedicated name run yet): an empty side scores Inf, and a mediocre
    # finite fuzzy split across both sides can numerically beat a perfect
    # exact match on just one side purely because Inf so heavily dominates
    # the sum. An outright exact match on one full side is always the right
    # call regardless of what state the other side is in, so it's checked
    # first and short-circuits the additive search entirely.
    full_run         <- paste(tok, collapse = " ")
    left_full_score  <- score_name(paste(c(left_base, full_run), collapse = " "))
    right_full_score <- score_name(paste(c(full_run, right_base), collapse = " "))
    
    if(left_full_score == 0){
      
      # Prefer completing the left (preceding) row whenever that's an
      # exact match -- including when the right side would ALSO score an
      # exact match (which happens when both adjacent rows are still
      # completely nameless, e.g. right_base == "" too: appending the run
      # to either trivially "matches" itself). A name run belongs to the
      # row whose numbers already appeared, not the row still to come.
      best_s <- m
      
    } else if(right_full_score == 0){
      
      best_s <- 0
      
    } else {
      
      # No unambiguous whole-run exact match -- fall back to the
      # fine-grained split search (genuine cross-row name wraps, e.g.
      # "Islamabad Capital" + "Territory" spanning two lines of the SAME
      # row's name).
      min_split    <- 0
      running_left <- left_base
      while(min_split < m && bracket_open(running_left)){
        min_split    <- min_split + 1
        running_left <- paste(running_left, tok[min_split])
      }
      
      best_s     <- min_split
      best_score <- Inf
      
      for(s in min_split:m){
        left_name  <- paste(c(left_base, tok[seq_len(s)]), collapse = " ")
        right_name <- if(s < m) paste(c(tok[(s + 1):m], right_base), collapse = " ") else right_base
        
        score <- score_name(left_name) + score_name(right_name)
        
        if(score < best_score){
          best_score <- score
          best_s     <- s
        }
      }
    }
    
    if(best_s > 0){
      rows[[left_idx]]$name_parts <- c(rows[[left_idx]]$name_parts, tok[seq_len(best_s)])
    }
    if(best_s < m){
      rows[[right_idx]]$name_parts <- c(tok[(best_s + 1):m], rows[[right_idx]]$name_parts)
    }
  }
  
  # ---- Pass 3: resolve stray last-column values (values on their own line) ----
  if(length(stray_values) > 0){
    
    pending <- which(vapply(rows, row_missing_last, logical(1)))
    
    for(st in stray_values){
      
      if(length(pending) == 0){
        warning("Found a stray value with no row left to attach it to: ", st$value)
        next
      }
      
      # "Look above or below": attach to whichever pending row is nearest
      # (by row index) to the point in the source where this value was
      # encountered. Ties favor the row above (lower index), since values
      # more often print just after their row than just before the next.
      dist    <- abs(pending - st$after_row)
      nearest <- pending[which.min(dist)]
      
      rows[[nearest]]$data_tokens[length(rows[[nearest]]$data_tokens)] <- st$value
      pending <- pending[pending != nearest]
    }
  }
  
  still_missing <- which(vapply(rows, row_missing_last, logical(1)))
  if(length(still_missing) > 0){
    last_col_name <- if(!is.null(data_col_names)) data_col_names[length(data_col_names)] else "the last column"
    warning(sprintf(
      'Blank cell (no value at all -- not "0", not "NR") for %s: %s -- recorded as Missing rather than guessed.',
      last_col_name,
      paste(vapply(rows[still_missing], function(r) paste(r$name_parts, collapse = " "), character(1)),
            collapse = "; ")
    ))
    # Mark with the same "MISSING" placeholder resolve_blank_column() uses,
    # rather than leaving a bare NA -- paste()-ing an NA into the
    # reassembled row string below would otherwise stringify it as the
    # literal text "NA", which downstream would be neither a valid number
    # nor a recognised "NR"/"MISSING" token and would just get silently
    # dropped instead of recorded.
    for(i in still_missing){
      rows[[i]]$data_tokens[length(rows[[i]]$data_tokens)] <- "MISSING"
    }
  }
  
  # ---- Reassemble into the original flat-string row format ------------------
  vapply(rows, function(r){
    full_name <- paste(r$name_parts, collapse = " ")
    paste(c(full_name, r$data_tokens), collapse = "   ")
  }, character(1))
}


split_glued_token <- function(token, lookup = column_lookup){
  
  clean <- tolower(token)
  
  # Try every pair of (start_variant, end_variant) to see if the token is
  # exactly two known column names concatenated with no space between them.
  all_variants <- unlist(lookup, use.names = FALSE)
  all_variants <- all_variants[order(-nchar(all_variants))]  # longest first, avoids partial matches
  
  for(v1 in all_variants){
    if(startsWith(clean, v1)){
      
      remainder <- substring(clean, nchar(v1) + 1)
      remainder <- trimws(remainder)
      
      if(nchar(remainder) == 0) next  # token WAS just v1 alone, nothing to split
      
      for(v2 in all_variants){
        if(remainder == v2){
          return(c(v1, v2))
        }
      }
    }
  }
  
  NULL  # could not decompose into two known tokens
}

# ---- Vocabulary-guided splitting for headers where two OR MORE columns'
# labels got jammed together, not necessarily contiguously. split_glued_token()
# above only handles exactly two labels concatenated with NO separator at
# all; this handles the messier case seen in some bulletins where several
# short, single-physical-line column labels sit close enough together that
# pdftools only leaves a single space between them (rather than the 2+
# spaces the coarse tokenizer treats as a column boundary), AND the header
# label spans multiple physical lines besides -- since header fragments are
# pooled top-to-bottom by physical line (see merge_multiline_header()), a
# short one-line label's word(s) can end up sandwiched INSIDE a longer
# wrapped label's text. E.g. "ALRI" (line 1) + "< 5 Typhoid SARI" (line 2)
# + "years" (line 3) all cluster together (same x-offset neighbourhood) and
# concatenate to "ALRI < 5 Typhoid SARI years", where "Typhoid" and "SARI"
# are two unrelated columns' labels injected between "5" and "years" (the
# rest of "ALRI < 5 ... years"). This peels every known vocabulary phrase
# (from `lookup`) out of the blob's words as an in-order (not necessarily
# contiguous) subsequence, trying multi-word/longer phrases first so they
# get first claim on shared words, then orders the resulting columns by
# where each one's first claimed word sat in the original blob. If any
# words are left over unclaimed, this declines (returns NULL) rather than
# guess -- the caller's existing "unmatched column header" warning covers
# that case exactly as it did before this function existed.
split_blob_into_known_columns <- function(token, lookup){

  words <- str_split(trimws(token), "\\s+")[[1]]
  words <- words[nzchar(words)]
  if(length(words) <= 1) return(NULL)

  words_lower <- tolower(words)
  remaining   <- rep(TRUE, length(words))

  phrases <- list()
  for(canonical in names(lookup)){
    for(variant in lookup[[canonical]]){
      vwords <- str_split(trimws(variant), "\\s+")[[1]]
      vwords <- vwords[nzchar(vwords)]
      if(length(vwords) == 0) next
      phrases[[length(phrases) + 1]] <- list(canonical = canonical, words = vwords)
    }
  }
  if(length(phrases) == 0) return(NULL)
  phrase_order <- order(-vapply(phrases, function(p) length(p$words), integer(1)),
                        -vapply(phrases, function(p) sum(nchar(p$words)), integer(1)))
  phrases <- phrases[phrase_order]

  matches <- list()

  for(p in phrases){
    avail_idx <- which(remaining)
    if(length(avail_idx) < length(p$words)) next

    pos <- integer(0)
    search_from <- 1
    ok <- TRUE
    for(w in p$words){
      hit <- NA_integer_
      if(search_from <= length(avail_idx)){
        for(j in seq(search_from, length(avail_idx))){
          if(words_lower[avail_idx[j]] == tolower(w)){ hit <- j; break }
        }
      }
      if(is.na(hit)){ ok <- FALSE; break }
      pos <- c(pos, avail_idx[hit])
      search_from <- hit + 1
    }

    if(ok && length(pos) == length(p$words)){
      remaining[pos] <- FALSE
      matches[[length(matches) + 1]] <- list(canonical = p$canonical, first = min(pos))
    }
  }

  if(any(remaining))       return(NULL)  # leftover words nobody claimed -- decline
  if(length(matches) < 2)  return(NULL)  # nothing actually split

  ord <- order(vapply(matches, function(m) m$first, numeric(1)))
  vapply(matches[ord], function(m) m$canonical, character(1))
}

# ---- Generic helper: locate a table's header line by keyword hits ------

find_header_line <- function(lines, header_keywords, min_matches){
  # Returns the index (within `lines`) of the first line that mentions at
  # least `min_matches` of `header_keywords`, or NA if none do.
  keyword_counts <- vapply(lines, function(ln){
    sum(sapply(header_keywords, function(k) grepl(k, ln, ignore.case = TRUE)))
  }, integer(1))
  
  hits <- which(keyword_counts >= min_matches)
  if(length(hits) == 0) return(NA_integer_)
  hits[1]
}

# ---- Main disease-counts table ------------------------------------------

extract_table_rows <- function(pdf_text){
  
  page <- pdf_text[grepl("Table\\s*1\\b", pdf_text, ignore.case = TRUE)]
  if(length(page) == 0) return(NULL)
  
  lines <- str_split(page, "\\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]
  
  header_keywords <- c("Disease", "AJK", "Balochistan", "GB", "ICT", "KP", "Punjab", "Sindh", "Total")
  
  start <- find_header_line(lines, header_keywords, min_matches = 4)
  if(is.na(start)) return(NULL)
  
  header_line   <- lines[start]
  header_tokens <- str_split(str_trim(header_line), "\\s{2,}")[[1]]
  matched_cols  <- vapply(header_tokens, match_column_name, character(1))
  
  if(!("Disease" %in% matched_cols)){
    header_tokens <- c("Disease", header_tokens)
    matched_cols  <- c("Disease", matched_cols)
  }
  
  if(any(is.na(matched_cols))){
    
    new_tokens  <- list()
    new_matches <- list()
    
    for(idx in seq_along(header_tokens)){
      
      if(!is.na(matched_cols[idx])){
        new_tokens[[idx]]  <- header_tokens[idx]
        new_matches[[idx]] <- matched_cols[idx]
        next
      }
      
      split_attempt <- split_glued_token(header_tokens[idx])

      if(!is.null(split_attempt)){
        resolved <- vapply(split_attempt, match_column_name, character(1))
        if(!any(is.na(resolved))){
          message(sprintf('Split glued header token "%s" -> %s',
                          header_tokens[idx], paste(split_attempt, collapse = " + ")))
          new_tokens[[idx]]  <- split_attempt
          new_matches[[idx]] <- resolved
          next
        }
      }

      blob_split <- split_blob_into_known_columns(header_tokens[idx], lookup = column_lookup)

      if(!is.null(blob_split)){
        message(sprintf('Split header token "%s" -> %s',
                        header_tokens[idx], paste(blob_split, collapse = " + ")))
        new_tokens[[idx]]  <- blob_split
        new_matches[[idx]] <- blob_split
        next
      }

      new_tokens[[idx]]  <- header_tokens[idx]
      new_matches[[idx]] <- NA_character_
    }

    header_tokens <- unlist(new_tokens)
    matched_cols  <- unlist(new_matches)

    if(any(is.na(matched_cols))){
      stop("Unmatched column header(s): ",
           paste(header_tokens[is.na(matched_cols)], collapse = ", "))
    }
  }
  
  rows <- lines[(start + 1):length(lines)]
  figure_idx <- which(grepl("^\\s*Figure", rows, ignore.case = TRUE))
  if(length(figure_idx) > 0){
    rows <- rows[seq_len(min(figure_idx) - 1)]
  }
  rows <- rows[!grepl("Page\\s*\\|", rows, ignore.case = TRUE)]   # <-- drop footer lines
  rows <- merge_wrapped_lines(rows, n_cols = length(matched_cols), variant_map = disease_variant_map, column_names = matched_cols)
  
  list(
    rows    = rows,
    columns = matched_cols
  )
}

convert_cases_table <- function(extracted){
  
  rows         <- extracted$rows
  matched_cols <- extracted$columns
  
  if(is.null(matched_cols) || length(matched_cols) == 0){
    stop("`extracted` has no columns — check header parsing for this page.")
  }
  
  if(is.null(rows) || length(rows) == 0){
    stop("`extracted` has no rows — check row extraction/merging for this page.")
  }
  
  split_lengths <- rows |>
    str_replace_all("(?<=\\d),(?=\\d)", "") |>
    str_split("\\s{2,}") |>
    map_int(length)
  
  good_rows <- rows[split_lengths == length(matched_cols)]
  
  if(any(split_lengths != length(matched_cols))){
    warning(sum(split_lengths != length(matched_cols)),
            " row(s) dropped due to unexpected column count: ",
            paste(rows[split_lengths != length(matched_cols)], collapse = " | "))
  }
  
  data <- good_rows |>
    str_replace_all("(?<=\\d),(?=\\d)", "") |>
    str_split_fixed("\\s{2,}", n = length(matched_cols)) |>
    as_tibble(.name_repair = ~ matched_cols)
  
  colnames(data) <- matched_cols
  
  data <- data |>
    mutate(
      Disease_raw = Disease,
      Disease = vapply(Disease, match_disease_name, character(1)),
      Disease = coalesce(Disease, Disease_raw)   # keep unmatched names visible, not NA
    )
  
  unresolved <- data |> filter(Disease == Disease_raw & !(Disease %in% names(disease_lookup)))
  if(nrow(unresolved) > 0){
    stop(sprintf(
      "%d disease name(s) unmatched, kept as-is: %s",
      nrow(unresolved), paste(unique(unresolved$Disease_raw), collapse = "; ")
    ))
  }
  
  cases_table = data |>
    select(-Disease_raw) |>
    pivot_longer(-Disease, names_to = "Province", values_to = "Cases") |>
    mutate(
      # Keep an explicit "NR" cell as its own row (Cases = NA, Status =
      # "NR") rather than dropping it outright. This preserves the
      # distinction, downstream, between "this disease WAS in the table
      # this week, but this region reported nothing" (a real row, flagged
      # NR) and "this disease wasn't a row in this week's table at all"
      # (which never reaches this point in the first place -- there's no
      # PDF row to pivot from, so it's simply absent from the output,
      # exactly as before). "MISSING" is a third, distinct case: a cell
      # this pipeline itself INFERRED was blank in the source PDF (see
      # resolve_blank_column()), as opposed to "NR" which the bulletin
      # explicitly printed -- kept separate (Status = "Missing") rather
      # than folded into either "NR" or "reported", since it's a different
      # (and less certain) kind of gap. Any other non-numeric cell is
      # dropped as before.
      Status = dplyr::case_when(
        Cases == "NR"      ~ "NR",
        Cases == "MISSING" ~ "Missing",
        TRUE                ~ "reported"
      ),
      Cases  = na_if(Cases, "NR"),
      Cases  = na_if(Cases, "MISSING"),
      Cases  = as.numeric(Cases)
    ) |>
    filter(!is.na(Cases) | Status %in% c("NR", "Missing"))

  return(cases_table)
}

write_cases_to_csv <- function(cases_table, metadata, file_loc = 'Data/PAK_IDSR_Data.csv'){
  
  # ---- Compute the date corresponding to the last day of the epi week ----
  # Assumes week 1 = days 1-7 of the year, week 2 = days 8-14, etc.
  # i.e. "last date in week" = Jan 1 + (week * 7) - 1 days.
  # Adjust this formula if your source defines epi weeks differently
  # (e.g. ISO 8601 weeks, which start on Mondays and follow different rules).
  
  week_end_date <- as.Date(paste0(metadata$year, "-01-01")) + (metadata$week * 7) - 1
  
  # ---- Combine cases_table with metadata columns --------------------------
  
  out <- cases_table |>
    mutate(
      Week           = metadata$week,
      Year           = metadata$year,
      `Source link`  = metadata$link,
      `Source title` = metadata$title,
      date           = week_end_date,
      datetime_loaded = Sys.time()
    )
  
  # ---- Create directory if it doesn't exist --------------------------------
  
  dir_create(path_dir(file_loc))
  
  # ---- Write: create with header if new, append without header if exists --
  
  file_exists <- file.exists(file_loc)
  
  write_csv(
    out,
    file      = file_loc,
    append    = file_exists,
    col_names = !file_exists
  )
  
  invisible(out)
}

# ---- Compliance table -----------------------------------------------------
#
# The compliance table has no consistent label ("Table 1" etc.), is preceded
# by a variable amount of title/bullet text, and -- unlike the disease-counts
# table -- its exact column layout can be unreliable in the underlying PDF:
# individual cells (in observed real bulletins, sometimes a row's Expected/
# Received values, sometimes its Total-equivalent) can render on a
# completely separate text line from the rest of their own row, in ways
# that don't follow one single consistent pattern. Trying to keep chasing
# each new drift pattern with text-line heuristics doesn't scale, so this
# uses word-level (x, y) coordinates from pdftools::pdf_data() instead:
# each word is bucketed into a column purely by its x-position (relative to
# the header labels' x-positions), then within each column the words are
# read top-to-bottom by y. Because each of the 4 columns is processed
# independently, it doesn't matter which physical text line a value prints
# on, or whether a cell's text itself is split into multiple word-tokens
# (e.g. a dropped "ti" ligature turning "National" into two word-tokens,
# "Na" and "onal", both still land in the Region column and still end up
# concatenated back together within that row's y-cluster).
#
# What IS consistent across all seen examples:
#   - It appears somewhere in the first few pages of the bulletin.
#   - Its header row mentions "Region", "Expected", "Received" and
#     "Compliance", left-to-right in that order.
#   - Its last row is always the "National" total.

extract_compliance_rows <- function(pdf_data, max_pages = 5){
  
  header_labels  <- c("Region", "Expected", "Received", "Compliance")
  n_search_pages <- min(max_pages, length(pdf_data))
  diagnostics    <- character(0)
  
  for(p in seq_len(n_search_pages)){
    
    words <- pdf_data[[p]]
    if(is.null(words) || nrow(words) == 0){
      diagnostics <- c(diagnostics, sprintf("page %d: empty page", p))
      next
    }
    
    # ---- Locate the header row -------------------------------------------
    kw_hit <- vapply(words$text, function(t){
      any(vapply(header_labels, function(k) grepl(k, t, ignore.case = TRUE), logical(1)))
    }, logical(1))
    header_words <- words[kw_hit, ]
    if(nrow(header_words) < 3){
      diagnostics <- c(diagnostics, sprintf("page %d: no Region/Expected/Received/Compliance keywords found", p))
      next
    }
    
    # Bullet-point prose before the table often repeats a header word (e.g.
    # "compliance" appearing several times in "The national compliance
    # rate..." etc.), so the y band with the most raw keyword hits is often
    # NOT the header. Instead, cluster keyword hits by y-proximity and take
    # whichever cluster contains the most DISTINCT header labels together
    # -- that's what actually identifies a genuine header row.
    header_words <- header_words[order(header_words$y), ]
    row_break <- c(TRUE, diff(header_words$y) > 8)
    header_words$cluster <- cumsum(row_break)
    
    label_of <- function(txt){
      hits <- header_labels[vapply(header_labels, function(k) grepl(k, txt, ignore.case = TRUE), logical(1))]
      if(length(hits) == 0) NA_character_ else hits[1]
    }
    header_words$label <- vapply(header_words$text, label_of, character(1))
    
    cluster_scores <- tapply(header_words$label, header_words$cluster, function(l) length(unique(l)))
    if(max(cluster_scores) < 3){
      diagnostics <- c(diagnostics, sprintf("page %d: header keywords found but never clustered together (max %d distinct labels at one y)", p, max(cluster_scores)))
      next
    }
    best_cluster <- names(cluster_scores)[which.max(cluster_scores)]
    
    header_words <- header_words[header_words$cluster == best_cluster, ]
    header_y     <- mean(header_words$y)
    if(nrow(header_words) < 3){
      diagnostics <- c(diagnostics, sprintf("page %d: header cluster had fewer than 3 labels after filtering", p))
      next
    }
    
    col_x <- setNames(rep(NA_real_, length(header_labels)), header_labels)
    for(lbl in header_labels){
      hit <- header_words[grepl(lbl, header_words$text, ignore.case = TRUE), ]
      if(nrow(hit) > 0) col_x[lbl] <- min(hit$x)
    }
    # Columns should read left-to-right in exactly this order; if any
    # header label is missing, or they aren't in increasing x order, this
    # isn't (or isn't reliably) the table we're looking for.
    if(any(is.na(col_x))){
      diagnostics <- c(diagnostics, sprintf("page %d: header found but missing one of Region/Expected/Received/Compliance", p))
      next
    }
    if(is.unsorted(col_x)){
      diagnostics <- c(diagnostics, sprintf("page %d: header labels found but not in left-to-right Region/Expected/Received/Compliance order", p))
      next
    }
    
    # ---- Bucket everything below the header row -----------------------
    # The table can run across a page break (e.g. the first 5 regions on
    # one page, "Sindh" and "National" continuing at the top of the next
    # page with NO header repeated there). So rather than restrict body to
    # just this page, pull in up to 2 subsequent pages' content too, each
    # shifted by a large y-offset so it sorts strictly after this page's
    # remaining content -- the region/numeric logic below doesn't care
    # which page a word came from, only its relative order.
    y_offset_unit <- 100000
    body_parts <- list(words[words$y > header_y + 8, ])
    n_continuation_pages <- min(2, length(pdf_data) - p)
    if(n_continuation_pages > 0){
      for(k in seq_len(n_continuation_pages)){
        next_page <- pdf_data[[p + k]]
        if(!is.null(next_page) && nrow(next_page) > 0){
          next_page$y <- next_page$y + k * y_offset_unit
          body_parts[[length(body_parts) + 1]] <- next_page
        }
      }
    }
    body <- bind_rows(body_parts)
    if(nrow(body) == 0){
      diagnostics <- c(diagnostics, sprintf("page %d: nothing found below the header row", p))
      next
    }
    
    is_num <- grepl("^[0-9,]+$", body$text)
    
    # ---- Region column: non-numeric tokens -----------------------------
    # This step doesn't depend on x-boundaries at all (every non-numeric
    # token is unconditionally Region), so it's used to establish the real
    # table's row count and y-range FIRST, before tackling the 3 numeric
    # columns below.
    region_words <- body[!is_num, ]
    region_words <- region_words[order(region_words$y), ]
    if(nrow(region_words) == 0){
      diagnostics <- c(diagnostics, sprintf("page %d: no region-name text found below the header", p))
      next
    }
    
    # First, group into tight physical-line clusters (words on the exact
    # same line, e.g. "Khyber" + "Pakhtunkhwa").
    row_break <- c(TRUE, diff(region_words$y) > 8)
    region_words$row_id <- cumsum(row_break)
    
    line_tbl <- region_words |>
      group_by(row_id) |>
      summarise(name = paste(text, collapse = " "), y = min(y), .groups = "drop") |>
      arrange(y)
    
    # Drop obvious footer/page-marker fragments (e.g. a page number like
    # "Page | 3" has its digit filtered out as numeric, leaving a stray
    # "Page |" line here) before the greedy region matching below. Left
    # in, such a line can accumulate together with subsequent genuinely
    # good lines (e.g. "Sindh", "National") before the noise-reset
    # threshold is reached, discarding all of them together as one bad
    # buffer instead of just the footer itself. Its y is also recorded so
    # any numeric token at that same y (e.g. the page-number digit itself,
    # which survives the is_num filter) can later be excluded from the
    # numeric x-clustering below -- otherwise it becomes a contaminating
    # outlier that skews where the column boundaries are drawn.
    footer_mask <- grepl("^\\s*Page\\b", line_tbl$name, ignore.case = TRUE) | line_tbl$name == "|"
    footer_ys   <- line_tbl$y[footer_mask]
    line_tbl    <- line_tbl[!footer_mask, ]
    if(nrow(line_tbl) == 0){
      diagnostics <- c(diagnostics, sprintf("page %d: region text found but produced no line groups", p))
      next
    }
    
    # A region name can wrap across two (or more) physical lines (e.g.
    # "Islamabad Capital" / "Territory", with its Expected/Received/
    # Compliance values sitting at a y roughly BETWEEN the two name lines
    # -- vertically centered against the two-line-tall cell). The gap
    # between those two name lines is often indistinguishable from the gap
    # between two genuinely different rows, since both are just "the next
    # line down" at the same fixed line height. So rather than trust that
    # gap, physical lines are greedily accumulated in y-order and tested
    # against the known region vocabulary: once the accumulated text
    # resolves to a valid region, that closes the row. If 3 lines
    # accumulate without a match, it's almost certainly noise (stray
    # footer/prose text caught in this y-range), so the buffer is dropped
    # and accumulation restarts from the next line.
    rows_acc <- list()
    buf      <- character(0)
    buf_y    <- NA_real_
    is_exact_region_match <- function(x) tolower(trimws(x)) %in% names(region_variant_map)
    for(i in seq_len(nrow(line_tbl))){
      if(length(buf) == 0) buf_y <- line_tbl$y[i]
      buf <- c(buf, line_tbl$name[i])
      candidate <- paste(buf, collapse = " ")
      
      # An EXACT match is required to close a row here, not the lenient
      # prefix/fuzzy matching match_region_name() normally allows -- e.g.
      # "Islamabad Capital" alone already prefix-matches the full
      # "islamabad capital territory" lookup entry, which would wrongly
      # close the row before "Territory" (the actual next physical line)
      # is ever consumed. Only once 3 lines have accumulated without an
      # exact hit does the lenient matcher get a last try, in case a
      # genuinely fuzzy/abbreviated multi-line name needs it.
      if(is_exact_region_match(candidate)){
        rows_acc[[length(rows_acc) + 1]] <- list(name = candidate, y = buf_y, region = match_region_name(candidate))
        buf <- character(0)
        if(rows_acc[[length(rows_acc)]]$region == "National") break
      } else if(length(buf) >= 3){
        resolved <- tryCatch(match_region_name(candidate), error = function(e) NA_character_)
        if(!is.na(resolved)){
          rows_acc[[length(rows_acc) + 1]] <- list(name = candidate, y = buf_y, region = resolved)
          if(resolved == "National") break
        }
        buf <- character(0)
      }
    }
    
    if(length(rows_acc) == 0 || rows_acc[[length(rows_acc)]]$region != "National"){
      diagnostics <- c(diagnostics, sprintf(
        "page %d: matched %d region row(s) (%s) but never reached National",
        p, length(rows_acc),
        if(length(rows_acc) > 0) paste(vapply(rows_acc, function(r) r$region, character(1)), collapse = ", ") else "none"
      ))
      next
    }
    
    region_tbl <- tibble(
      name = vapply(rows_acc, function(r) r$name, character(1)),
      y    = vapply(rows_acc, function(r) r$y,    numeric(1))
    )
    n_rows <- nrow(region_tbl)
    
    # ---- Numeric columns: cluster by x within the real table's y-range ----
    # Numeric columns are commonly right-aligned, so a short value (e.g. a
    # 2-digit "23") can sit noticeably further right within its column than
    # a long one (e.g. a 4-digit "2315") in the same column -- sometimes
    # far enough right to cross a boundary based on the HEADER label's x
    # position, which is typically left-aligned. So rather than trust
    # header-derived boundaries, the actual numeric x-positions are
    # clustered directly: restricting to y <= the National row's y (which
    # excludes footer/page-number junk below the table) and splitting on
    # the two largest gaps in x should cleanly separate Expected/Received/
    # Compliance regardless of alignment or digit-count quirks.
    y_cutoff <- region_tbl$y[n_rows] + 15
    numeric_words <- body[is_num & body$y <= y_cutoff, ]
    if(length(footer_ys) > 0){
      near_footer <- vapply(numeric_words$y, function(yy) any(abs(yy - footer_ys) <= 8), logical(1))
      numeric_words <- numeric_words[!near_footer, ]
    }
    if(nrow(numeric_words) < n_rows * 3){
      diagnostics <- c(diagnostics, sprintf(
        "page %d: found all %d region rows (through National) but only %d numeric value(s) above it -- expected at least %d",
        p, n_rows, nrow(numeric_words), n_rows * 3
      ))
      next
    }
    
    ux <- sort(unique(numeric_words$x))
    if(length(ux) < 3){
      diagnostics <- c(diagnostics, sprintf("page %d: numeric values found but only %d distinct x-position(s) among them", p, length(ux)))
      next
    }
    gaps <- diff(ux)
    top_gap_idx <- order(gaps, decreasing = TRUE)[seq_len(min(2, length(gaps)))]
    x_breaks <- sort(ux[top_gap_idx] + gaps[top_gap_idx] / 2)
    if(length(x_breaks) < 2){
      diagnostics <- c(diagnostics, sprintf("page %d: couldn't split numeric values into 3 distinct x-clusters", p))
      next
    }
    
    numeric_words$column <- c("Expected", "Received", "Compliance")[
      findInterval(numeric_words$x, x_breaks) + 1
    ]
    
    get_column_values <- function(col_name){
      cw <- numeric_words[numeric_words$column == col_name, ]
      cw <- cw[order(cw$y), ]
      cw$text
    }
    expected_vals   <- get_column_values("Expected")
    received_vals   <- get_column_values("Received")
    compliance_vals <- get_column_values("Compliance")
    
    if(length(expected_vals) < n_rows || length(received_vals) < n_rows ||
       length(compliance_vals) < n_rows){
      diagnostics <- c(diagnostics, sprintf(
        "page %d: after x-clustering, columns had %d/%d/%d values (Expected/Received/Compliance) but needed %d each",
        p, length(expected_vals), length(received_vals), length(compliance_vals), n_rows
      ))
      next
    }
    
    rows_out <- paste(
      region_tbl$name,
      expected_vals[seq_len(n_rows)],
      received_vals[seq_len(n_rows)],
      compliance_vals[seq_len(n_rows)],
      sep = "   "
    )
    
    return(list(rows = rows_out, columns = header_labels))
  }
  
  stop("No compliance table found in the first ", n_search_pages,
       " page(s) of this bulletin — expected a table with Region/Expected/",
       "Received/Compliance columns ending in a National row.",
       if(length(diagnostics) > 0) paste0("\nDiagnostics:\n  - ", paste(diagnostics, collapse = "\n  - ")) else "")
}

convert_compliance_table <- function(extracted){
  
  rows <- extracted$rows
  cols <- extracted$columns
  
  if(is.null(rows) || length(rows) == 0){
    stop("`extracted` has no compliance rows — check header parsing for this page.")
  }
  
  split_lengths <- rows |>
    str_split("\\s{2,}") |>
    map_int(length)
  
  good_rows <- rows[split_lengths == length(cols)]
  
  if(any(split_lengths != length(cols))){
    stop(sum(split_lengths != length(cols)),
         " compliance row(s) had an unexpected column count: ",
         paste(rows[split_lengths != length(cols)], collapse = " | "))
  }
  
  data <- good_rows |>
    str_split_fixed("\\s{2,}", n = length(cols)) |>
    as_tibble(.name_repair = ~ cols)
  
  data <- data |>
    mutate(
      Region_raw = Region,
      Region     = vapply(Region, match_region_name, character(1)),
      Region     = coalesce(Region, Region_raw)
    )
  
  unresolved <- data |> filter(Region == Region_raw & !(Region %in% names(region_lookup)))
  if(nrow(unresolved) > 0){
    stop(sprintf(
      "%d region name(s) unmatched, kept as-is: %s",
      nrow(unresolved), paste(unique(unresolved$Region_raw), collapse = "; ")
    ))
  }
  
  data |>
    select(-Region_raw) |>
    rename(
      `Expected Reports` = Expected,
      `Received Reports` = Received,
      `Compliance (%)`   = Compliance
    ) |>
    mutate(across(
      c(`Expected Reports`, `Received Reports`, `Compliance (%)`),
      ~ as.numeric(str_replace_all(.x, ",", ""))
    ))
}

write_compliance_to_csv <- function(compliance_table, metadata, file_loc = 'Data/PAK_IDSR_Compliance.csv'){
  
  out <- compliance_table |>
    mutate(
      Week           = metadata$week,
      Year           = metadata$year,
      `Source link`  = metadata$link,
      `Source title` = metadata$title,
      datetime_loaded = Sys.time()
    )
  
  dir_create(path_dir(file_loc))
  
  file_exists <- file.exists(file_loc)
  
  write_csv(
    out,
    file      = file_loc,
    append    = file_exists,
    col_names = !file_exists
  )
  
  invisible(out)
}

# ==========================================================================
# ---- District-level extraction (disease cases & compliance) ------------
# ==========================================================================
# ---- District-level lookups -------------------------------------------
# Compiled from the district names observed in NIH Pakistan weekly bulletins
# (both the "Table 2/3/4: District wise distribution..." case tables and the
# "Table 6/7: ...compliance of IDSR reporting districts..." tables). Districts
# are occasionally spelled differently between these two table types within
# the SAME bulletin (e.g. "Karachi Keamari" vs "Karachi-Kemari"), so each
# canonical name carries the variant spellings seen in practice.
#
# This list will not be perfectly exhaustive forever (new districts are
# occasionally created, e.g. by splitting an existing one) -- unmatched
# district names are kept as-is (with a warning) rather than causing the
# whole page to fail, so the pipeline degrades gracefully when it does.

district_region_lookup <- list(
  AJK = list(
    "Neelum"          = c("neelum"),
    "Jhelum Velley"   = c("jhelum velley", "jhelum vellay", "jhelum valley"),
    "Sudhnooti"       = c("sudhnooti", "sudhnuti"),
    "Mirpur"          = c("mirpur"),
    "Bhimber"         = c("bhimber"),
    "Kotli"           = c("kotli"),
    "Muzaffarabad"    = c("muzaffarabad"),
    "Poonch"          = c("poonch"),
    "Haveli"          = c("haveli"),
    "Bagh"            = c("bagh")
  ),
  Balochistan = list(
    "Awaran"          = c("awaran"),
    "Barkhan"         = c("barkhan"),
    "Chagai"          = c("chagai"),
    "Chaman"          = c("chaman"),
    "Dera Bugti"      = c("dera bugti"),
    "Duki"            = c("duki"),
    "Gwadar"          = c("gwadar"),
    "Harnai"          = c("harnai"),
    "Hub"             = c("hub"),
    "Jaffarabad"      = c("jaffarabad"),
    "Jhal Magsi"      = c("jhal magsi"),
    "Kachhi (Bolan)"  = c("kachhi (bolan)", "kachhi", "bolan"),
    "Kalat"           = c("kalat"),
    "Kech (Turbat)"   = c("kech (turbat)", "kech", "turbat"),
    "Kharan"          = c("kharan"),
    "Khuzdar"         = c("khuzdar"),
    "Killa Abdullah"  = c("killa abdullah"),
    "Killa Saifullah" = c("killa saifullah"),
    "Kohlu"           = c("kohlu"),
    "Lasbella"        = c("lasbella"),
    "Loralai"         = c("loralai"),
    "Mastung"         = c("mastung"),
    "MusaKhel"        = c("musakhel", "musa khel", "musa khail"),
    "Naseerabad"      = c("naseerabad"),
    "Nushki"          = c("nushki"),
    "Panjgur"         = c("panjgur"),
    "Pishin"          = c("pishin"),
    "Quetta"          = c("quetta"),
    "Sherani"         = c("sherani"),
    "Sibi"            = c("sibi"),
    "Sohbat pur"      = c("sohbat pur", "sohbatpur"),
    "Surab"           = c("surab"),
    "Usta Muhammad"   = c("usta muhammad"),
    "Washuk"          = c("washuk"),
    "Zhob"            = c("zhob"),
    "Ziarat"          = c("ziarat")
  ),
  GB = list(
    "Hunza"           = c("hunza"),
    "Nagar"           = c("nagar"),
    "Ghizer"          = c("ghizer"),
    "Gilgit"          = c("gilgit"),
    "Diamer"          = c("diamer"),
    "Astore"          = c("astore"),
    "Shigar"          = c("shigar"),
    "Skardu"          = c("skardu"),
    "Ganche"          = c("ganche", "ghanche"),
    "Kharmang"        = c("kharmang")
  ),
  ICT = list(
    "ICT"             = c("ict", "islamabad capital territory"),
    "CDA"             = c("cda", "capital development authority")
  ),
  KP = list(
    "Abbottabad"                = c("abbottabad"),
    "Bajaur"                    = c("bajaur"),
    "Bannu"                     = c("bannu"),
    "Battagram"                 = c("battagram"),
    "Buner"                     = c("buner"),
    "Charsadda"                 = c("charsadda"),
    "Chitral Lower"             = c("chitral lower", "lower chitral"),
    "Chitral Upper"             = c("chitral upper", "upper chitral"),
    "D.I. Khan"                 = c("d.i. khan", "di khan", "dera ismail khan"),
    "Dir Lower"                 = c("dir lower", "lower dir"),
    "Dir Upper"                 = c("dir upper", "upper dir"),
    "Hangu"                     = c("hangu"),
    "Haripur"                   = c("haripur"),
    "Karak"                     = c("karak"),
    "Khyber"                    = c("khyber"),
    "Kohat"                     = c("kohat"),
    "Kohistan Lower"            = c("kohistan lower", "lower kohistan"),
    "Kohistan Upper"            = c("kohistan upper", "upper kohistan"),
    "Kolai Palas"               = c("kolai palas"),
    "L & C Kurram"              = c("l & c kurram", "lower & central kurram", "lower and central kurram"),
    "Lakki Marwat"              = c("lakki marwat"),
    "Malakand"                  = c("malakand"),
    "Mansehra"                  = c("mansehra"),
    "Mardan"                    = c("mardan"),
    "Mohmand"                   = c("mohmand"),
    "North Waziristan"          = c("north waziristan"),
    "Nowshera"                  = c("nowshera"),
    "Orakzai"                   = c("orakzai"),
    "Peshawar"                  = c("peshawar"),
    "SD DI Khan"                = c("sd di khan", "sd d.i. khan"),
    "SD Peshawar"               = c("sd peshawar"),
    "SD Tank"                   = c("sd tank"),
    "Shangla"                   = c("shangla"),
    # South Waziristan is split into Lower/Upper in the district-level case
    # tables, but the district-level COMPLIANCE tables in some bulletins
    # (e.g. Week 07 2024, Week 50 2023, Week 43 2024) report it as a single
    # combined "South Waziristan" row instead. Without this entry, that bare
    # form is an exact prefix of both split names, which fuzzy_match() (correctly)
    # refuses to resolve on its own -- so it's kept as its own canonical
    # district here, matched only by the unqualified form.
    "South Waziristan"          = c("south waziristan"),
    "South Waziristan (Lower)"  = c("south waziristan (lower)", "south waziristan lower", "swl"),
    "South Waziristan (Upper)"  = c("south waziristan (upper)", "south waziristan upper", "swu"),
    "SWA"                       = c("swa"),
    "Swabi"                     = c("swabi"),
    "Swat"                      = c("swat"),
    "Tank"                      = c("tank"),
    "Tor Ghar"                  = c("tor ghar", "torghar"),
    "Upper Kurram"              = c("upper kurram")
  ),
  Sindh = list(
    "Badin"                  = c("badin"),
    "Dadu"                   = c("dadu"),
    "Ghotki"                 = c("ghotki"),
    "Hyderabad"              = c("hyderabad"),
    "Jacobabad"              = c("jacobabad"),
    "Jamshoro"               = c("jamshoro"),
    "Kamber Shadadkot"       = c("kamber shadadkot", "kamber shahdadkot", "kamber", "kambar shahdadkot"),
    "Karachi Central"        = c("karachi central", "karachi-central"),
    "Karachi East"           = c("karachi east", "karachi-east"),
    "Karachi Keamari"        = c("karachi keamari", "karachi-kemari", "karachi kemari", "karachi-keamari"),
    "Karachi Korangi"        = c("karachi korangi", "karachi-korangi"),
    "Karachi Malir"          = c("karachi malir", "karachi-malir"),
    "Karachi South"          = c("karachi south", "karachi-south"),
    "Karachi West"           = c("karachi west", "karachi-west"),
    "Kashmore"               = c("kashmore", "kashmor"),
    "Khairpur"               = c("khairpur"),
    "Larkana"                = c("larkana"),
    "Matiari"                = c("matiari"),
    "Mirpurkhas"             = c("mirpurkhas", "mirpur khas"),
    "Naushero Feroze"        = c("naushero feroze", "naushahro feroze"),
    "Sanghar"                = c("sanghar"),
    "Shaheed Benazirabad"    = c("shaheed benazirabad"),
    "Shikarpur"              = c("shikarpur"),
    "Sujawal"                = c("sujawal"),
    "Sukkur"                 = c("sukkur"),
    "Tando Allahyar"         = c("tando allahyar"),
    "Tando Muhammad Khan"    = c("tando muhammad khan"),
    "Tharparkar"             = c("tharparkar"),
    "Thatta"                 = c("thatta"),
    "Umerkot"                = c("umerkot")
  )
)

# Flatten into (a) a canonical-name -> variants lookup for fuzzy matching,
# in the same shape/spirit as disease_lookup/region_lookup, and (b) a
# canonical-name -> region lookup used to tag each district row with its
# province/region.
district_lookup <- unlist(unname(district_region_lookup), recursive = FALSE)
district_lookup <- district_lookup |> lapply(tolower) |> add_ligature_variants()

district_to_region <- unlist(lapply(names(district_region_lookup), function(region){
  setNames(rep(region, length(district_region_lookup[[region]])), names(district_region_lookup[[region]]))
}))

district_variant_map <- build_matcher(district_lookup)
match_district_name   <- function(x) fuzzy_match(x, district_variant_map, max_dist = 2)
region_of_district     <- function(canonical_district) unname(district_to_region[canonical_district])

# ---- Generic: find every "Table N:" title line across the document -------
# Used to bound multi-page tables generically: a table's content runs from
# its own title line until the NEXT "Table N:" title line found anywhere in
# the document (on any later page), regardless of what that next table is
# labelled -- this is robust to tables spanning a variable number of pages,
# and to the exact table numbers drifting between bulletins.
find_all_table_titles <- function(pdf_text){
  out <- list()
  for(p in seq_along(pdf_text)){
    lines <- str_split(pdf_text[[p]], "\\n")[[1]]
    hits <- which(grepl("^\\s*Table\\s*\\d+\\s*:", lines, ignore.case = TRUE))
    for(h in hits){
      out[[length(out) + 1]] <- list(page = p, line = h, text = str_trim(lines[h]))
    }
  }
  if(length(out) == 0) return(tibble(page = integer(0), line = integer(0), text = character(0)))
  tibble(
    page = vapply(out, function(x) x$page, integer(1)),
    line = vapply(out, function(x) x$line, integer(1)),
    text = vapply(out, function(x) x$text, character(1))
  )
}

# For a table starting at (start_page, start_line), work out which page it
# ends on: the page before whichever "Table N:" title comes next in the
# document (on a later page, or later on the same page), or the last page
# of the document if this is the final table.
table_end_page <- function(all_titles, start_page, start_line, n_pages){
  later <- all_titles |>
    filter(page > start_page | (page == start_page & line > start_line)) |>
    arrange(page, line)
  if(nrow(later) == 0) return(n_pages)
  later$page[1] - 1L
}

drop_footer_lines <- function(lines){
  lines[!grepl("Page\\s*\\|", lines, ignore.case = TRUE)]
}


# ---- Multi-line header reconstruction -----------------------------------
# Disease-name column headers in the district tables often wrap across up
# to 3 physical text lines (e.g. "AD (Non-" / "Cholera)" either side of the
# line that actually contains the word "Districts"), AND the wrapped
# fragments are not necessarily left-aligned to their column's data -- they
# can be positioned anywhere within the column's horizontal span. Working
# from pdf_text() (plain strings, no word coordinates) rather than
# pdf_data(), column identity is recovered from the character column
# (start-offset) each token begins at, since pdftools preserves the
# original visual spacing of these particular tables. Tokens from ALL header
# lines are pooled and clustered by start-offset (a small-gap cluster is
# "the same column"), then re-assembled top-to-bottom within each cluster,
# and clusters are read out left-to-right -- this correctly reconstructs
# column order even when a short single-line label (e.g. "Malaria") sits
# between two fragments of a longer wrapped label (e.g. "AD (Non-" /
# "Cholera)") without ever appearing contiguous in the raw text.

tokenize_with_offsets <- function(line){
  n <- nchar(line)
  gaps <- str_locate_all(line, " {2,}")[[1]]
  if(nrow(gaps) == 0){
    starts <- 1; ends <- n
  } else {
    starts <- c(1, gaps[, 2] + 1)
    ends   <- c(gaps[, 1] - 1, n)
  }
  keep <- starts <= ends
  starts <- starts[keep]; ends <- ends[keep]
  if(length(starts) == 0) return(data.frame(start = integer(0), end = integer(0), text = character(0)))
  text <- substr(rep(line, length(starts)), starts, ends)
  out <- lapply(seq_along(text), function(i){
    t <- text[i]
    if(trimws(t) == "") return(NULL)
    lead <- attr(regexpr("^ *", t), "match.length")
    list(start = as.integer(starts[i] + lead), end = as.integer(ends[i]), text = trimws(t))
  })
  out <- out[!vapply(out, is.null, logical(1))]
  if(length(out) == 0) return(data.frame(start = integer(0), end = integer(0), text = character(0)))
  data.frame(
    start = vapply(out, function(x) x$start, integer(1)),
    end   = vapply(out, function(x) x$end,   integer(1)),
    text  = vapply(out, function(x) x$text,  character(1))
  )
}

merge_multiline_header <- function(lines, gap_threshold = 4){
  toks <- do.call(rbind, lapply(seq_along(lines), function(i){
    t <- tokenize_with_offsets(lines[i])
    if(nrow(t) == 0) return(NULL)
    t$line <- i
    t
  }))
  if(is.null(toks) || nrow(toks) == 0) return(character(0))
  toks <- toks[order(toks$start), ]
  breaks <- c(TRUE, diff(toks$start) > gap_threshold)
  toks$cluster <- cumsum(breaks)
  agg <- do.call(rbind, lapply(split(toks, toks$cluster), function(g){
    g <- g[order(g$line), ]
    data.frame(text = paste(g$text, collapse = " "), start = min(g$start))
  }))
  agg <- agg[order(agg$start), ]
  agg$text
}

is_data_line <- function(ln, min_numeric = 1){
  toks <- str_split(str_trim(ln), "\\s{2,}")[[1]]
  toks <- toks[nzchar(toks)]
  sum(grepl("^(NR|[0-9,]+)$", toks)) >= min_numeric
}

# Given the line index of a header anchor (a line known to be part of the
# header block, e.g. containing "Districts"), grow upward and downward to
# capture every header line, stopping at the table title, a bullet-point
# line, a blank line, or the first genuine data row.
find_header_block <- function(lines, anchor){
  begin <- anchor
  while(begin > 1 &&
        nzchar(trimws(lines[begin - 1])) &&
        !is_data_line(lines[begin - 1]) &&
        !grepl("^\\s*Table\\s*\\d+\\s*:", lines[begin - 1], ignore.case = TRUE) &&
        !grepl("^\\s*[•●]", lines[begin - 1])){
    begin <- begin - 1
  }
  end <- anchor
  while(end < length(lines) &&
        nzchar(trimws(lines[end + 1])) &&
        !is_data_line(lines[end + 1])){
    end <- end + 1
  }
  c(begin, end)
}

# ---- District-wise disease case tables (Table 2 / 3 / 4 style) -----------
#
# One such table appears per (large) province -- in every bulletin seen so
# far: Sindh, Balochistan and KP. Its shape is structurally identical to the
# national Table 1 (a header row of disease names, followed by one data row
# per name, with values possibly wrapping across lines) -- the only
# difference is the row label is a DISTRICT rather than a disease, and the
# table is titled "... District wise distribution ... <Province>." rather
# than being found by the "Table 1" label. So this reuses
# merge_wrapped_lines()/the disease column-matching machinery from the
# national table, just matched against the district lookup for row names.

extract_district_case_tables <- function(pdf_text){
  
  all_titles <- find_all_table_titles(pdf_text)
  title_hits <- all_titles |> filter(grepl("district\\s*wise\\s*distribution", text, ignore.case = TRUE))
  
  if(nrow(title_hits) == 0) return(list())
  
  results <- list()
  
  for(i in seq_len(nrow(title_hits))){
    
    start_page <- title_hits$page[i]
    start_line <- title_hits$line[i]
    end_page   <- table_end_page(all_titles, start_page, start_line, length(pdf_text))
    title_text <- title_hits$text[i]
    
    # ---- Province for this table: the text between the last comma and the
    # trailing period/end of the title line, e.g. "...Week 30, Sindh." -> "Sindh"
    province_raw <- str_match(title_text, ",\\s*([^,]+?)\\.?\\s*$")[, 2]
    province <- if(!is.na(province_raw)) tryCatch(match_region_name(province_raw), error = function(e) NA_character_) else NA_character_
    
    # ---- Gather this table's lines across all its pages ----
    page_lines <- lapply(start_page:end_page, function(p){
      lines <- str_split(pdf_text[[p]], "\\n")[[1]]
      if(p == start_page) lines <- lines[(start_line + 1):length(lines)]
      lines
    })
    lines <- unlist(page_lines)
    lines <- lines[nzchar(trimws(lines))]
    lines <- drop_footer_lines(lines)
    
    # The row-label column is normally headed "Districts", but at least one
    # observed bulletin (Week 22, 2023, KP table) mislabels it "Diseases"
    # instead -- almost certainly a copy/paste error in the source document,
    # since every row in that column is still a genuine district name. Both
    # labels are accepted as the anchor; "Districts" still wins whenever
    # both appear, since find_header_line() returns the earliest matching
    # line and the genuine header always comes immediately after the table
    # title (any later, unrelated "Diseases" mention -- e.g. in a footnote
    # -- sits further down and is never reached first).
    header_keywords <- c("Districts", "Diseases")
    anchor <- find_header_line(lines, header_keywords, min_matches = 1)
    if(is.na(anchor)){
      warning(sprintf('District case table "%s": could not find a "Districts"/"Diseases" header row -- skipped.', title_text))
      next
    }

    # The disease-name headers can wrap across up to 3 physical lines
    # straddling the anchor line (see find_header_block()/merge_multiline_header()
    # above) -- gather the whole block and reconstruct column order by
    # character-column position rather than trusting a single line.
    header_bounds <- find_header_block(lines, anchor)
    start         <- header_bounds[2]   # last header line -- rows begin after this
    header_tokens <- merge_multiline_header(lines[header_bounds[1]:header_bounds[2]])
    matched_cols  <- vapply(header_tokens, function(tok){
      if(grepl("^districts?$|^diseases?$", tok, ignore.case = TRUE)) return("District")
      match_disease_name(tok)
    }, character(1))
    
    if(!("District" %in% matched_cols)){
      header_tokens <- c("District", header_tokens)
      matched_cols  <- c("District", matched_cols)
    }
    
    if(any(is.na(matched_cols))){
      
      new_tokens  <- list()
      new_matches <- list()
      
      for(idx in seq_along(header_tokens)){
        
        if(!is.na(matched_cols[idx])){
          new_tokens[[idx]]  <- header_tokens[idx]
          new_matches[[idx]] <- matched_cols[idx]
          next
        }
        
        split_attempt <- split_glued_token(header_tokens[idx], lookup = disease_lookup)

        if(!is.null(split_attempt)){
          resolved <- vapply(split_attempt, match_disease_name, character(1))
          if(!any(is.na(resolved))){
            new_tokens[[idx]]  <- split_attempt
            new_matches[[idx]] <- resolved
            next
          }
        }

        # Fallback: several columns' labels jammed together (not necessarily
        # contiguously -- see split_blob_into_known_columns()'s comment) by a
        # combination of single-space gaps and multi-line header wrapping.
        blob_split <- split_blob_into_known_columns(header_tokens[idx], lookup = disease_lookup)

        if(!is.null(blob_split)){
          message(sprintf('Split header token "%s" -> %s',
                          header_tokens[idx], paste(blob_split, collapse = " + ")))
          new_tokens[[idx]]  <- blob_split
          new_matches[[idx]] <- blob_split  # already canonical names
          next
        }

        new_tokens[[idx]]  <- header_tokens[idx]
        new_matches[[idx]] <- NA_character_
      }

      header_tokens <- unlist(new_tokens)
      matched_cols  <- unlist(new_matches)

      if(any(is.na(matched_cols))){
        warning(sprintf('District case table "%s": unmatched column header(s): %s -- skipped.',
                        title_text, paste(header_tokens[is.na(matched_cols)], collapse = ", ")))
        next
      }
    }
    
    rows <- lines[(start + 1):length(lines)]
    figure_idx <- which(grepl("^\\s*Figure", rows, ignore.case = TRUE))
    if(length(figure_idx) > 0){
      rows <- rows[seq_len(min(figure_idx) - 1)]
    }
    # Stop at the next bullet-point narrative block (a line starting with the
    # bullet character), in case pagination pulls in the start of the next
    # section's prose before the next Table title is reached.
    bullet_idx <- which(grepl("^\\s*[•●]", rows))
    if(length(bullet_idx) > 0){
      rows <- rows[seq_len(min(bullet_idx) - 1)]
    }
    # The table always ends with a "Total" row -- anything captured after it
    # (e.g. narrative text for other regions on the way to the next labelled
    # table) is discarded outright rather than left for merge_wrapped_lines'
    # noise-discarding fallback to clean up.
    total_idx <- which(grepl("^\\s*Total\\b", rows, ignore.case = TRUE))
    if(length(total_idx) > 0){
      rows <- rows[seq_len(min(total_idx))]
    }

    rows <- tryCatch(
      merge_wrapped_lines(rows, n_cols = length(matched_cols), variant_map = district_variant_map, column_names = matched_cols),
      error = function(e){
        warning(sprintf('District case table "%s": failed to parse rows: %s', title_text, conditionMessage(e)))
        character(0)
      }
    )
    
    if(length(rows) == 0) next
    
    results[[length(results) + 1]] <- list(
      rows     = rows,
      columns  = matched_cols,
      province = province,
      title    = title_text
    )
  }
  
  results
}

convert_district_cases_table <- function(extracted){
  
  rows         <- extracted$rows
  matched_cols <- extracted$columns
  
  split_lengths <- rows |>
    str_replace_all("(?<=\\d),(?=\\d)", "") |>
    str_split("\\s{2,}") |>
    map_int(length)
  
  good_rows <- rows[split_lengths == length(matched_cols)]
  
  if(any(split_lengths != length(matched_cols))){
    warning(sum(split_lengths != length(matched_cols)),
            ' row(s) dropped from district table "', extracted$title,
            '" due to unexpected column count: ',
            paste(rows[split_lengths != length(matched_cols)], collapse = " | "))
  }
  
  if(length(good_rows) == 0) return(NULL)
  
  data <- good_rows |>
    str_replace_all("(?<=\\d),(?=\\d)", "") |>
    str_split_fixed("\\s{2,}", n = length(matched_cols)) |>
    as_tibble(.name_repair = ~ matched_cols)
  
  colnames(data) <- matched_cols
  
  data <- data |>
    mutate(
      District_raw = District,
      District     = vapply(District, match_district_name, character(1)),
      District     = coalesce(District, District_raw)
    )
  
  unmatched <- data |> filter(District == District_raw & !(District %in% names(district_lookup)) & District_raw != "Total")
  if(nrow(unmatched) > 0){
    warning(sprintf('District table "%s": %d district name(s) unmatched, kept as-is: %s',
                    extracted$title, nrow(unmatched), paste(unique(unmatched$District_raw), collapse = "; ")))
  }
  
  # The "Total" row is a column-sum check row, not a real district -- drop it
  # from the output, but use it to sanity-check the province total reported
  # in the national Table 1 (best-effort: any mismatch is only a warning).
  total_row <- data |> filter(tolower(District_raw) == "total")
  data <- data |> filter(tolower(District_raw) != "total")
  
  cases_table <- data |>
    select(-District_raw) |>
    pivot_longer(-District, names_to = "Disease", values_to = "Cases") |>
    mutate(
      # "MISSING" marks a cell this pipeline itself inferred was blank in
      # the source PDF (see resolve_blank_column()), as distinct from an
      # "NR" the bulletin explicitly printed -- kept as its own Status
      # rather than folded into either "NR" or "reported".
      Status = dplyr::case_when(
        Cases == "NR"      ~ "NR",
        Cases == "MISSING" ~ "Missing",
        TRUE                ~ "reported"
      ),
      Cases  = na_if(Cases, "NR"),
      Cases  = na_if(Cases, "MISSING"),
      Cases  = as.numeric(Cases)
    ) |>
    filter(!is.na(Cases) | Status %in% c("NR", "Missing")) |>
    mutate(
      Province = extracted$province,
      Region   = coalesce(region_of_district(District), Province)
    ) |>
    select(Province, Region, District, Disease, Cases, Status)

  # ---- Best-effort sanity check against the table's own "Total" row -----
  # The "Total" row is a column-sum checksum printed in the source PDF
  # itself. Comparing it against the actual sum of this table's own
  # district-level values catches extraction problems that don't raise a
  # parse error but DO produce a wrong number -- e.g. a value whose digits
  # get split across two different positions in the PDF (observed: a
  # province's ILI total rendered partly as a stray value on its own
  # separate line and partly as a much smaller number on the Total row's
  # own line, extracting as "9" instead of the true "5,209" -- the stray
  # part has nowhere reliable to attach to, so is dropped with its own
  # warning, and the Total row is left visibly wrong here rather than
  # silently). Only checked for diseases with no "NR"/"Missing" rows in
  # this table, since an exact reconciliation isn't possible to know
  # either way once any district's true count is unknown.
  if(nrow(total_row) == 1){
    for(disease_col in matched_cols[-1]){
      col_rows <- cases_table |> filter(Disease == disease_col)
      if(nrow(col_rows) == 0 || any(col_rows$Status %in% c("NR", "Missing"))) next
      reported_sum   <- sum(col_rows$Cases, na.rm = TRUE)
      printed_total  <- suppressWarnings(as.numeric(total_row[[disease_col]]))
      if(!is.na(printed_total) && printed_total != reported_sum){
        warning(sprintf(
          'District table "%s": printed "Total" for %s is %s but summing this table\'s own district rows gives %s -- the printed total looks wrong (likely a PDF text-extraction glitch) and is excluded from the output; only the individual district rows are kept.',
          extracted$title, disease_col, printed_total, reported_sum
        ))
      }
    }
  }

  attr(cases_table, "total_row") <- total_row
  cases_table
}

write_district_cases_to_csv <- function(cases_table, metadata, file_loc = 'Data/PAK_IDSR_Data_District.csv'){
  
  week_end_date <- as.Date(paste0(metadata$year, "-01-01")) + (metadata$week * 7) - 1
  
  out <- cases_table |>
    mutate(
      Week            = metadata$week,
      Year            = metadata$year,
      `Source link`   = metadata$link,
      `Source title`  = metadata$title,
      date            = week_end_date,
      datetime_loaded = Sys.time()
    )
  
  dir_create(path_dir(file_loc))
  file_exists <- file.exists(file_loc)
  write_csv(out, file = file_loc, append = file_exists, col_names = !file_exists)
  
  invisible(out)
}

# ---- District-level compliance tables (Table 6 / 7 style) ----------------
#
# These tables ("... IDSR reporting districts ..." and "... reporting
# Tertiary care hospitals ...") list one row per district/site with its
# Total Sites / Reported Sites / Compliance Rate, grouped under a
# "Provinces/Regions" label that -- unlike every other table in this
# pipeline -- is NOT repeated per row. It's rendered once per group and
# vertically centred against that group's (variable-height) block of rows,
# so its exact line position relative to the data rows isn't reliable to
# parse from. Rather than trying to reconstruct that layout, this ignores
# the region-label column entirely and instead derives each row's Region by
# looking its District up in district_to_region -- the same lookup used for
# the disease tables. That sidesteps the alignment problem altogether and is
# robust to however the region label happens to wrap/centre in a given
# bulletin.

extract_district_compliance_tables <- function(pdf_text){
  
  all_titles <- find_all_table_titles(pdf_text)
  title_hits <- all_titles |> filter(grepl("reporting\\s+(districts?|tertiary)", text, ignore.case = TRUE))
  
  if(nrow(title_hits) == 0) return(list())
  
  results <- list()
  
  for(i in seq_len(nrow(title_hits))){
    
    start_page <- title_hits$page[i]
    start_line <- title_hits$line[i]
    end_page   <- table_end_page(all_titles, start_page, start_line, length(pdf_text))
    title_text <- title_hits$text[i]
    
    facility_type <- if(grepl("tertiary", title_text, ignore.case = TRUE)) "Tertiary Care Hospital" else "IDSR Reporting Site"
    
    page_lines <- lapply(start_page:end_page, function(p){
      lines <- str_split(pdf_text[[p]], "\\n")[[1]]
      if(p == start_page) lines <- lines[(start_line + 1):length(lines)]
      lines
    })
    lines <- unlist(page_lines)
    lines <- lines[nzchar(trimws(lines))]
    lines <- drop_footer_lines(lines)
    
    results[[length(results) + 1]] <- list(
      lines         = lines,
      title         = title_text,
      facility_type = facility_type
    )
  }
  
  results
}

# A trailing run of "<number> <number> <percent>%" tokens identifies a data
# row (the compliance figures); a "-"/blank cell is tolerated as NA. Text
# before that run is the row's name column(s) -- usually just the district,
# occasionally the region label glued onto the same line as the first
# district of its block (e.g. "Islamabad Capital    ICT   24   24   100%").
convert_district_compliance_table <- function(extracted){
  
  lines <- extracted$lines
  
  is_value_tok <- function(tok) grepl("^([0-9,]+%?|-|NR)$", tok)
  
  parsed <- lapply(lines, function(ln){
    toks <- str_split(str_trim(ln), "\\s{2,}")[[1]]
    toks <- toks[nzchar(toks)]
    n <- length(toks)
    if(n < 4) return(NULL)

    # Trailing run of value tokens (numbers / "-" / "NR", the last one
    # optionally ending in "%"). Most bulletins report exactly 3 trailing
    # values (Total Sites, Reported Sites, Compliance %), but at least one
    # observed bulletin (Week 22, 2023) inserts an extra "Number of Agreed
    # Reporting Sites" column between Total and Reported, giving a 4-value
    # run instead. Rather than hard-coding a fixed run length, take
    # whatever trailing value tokens are present: the LAST one (which must
    # end in "%") is always Compliance, the one immediately before it is
    # always the reported-this-week count, and the FIRST one in the run is
    # always the total -- any extra columns in between (e.g. that "Agreed"
    # count) are ignored, since this pipeline's output schema has no place
    # for them and they aren't needed to compute compliance.
    run_start <- n
    while(run_start > 1 && is_value_tok(toks[run_start - 1])) run_start <- run_start - 1
    run <- toks[run_start:n]
    if(length(run) < 3) return(NULL)
    if(!grepl("%$", run[length(run)])) return(NULL)   # last value must be the compliance %

    name_toks <- toks[seq_len(run_start - 1)]
    if(length(name_toks) == 0) return(NULL)
    district_raw <- name_toks[length(name_toks)]
    region_label_raw <- if(length(name_toks) > 1) paste(name_toks[-length(name_toks)], collapse = " ") else NA_character_

    list(
      district_raw      = district_raw,
      region_label_raw  = region_label_raw,
      total_sites       = suppressWarnings(as.numeric(gsub(",", "", run[1]))),
      reported_sites    = suppressWarnings(as.numeric(gsub(",", "", run[length(run) - 1]))),
      compliance_pct    = suppressWarnings(as.numeric(gsub("%", "", run[length(run)])))
    )
  })
  
  parsed <- parsed[!vapply(parsed, is.null, logical(1))]
  if(length(parsed) == 0) return(NULL)
  
  out <- tibble(
    District_raw      = vapply(parsed, function(x) x$district_raw, character(1)),
    RegionLabel_raw   = vapply(parsed, function(x) x$region_label_raw %||% NA_character_, character(1)),
    `Total Sites`     = vapply(parsed, function(x) x$total_sites, numeric(1)),
    `Reported Sites`  = vapply(parsed, function(x) x$reported_sites, numeric(1)),
    `Compliance (%)`  = vapply(parsed, function(x) x$compliance_pct, numeric(1))
  )
  
  out <- out |>
    mutate(
      District = vapply(District_raw, match_district_name, character(1)),
      District = coalesce(District, District_raw),
      Region   = region_of_district(District),
      # Fall back to fuzzy-matching whatever region-label text happened to be
      # glued onto this particular line, for districts the lookup doesn't
      # (yet) know about.
      Region = ifelse(
        is.na(Region) & !is.na(RegionLabel_raw),
        vapply(RegionLabel_raw, function(x) tryCatch(match_region_name(x), error = function(e) NA_character_), character(1)),
        Region
      )
    )
  
  unmatched <- out |> filter(is.na(Region))
  if(nrow(unmatched) > 0){
    warning(sprintf('District compliance table "%s": %d district(s) with no resolvable region, kept with Region = NA: %s',
                    extracted$title, nrow(unmatched), paste(unique(unmatched$District_raw), collapse = "; ")))
  }
  
  out |>
    mutate(`Facility Type` = extracted$facility_type) |>
    select(Region, District, `Facility Type`, `Total Sites`, `Reported Sites`, `Compliance (%)`)
}

`%||%` <- function(x, y) if(is.null(x) || length(x) == 0) y else x

# ---- Fallback: national compliance derived from district-level data -------
#
# At least one observed bulletin (Week 22, 2023) has no native national-level
# Region/Expected/Received/Compliance summary table at all -- only a
# district-level compliance table (see extract_compliance_rows()). Rather
# than simply omitting the national compliance figure for weeks like that,
# this derives the same shape by summing the already-extracted
# district-level figures up to province level, plus a National row summing
# across provinces. Only "IDSR Reporting Site" rows are used -- Tertiary
# Care Hospital compliance is a separate reporting stream and isn't part of
# the national IDSR compliance figure the native table reports.
aggregate_district_compliance_to_national <- function(district_compliance_table){

  base <- district_compliance_table |>
    filter(`Facility Type` == "IDSR Reporting Site", !is.na(Region))

  if(nrow(base) == 0) return(NULL)

  province_totals <- base |>
    group_by(Region) |>
    summarise(
      `Expected Reports` = sum(`Total Sites`, na.rm = TRUE),
      `Received Reports` = sum(`Reported Sites`, na.rm = TRUE),
      .groups = "drop"
    )

  national_total <- tibble(
    Region             = "National",
    `Expected Reports` = sum(province_totals$`Expected Reports`),
    `Received Reports` = sum(province_totals$`Received Reports`)
  )

  bind_rows(province_totals, national_total) |>
    mutate(`Compliance (%)` = round(100 * `Received Reports` / `Expected Reports`))
}

write_district_compliance_to_csv <- function(compliance_table, metadata, file_loc = 'Data/PAK_IDSR_Compliance_District.csv'){
  
  out <- compliance_table |>
    mutate(
      Week            = metadata$week,
      Year            = metadata$year,
      `Source link`   = metadata$link,
      `Source title`  = metadata$title,
      datetime_loaded = Sys.time()
    )
  
  dir_create(path_dir(file_loc))
  file_exists <- file.exists(file_loc)
  write_csv(out, file = file_loc, append = file_exists, col_names = !file_exists)
  
  invisible(out)
}

# ---- Per-bulletin processing (shared by both extraction modes) ---------

process_one_bulletin <- function(week, year, link, title,
                                 cases_file               = 'Data/PAK_IDSR_Data.csv',
                                 compliance_file          = 'Data/PAK_IDSR_Compliance.csv',
                                 district_cases_file      = 'Data/PAK_IDSR_Data_District.csv',
                                 district_compliance_file = 'Data/PAK_IDSR_Compliance_District.csv',
                                 failed_log      = 'Data/failed_reports.txt',
                                 include_district = TRUE){
  
  # Shared line-writer for failed_log -- used both for genuine failures
  # (kind = "ERROR", via log_failure() below) and, via the withCallingHandlers()
  # wrapped around the whole body further down, for every warning() and
  # message() raised anywhere during this bulletin's processing (fuzzy-match
  # notices, blank-cell repairs, unresolved names, header-splitting notes,
  # etc.) -- so a persistent, on-disk record of these exists regardless of
  # how/where process_one_bulletin() is called from, not just when it happens
  # to be run under a caller (like run_pipeline.R) that sets up its own
  # message/warning capture.
  log_line <- function(kind, text){
    dir_create(path_dir(failed_log))
    write(sprintf("[%s] Week %s, %s — %s: [%s] %s",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  week, year, link, kind, text),
          file = failed_log, append = file.exists(failed_log))
  }
  log_failure <- function(reason) log_line("ERROR", reason)

  withCallingHandlers({

    metadata <- tibble(week = week, year = year, link = link, title = title)

    print(paste0('Loading report: Week ', week, ' ', year))

    pdf <- download_pdf(link)
    if(is.null(pdf)){
      log_failure('Cannot load link')
      return(invisible(NULL))
    }

    # ---- Disease-counts table ----
    extracted <- tryCatch(extract_table_rows(pdf$text), error = function(e) e)
    if(inherits(extracted, "error") || is.null(extracted)){
      log_failure(paste('Cannot extract disease table rows:',
                        if(inherits(extracted, "error")) conditionMessage(extracted) else "not found"))
    } else {
      cases_table <- tryCatch(convert_cases_table(extracted), error = function(e) e)
      if(inherits(cases_table, "error")){
        log_failure(paste('Cannot convert disease table:', conditionMessage(cases_table)))
      } else {
        write_cases_to_csv(cases_table, metadata, file_loc = cases_file)
      }
    }

    # ---- District-level disease-counts tables (one per large province) ----
    # Skipped entirely when include_district = FALSE (province-level Table 1
    # above is still extracted either way) -- neither the CSV nor the failed
    # log is touched for these tables in that case, since "not extracted
    # because it wasn't requested" isn't a failure worth recording.
    district_compliance_tables <- list()

    if(include_district){

      district_case_blocks <- tryCatch(extract_district_case_tables(pdf$text), error = function(e) e)
      if(inherits(district_case_blocks, "error")){
        log_failure(paste('Cannot extract district case tables:', conditionMessage(district_case_blocks)))
      } else if(length(district_case_blocks) == 0){
        log_failure('No district case tables found in this bulletin')
      } else {
        for(block in district_case_blocks){
          district_cases_table <- tryCatch(convert_district_cases_table(block), error = function(e) e)
          if(inherits(district_cases_table, "error") || is.null(district_cases_table)){
            log_failure(paste0('Cannot convert district case table "', block$title, '": ',
                               if(inherits(district_cases_table, "error")) conditionMessage(district_cases_table) else "no rows extracted"))
          } else {
            write_district_cases_to_csv(district_cases_table, metadata, file_loc = district_cases_file)
          }
        }
      }

      # ---- District-level compliance tables (general sites + tertiary care) ----
      # Extracted before the national compliance table below so its output can
      # be reused as a fallback source for the national figure, for bulletins
      # that have no native national-level compliance table of their own (see
      # aggregate_district_compliance_to_national()).
      district_compliance_blocks <- tryCatch(extract_district_compliance_tables(pdf$text), error = function(e) e)
      if(inherits(district_compliance_blocks, "error")){
        log_failure(paste('Cannot extract district compliance tables:', conditionMessage(district_compliance_blocks)))
      } else if(length(district_compliance_blocks) == 0){
        log_failure('No district compliance tables found in this bulletin')
      } else {
        for(block in district_compliance_blocks){
          district_compliance_table <- tryCatch(convert_district_compliance_table(block), error = function(e) e)
          if(inherits(district_compliance_table, "error") || is.null(district_compliance_table)){
            log_failure(paste0('Cannot convert district compliance table "', block$title, '": ',
                               if(inherits(district_compliance_table, "error")) conditionMessage(district_compliance_table) else "no rows extracted"))
          } else {
            write_district_compliance_to_csv(district_compliance_table, metadata, file_loc = district_compliance_file)
            district_compliance_tables[[length(district_compliance_tables) + 1]] <- district_compliance_table
          }
        }
      }
    }

    # ---- Compliance table ----
    compliance_extracted <- tryCatch(extract_compliance_rows(pdf$data), error = function(e) e)
    if(inherits(compliance_extracted, "error")){

      fallback_table <- if(length(district_compliance_tables) > 0){
        tryCatch(
          aggregate_district_compliance_to_national(bind_rows(district_compliance_tables)),
          error = function(e) NULL
        )
      } else NULL

      if(!is.null(fallback_table)){
        message("No native national compliance table found -- derived one from this bulletin's district-level compliance data instead.")
        write_compliance_to_csv(fallback_table, metadata, file_loc = compliance_file)
      } else {
        log_failure(paste('Cannot extract compliance table:', conditionMessage(compliance_extracted)))
      }

    } else {
      compliance_table <- tryCatch(convert_compliance_table(compliance_extracted), error = function(e) e)
      if(inherits(compliance_table, "error")){
        log_failure(paste('Cannot convert compliance table:', conditionMessage(compliance_table)))
      } else {
        write_compliance_to_csv(compliance_table, metadata, file_loc = compliance_file)
      }
    }

    invisible(NULL)

  },
  # Non-exiting handlers: log the condition, then let it keep propagating
  # (no invokeRestart) so console printing and any caller-level handler --
  # e.g. run_pipeline.R's own withCallingHandlers that builds run_log.txt --
  # still see it and behave exactly as before.
  warning = function(w) log_line("WARNING", trimws(conditionMessage(w))),
  message = function(m) log_line("MESSAGE", trimws(conditionMessage(m)))
  )
}

# ---- Change detection: has an already-processed week's link changed? -----
#
# Every row written by write_cases_to_csv()/write_compliance_to_csv() carries
# the `Source link` it was extracted from. Comparing that stored link against
# the link currently shown on the bulletin page for the same (Year, Week) is
# how a corrected/replaced bulletin gets detected -- the week itself isn't
# new, but its content might now differ from what's on file.
detect_changed_links <- function(link_metadata, cases_file){
  if(!file.exists(cases_file)){
    return(link_metadata[0, ])
  }

  existing_links <- read_csv(cases_file, show_col_types = FALSE) |>
    distinct(Year, Week, `Source link`) |>
    rename(year = Year, week = Week, existing_link = `Source link`)

  link_metadata |>
    inner_join(existing_links, by = c("year", "week")) |>
    filter(link != existing_link) |>
    select(title, link, week, year)
}

# ---- Remove all rows for one (Year, Week) from a CSV ----------------------
# Used before re-processing a changed week, so the refreshed bulletin's data
# REPLACES what's on file rather than duplicating alongside it.
remove_existing_week <- function(file_loc, year, week){
  if(!file.exists(file_loc)) return(invisible(NULL))

  df <- read_csv(file_loc, show_col_types = FALSE) |>
    filter(!(Year == year & Week == week))

  write_csv(df, file_loc)
  invisible(NULL)
}

# ---- Main entry point ----------------------------------------------------
# Always returns (invisibly) a list(new = <n>, changed = <n>) summarising
# what it did, even when there's nothing to do -- this is what the CI driver
# script (run_pipeline.R) uses to build its run summary.
extract_PAK_data_main <- function(extract_all = FALSE,
                                  cases_file               = 'Data/PAK_IDSR_Data.csv',
                                  compliance_file          = 'Data/PAK_IDSR_Compliance.csv',
                                  district_cases_file      = 'Data/PAK_IDSR_Data_District.csv',
                                  district_compliance_file = 'Data/PAK_IDSR_Compliance_District.csv',
                                  include_district = TRUE){
  
  bulletin_url <-
    "https://www.nih.org.pk/phb/weekly-bulletin"
  
  links <- get_bulletin_links(
    bulletin_url
  )
  
  link_metadata <- extract_report_date(links)
  
  if(file.exists(cases_file)){
    existing <- read_csv(cases_file, show_col_types = FALSE) |>
      distinct(Year, Week)
  } else {
    existing <- tibble(Year = numeric(0), Week = numeric(0))
  }
  
  if(extract_all){
    
    new_rows     <- link_metadata
    changed_rows <- link_metadata[0, ]
    
  } else {
    
    # ---- New weeks: not already present in the cases CSV at all ----------
    new_rows <- link_metadata |>
      anti_join(existing, by = c("year" = "Year", "week" = "Week"))
    
    new_rows <- new_rows %>% filter(year >= 2026) # ignore those that are before 2026 and have changed
    
    # ---- Changed weeks: present, but the bulletin's link has changed -----
    changed_rows <- detect_changed_links(link_metadata, cases_file)
  }
  
  rows_to_run <- bind_rows(new_rows, changed_rows) |>
    distinct(year, week, .keep_all = TRUE)
  
  if(nrow(rows_to_run) == 0){
    message("No new or changed bulletins — dataset is already up to date.")
    return(invisible(list(new = 0L, changed = 0L)))
  }
  
  message(nrow(new_rows), " new week(s) and ", nrow(changed_rows), " changed week(s) to (re-)extract.")
  
  # Clear out the old data for any changed week BEFORE reprocessing it.
  # The district-level files are only cleared when this run is actually
  # going to re-extract district data -- if include_district = FALSE, any
  # district-level rows already on file from a PREVIOUS (district-including)
  # run for that week are left untouched rather than being wiped without a
  # replacement.
  if(nrow(changed_rows) > 0){
    for(i in seq_len(nrow(changed_rows))){
      message("Link changed for Week ", changed_rows$week[i], ", ", changed_rows$year[i],
             " — replacing existing data for that week.")
      remove_existing_week(cases_file, changed_rows$year[i], changed_rows$week[i])
      remove_existing_week(compliance_file, changed_rows$year[i], changed_rows$week[i])
      if(include_district){
        remove_existing_week(district_cases_file, changed_rows$year[i], changed_rows$week[i])
        remove_existing_week(district_compliance_file, changed_rows$year[i], changed_rows$week[i])
      }
    }
  }

  for(i_row in seq_len(nrow(rows_to_run))){

    process_one_bulletin(
      week                     = rows_to_run$week[i_row],
      year                     = rows_to_run$year[i_row],
      link                     = rows_to_run$link[i_row],
      title                    = rows_to_run$title[i_row],
      cases_file               = cases_file,
      compliance_file          = compliance_file,
      district_cases_file      = district_cases_file,
      district_compliance_file = district_compliance_file,
      include_district         = include_district
    )
  }
  
  invisible(list(new = nrow(new_rows), changed = nrow(changed_rows)))
}