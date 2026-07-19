
# Perform NIH Pakistan Weekly Bulletin extraction
#   To Do: 
#    - better identification of table of interest (rather than relying on it being labelled table 1)
#    - email mandersonloake@gmail.com with any unexpected outputs (or if output is good)
#    - fix up dates matching each week (currently assumes week 1 = 01-01 to 01-07)
#    - add data validation step
#    - If extract_all == FALSE, only extract for weeks missing from dataset
#    - Punjab delayed reporting?


# Questions:
#    - Rubella same as rubella (crs)?
#    - Diphtheria (probable) same as dipththeria?

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
column_lookup <- column_lookup |> lapply(tolower)

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
  "Rubella (CRS)"           = c("rubella (crs)", "rubella crs", "congenital rubella syndrome"),
  "Rabies"                  = c("rabies"),
  "COVID-19"                = c("covid-19", "covid 19", "covid19", "sars-cov-2"),
  "Mpox"                    = c("mpox", "monkeypox"),
  "Anthrax"                 = c("anthrax"),
  "Chikungunya"             = c("chikungunya")
)
disease_lookup <- disease_lookup |> lapply(tolower)

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
      link = str_replace(link, "\\?.*$", ""),
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

download_pdf_text <- function(url) {
  # Download a PDF and extract its text.
  # Args:
  #   url: URL of the PDF.
  # Returns:
  #   A character vector containing the text from each page, or NULL if the
  #   download or extraction fails.
  
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
    
    pdftools::pdf_text(tmp)
    
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

column_variant_map  <- build_matcher(column_lookup)
disease_variant_map <- build_matcher(disease_lookup)

fuzzy_match <- function(raw, variant_map, max_dist = 2){
  # Match disease/column names to the standard values
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
        stop(sprintf('Ambiguous prefix match for "%s": matches multiple different diseases (%s)',
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
      stop(sprintf('Ambiguous fuzzy match for "%s": matches multiple different diseases (%s) at distance %d',
                   raw, paste(canonicals, collapse = ", "), best_dist))
    }
    
    message(sprintf('Fuzzy matched "%s" -> "%s" (canonical: "%s", distance: %d)',
                    raw, names(variant_map)[best_idx[1]], canonicals, best_dist))
    
    return(canonicals)
  }
  
  NA_character_
}

match_column_name  <- function(x) fuzzy_match(x, column_variant_map,  max_dist = 2)
match_disease_name <- function(x) fuzzy_match(x, disease_variant_map, max_dist = 2)
is_exact_disease_match <- function(raw, variant_map = disease_variant_map){
  tolower(trimws(raw)) %in% names(variant_map)
}

merge_wrapped_lines <- function(lines, n_cols){
  
  # Disease names sometimes wrap across two (or three) lines in the source PDF.
  # Pure-text (non-data) lines are tokenized at the WORD level (not just on
  # double-spaces), since footnote/disclaimer text is often single-spaced and
  # glued to a genuine name continuation on the same line (e.g.
  # "(Probable) Punjab Data delayed due to non-reporting by HF" needs to
  # split into "(Probable)" + separate footnote words to be handled correctly).
  #
  # Approach:
  #   Pass 1: data lines anchor rows as before. Non-data lines contribute
  #           word-level tokens to a "floater run" between two rows.
  #   Pass 2: for a run between two rows, find the single split point
  #           minimising combined match score of both sides (as before).
  #           For a TRAILING run (nothing after it), search for the
  #           shortest prefix that best improves the previous row's match
  #           score; anything beyond that point is treated as noise
  #           (footnote/disclaimer text) and discarded with a warning,
  #           rather than being force-attached or turned into a phantom row.
  
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
  
  score_name <- function(name, variant_map = disease_variant_map){
    clean <- tolower(trimws(name))
    if(nchar(clean) == 0) return(Inf)
    if(clean %in% names(variant_map)) return(0)
    dists <- stringdist(clean, names(variant_map), method = "osa")
    min(dists)
  }
  
  # ---- Pass 1: data-anchored rows + name-only floater runs -----------------
  rows <- list()
  runs <- list()
  
  current_run <- character(0)
  
  flush_run <- function(){
    if(length(current_run) > 0){
      runs[[length(runs) + 1]] <<- list(after_row = length(rows), tokens = current_run)
      current_run <<- character(0)
    }
  }
  
  for(ln in lines){
    
    coarse_tokens <- get_tokens_coarse(ln)
    if(length(coarse_tokens) == 0) next
    
    is_num <- is_numeric_tok(coarse_tokens)
    
    if(any(is_num)){
      leading_name <- coarse_tokens[!is_num]
      data_tokens  <- coarse_tokens[is_num]
      
      flush_run()
      
      if(length(data_tokens) != n_cols - 1){
        stop("Data line has ", length(data_tokens), " value(s), expected ",
             n_cols - 1, ": ", paste(coarse_tokens, collapse = " "))
      }
      
      rows[[length(rows) + 1]] <- list(name_parts = leading_name, data_tokens = data_tokens)
      
    } else {
      # Pure-text line -- use WORD-level tokens so any footnote text glued
      # to a genuine name fragment can be separated later.
      current_run <- c(current_run, get_tokens_fine(ln))
    }
  }
  flush_run()
  
  # ---- Pass 2: resolve each run -----------------------------------------
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
      
      for(s in seq_len(m)){
        
        candidate <- paste(c(left_base, tok[seq_len(s)]), collapse = " ")
        sc <- score_name(candidate)
        
        if(sc == 0){
          # Exact match -- take it immediately, no need to keep searching.
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
      
      if(best_s > 0){
        rows[[left_idx]]$name_parts <- c(rows[[left_idx]]$name_parts, tok[seq_len(best_s)])
      }
      if(best_s < m){
        warning("Discarding trailing text as noise: ", paste(tok[(best_s + 1):m], collapse = " "))
      }
      next
    }
    
    # ---- Both sides present: split-point search as before ----------------
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
    
    if(best_s > 0){
      rows[[left_idx]]$name_parts <- c(rows[[left_idx]]$name_parts, tok[seq_len(best_s)])
    }
    if(best_s < m){
      rows[[right_idx]]$name_parts <- c(tok[(best_s + 1):m], rows[[right_idx]]$name_parts)
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

extract_table_rows <- function(pdf_text){
  
  page <- pdf_text[grepl("Table\\s*1\\b", pdf_text, ignore.case = TRUE)]
  if(length(page) == 0) return(NULL)
  
  lines <- str_split(page, "\\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]
  
  header_keywords <- c("Disease", "AJK", "Balochistan", "GB", "ICT", "KP", "Punjab", "Sindh", "Total")
  
  keyword_counts <- vapply(lines, function(ln){
    sum(sapply(header_keywords, function(k) grepl(k, ln, ignore.case = TRUE)))
  }, integer(1))
  
  start <- which(keyword_counts >= 4)
  if(length(start) == 0) return(NULL)
  
  header_line   <- lines[start[1]]
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
  
  rows <- lines[(start[1] + 1):length(lines)]
  figure_idx <- which(grepl("^\\s*Figure", rows, ignore.case = TRUE))
  if(length(figure_idx) > 0){
    rows <- rows[seq_len(min(figure_idx) - 1)]
  }
  rows <- rows[!grepl("Page\\s*\\|", rows, ignore.case = TRUE)]   # <-- drop footer lines
  rows <- merge_wrapped_lines(rows, n_cols = length(matched_cols))
  
  list(
    rows    = rows,
    columns = matched_cols
  )
}

convert_cases_table <- function(extracted, year, week, link){
  
  rows         <- extracted$rows
  matched_cols <- extracted$columns
  
  if(is.null(matched_cols) || length(matched_cols) == 0){
    stop("No matched columns found — check header parsing for this page.")
  }
  
  if(is.null(rows) || length(rows) == 0){
    stop("No data rows found — check row extraction/merging for this page.")
  }
  
  split_lengths <- rows |>
    str_replace_all("(?<=\\d),(?=\\d)", "") |>
    str_split("\\s{2,}") |>
    map_int(length)
  
  good_rows <- rows[split_lengths == length(matched_cols)]
  
  if(any(split_lengths != length(matched_cols))){
    stop(sum(split_lengths != length(matched_cols)),
            " row(s) dropped due to unexpected column count: ",
            paste(rows[split_lengths != length(matched_cols)], collapse = " | "))
  }
  
  data <- good_rows |>
    str_replace_all("(?<=\\d),(?=\\d)", "") |>
    str_split_fixed("\\s{2,}", n = length(matched_cols)) |>
    as_tibble(.name_repair = ~ matched_cols)
  
  data <- data |>
    mutate(
      Disease_raw = Disease,
      Disease     = vapply(Disease, match_disease_name, character(1)),
      Disease     = coalesce(Disease, Disease_raw)
    )
  
  data |>
    select(-Disease_raw) |>
    pivot_longer(-Disease, names_to = "Province", values_to = "Cases") |>
    mutate(
      Cases  = na_if(Cases, "NR"),
      Cases  = as.numeric(Cases),
      year   = year,
      week   = week,
      source = link
    ) |>
    filter(!is.na(Cases))
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
      Cases  = na_if(Cases, "NR"),
      Cases  = as.numeric(Cases)
    ) |>
    filter(!is.na(Cases))
  
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

extract_PAK_data_main(extract_all = FALSE){
  bulletin_url <-
    "https://www.nih.org.pk/phb/weekly-bulletin"
  
  links <- get_bulletin_links(
    bulletin_url
  )
  
  link_metadata = extract_report_date(links)
  
  if (extract_all){
    
    failed_log <- "Data/failed_reports.txt"
      
    
    for (i_row in 154:nrow(link_metadata)){
      
      week <- link_metadata$week[i_row]
      year <- link_metadata$year[i_row]
      link <- link_metadata$link[i_row]
      
      print(paste0('Loading report: Week ', week, ' ', year))
      
      pdf_text <- download_pdf_text(link)
      
      if (is.null(pdf_text)){
        fs::dir_create(fs::path_dir(failed_log))
        write(sprintf("[%s] Week %s, %s — %s: %s",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      week, year, link, 'Cannot load link'), file = failed_log, append = file.exists(failed_log))
        next
      } 
      
      rows <- extract_table_rows(pdf_text)
      if (is.null(rows)){
        fs::dir_create(fs::path_dir(failed_log))
        write(sprintf("[%s] Week %s, %s — %s: %s",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      week, year, link, 'Cannot extract rows'), file = failed_log, append = file.exists(failed_log))
        next
      } 
      
      cases_table <- convert_cases_table(rows)
      
      write_cases_to_csv(cases_table, link_metadata[i_row,], file_loc = 'Data/PAK_IDSR_Data.csv')
      
      }
      
    }
    
  } else {
    # Will extract data for weeks missing from ... 
    
  }
}

# week 13, 2025. Punjab* AWD (S.
# Week 34 2024 Meningi s