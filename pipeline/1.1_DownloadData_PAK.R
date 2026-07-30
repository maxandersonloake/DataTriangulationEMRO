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
  "Rubella (CRS)"           = c("rubella (crs)", "rubella crs", "congenital rubella syndrome", "rubella"),
  "Rabies"                  = c("rabies"),
  "COVID-19"                = c("covid-19", "covid 19", "covid19", "sars-cov-2"),
  "Mpox"                    = c("mpox", "monkeypox"),
  "Anthrax"                 = c("anthrax"),
  "Chikungunya"             = c("chikungunya")
)
disease_lookup <- disease_lookup |> lapply(tolower)

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
region_lookup <- region_lookup |> lapply(tolower)

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

column_variant_map   <- build_matcher(column_lookup)
disease_variant_map  <- build_matcher(disease_lookup)
region_variant_map   <- build_matcher(region_lookup)

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
  
  NA_character_
}

match_column_name  <- function(x) fuzzy_match(x, column_variant_map,  max_dist = 2)
match_disease_name <- function(x) fuzzy_match(x, disease_variant_map, max_dist = 2)
match_region_name  <- function(x) fuzzy_match(x, region_variant_map,  max_dist = 2)

is_exact_disease_match <- function(raw, variant_map = disease_variant_map){
  tolower(trimws(raw)) %in% names(variant_map)
}

merge_wrapped_lines <- function(lines, n_cols, variant_map = disease_variant_map){
  
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
  #           tokens is a complete row. A data line with n_cols-2 tokens is
  #           treated as a row that's missing its last value (placeholder NA).
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
  
  for(ln in lines){
    
    coarse_tokens <- get_tokens_coarse(ln)
    if(length(coarse_tokens) == 0) next
    
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
    warning("Row(s) still missing their last value after stray-value resolution: ",
            paste(vapply(rows[still_missing], function(r) paste(r$name_parts, collapse = " "), character(1)),
                  collapse = "; "))
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
  rows <- merge_wrapped_lines(rows, n_cols = length(matched_cols), variant_map = disease_variant_map)
  
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

# ---- Compliance table -----------------------------------------------------
#
# Unlike the disease-counts table, the compliance table has no consistent
# label ("Table 1" etc.) and is preceded by a variable amount of title/bullet
# text (see the 3 examples: sometimes a title + 3 bullets, sometimes nothing
# at all). What IS consistent across all seen examples is:
#   - It appears somewhere in the first few pages of the bulletin.
#   - Its header row mentions "Region", "Expected", "Received" and
#     "Compliance" (in that column order).
#   - Its last row is always the "National" total.
# So rather than anchoring on a label, we scan the first few pages for a
# line matching that header signature, wherever it happens to sit relative
# to any preceding text.

extract_compliance_rows <- function(pdf_text, max_pages = 5){
  
  header_keywords <- c("Region", "Expected", "Received", "Compliance")
  n_search_pages  <- min(max_pages, length(pdf_text))
  
  # The compliance table sometimes runs across a page break (e.g. the first
  # 5 regions on one page, "Sindh" and "National" continuing at the top of
  # the next page with NO header repeated). Searching page-by-page would
  # lose that continuation entirely, so instead we concatenate the first
  # few pages into one block of lines and search/parse across all of them
  # together. Page footers ("Page | 3") and blank lines can legitimately
  # land in the middle of that combined block (right at the page boundary),
  # so they're filtered out wherever they occur rather than used as a
  # truncation point.
  combined_lines <- unlist(lapply(seq_len(n_search_pages), function(p){
    lines <- str_split(pdf_text[p], "\\n")[[1]]
    lines[nzchar(trimws(lines))]
  }))
  combined_lines <- combined_lines[
    !grepl("^\\s*Page\\s*\\|\\s*\\d+\\s*$", combined_lines, ignore.case = TRUE)
  ]
  
  start <- find_header_line(combined_lines, header_keywords, min_matches = 3)
  if(is.na(start)){
    stop("No compliance table found in the first ", n_search_pages,
         " page(s) of this bulletin — expected a table with Region/Expected/",
         "Received/Compliance columns ending in a National row.")
  }
  
  matched_cols <- c("Region", "Expected", "Received", "Compliance")
  
  rows <- combined_lines[(start + 1):length(combined_lines)]
  
  # A genuine new section (the disease-counts Table 1, or a Figure) marks
  # the end of anything that should be read as compliance-table content.
  # Reaching one of these before finding a National row (checked below)
  # means the table is genuinely missing/malformed, not just page-split.
  stop_idx <- which(grepl("^\\s*(Figure|Table\\s*1\\b)", rows, ignore.case = TRUE))
  if(length(stop_idx) > 0){
    rows <- rows[seq_len(min(stop_idx) - 1)]
  }
  
  rows <- merge_wrapped_lines(rows, n_cols = length(matched_cols), variant_map = region_variant_map)
  
  # The table always ends at (and includes) the National row -- trim
  # anything picked up after it.
  national_idx <- which(grepl("national", rows, ignore.case = TRUE))
  if(length(national_idx) == 0){
    stop("Found what looks like a compliance table header, but no National ",
         "row followed it within the first ", n_search_pages, " page(s) — ",
         "check whether the table runs onto a later page than expected.")
  }
  rows <- rows[seq_len(national_idx[1])]
  
  list(rows = rows, columns = matched_cols)
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

# ---- Per-bulletin processing (shared by both extraction modes) ---------

process_one_bulletin <- function(week, year, link, title,
                                 cases_file      = 'Data/PAK_IDSR_Data.csv',
                                 compliance_file = 'Data/PAK_IDSR_Compliance.csv',
                                 failed_log      = 'Data/failed_reports.txt'){
  
  log_failure <- function(reason){
    dir_create(path_dir(failed_log))
    write(sprintf("[%s] Week %s, %s — %s: %s",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  week, year, link, reason),
          file = failed_log, append = file.exists(failed_log))
  }
  
  metadata <- tibble(week = week, year = year, link = link, title = title)
  
  print(paste0('Loading report: Week ', week, ' ', year))
  
  pdf_text <- download_pdf_text(link)
  if(is.null(pdf_text)){
    log_failure('Cannot load link')
    return(invisible(NULL))
  }
  
  # ---- Disease-counts table ----
  extracted <- tryCatch(extract_table_rows(pdf_text), error = function(e) e)
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
  
  # ---- Compliance table ----
  compliance_extracted <- tryCatch(extract_compliance_rows(pdf_text), error = function(e) e)
  if(inherits(compliance_extracted, "error")){
    log_failure(paste('Cannot extract compliance table:', conditionMessage(compliance_extracted)))
  } else {
    compliance_table <- tryCatch(convert_compliance_table(compliance_extracted), error = function(e) e)
    if(inherits(compliance_table, "error")){
      log_failure(paste('Cannot convert compliance table:', conditionMessage(compliance_table)))
    } else {
      write_compliance_to_csv(compliance_table, metadata, file_loc = compliance_file)
    }
  }
  
  invisible(NULL)
}

# ---- Main entry point ----------------------------------------------------

extract_PAK_data_main <- function(extract_all = FALSE,
                                  cases_file      = 'Data/PAK_IDSR_Data.csv',
                                  compliance_file = 'Data/PAK_IDSR_Compliance.csv'){
  
  bulletin_url <-
    "https://www.nih.org.pk/phb/weekly-bulletin"
  
  links <- get_bulletin_links(
    bulletin_url
  )
  
  link_metadata <- extract_report_date(links)
  
  if(extract_all){
    
    rows_to_run <- link_metadata
    
  } else {
    
    # ---- Only extract weeks/years not already present in the cases CSV ----
    if(file.exists(cases_file)){
      existing <- read_csv(cases_file, show_col_types = FALSE) |>
        distinct(Year, Week)
    } else {
      existing <- tibble(Year = numeric(0), Week = numeric(0))
    }
    
    rows_to_run <- link_metadata |>
      anti_join(existing, by = c("year" = "Year", "week" = "Week"))
    
    if(nrow(rows_to_run) == 0){
      message("No new weeks to extract — dataset is already up to date.")
      return(invisible(NULL))
    }
    
    message(nrow(rows_to_run), " new week(s) to extract.")
  }
  
  for(i_row in seq_len(nrow(rows_to_run))){
    
    process_one_bulletin(
      week            = rows_to_run$week[i_row],
      year            = rows_to_run$year[i_row],
      link            = rows_to_run$link[i_row],
      title           = rows_to_run$title[i_row],
      cases_file      = cases_file,
      compliance_file = compliance_file
    )
  }
  
  invisible(NULL)
}