
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

download_pdf <- function(url) {
  # Download a PDF to a temp file.
  # Args:
  #   url: URL of the PDF.
  # Returns:
  #   Path to the downloaded temp file, or NULL if the download fails.
  
  url <- utils::URLencode(url)
  tmp <- tempfile(fileext = ".pdf")
  
  result <- tryCatch({
    
    download.file(
      url = url,
      destfile = tmp,
      mode = "wb",
      quiet = TRUE
    )
    
    tmp
    
  }, error = function(e) {
    warning("Failed to download PDF: ", url)
    NULL
  })
  
  result
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

extract_pdf_table_data <- function(page_data, sidebar_gap = 40){
  
  # -------------------------------------------------------------------------
  # Extracts a disease-surveillance table from pdftools::pdf_data() output.
  #
  # ROWS: the Disease column's own words are linked into per-disease name
  # entries (splitting on a data-driven y-gap, then merging adjacent
  # fragments where doing so produces an exact match against the disease
  # lookup that neither fragment achieves alone -- resolves cases like
  # "AD (Non-" / "Cholera)" where the y-gap alone is ambiguous). Each
  # resulting disease entry's MEAN y becomes a canonical row center. Every
  # other word in the table (all data values) is then assigned to whichever
  # row center is nearest, bounded by the midpoint to the next-nearest row.
  #
  # COLUMNS: header words give one x-position per known column
  # (unambiguous, since each header word appears exactly once). The Disease
  # column's x-position is derived separately from where genuine disease
  # names start (using rows that carry real numeric data as an anchor, to
  # exclude any sidebar/caption text sharing the page). Every word is then
  # assigned to whichever column center is nearest, bounded by the midpoint
  # to the next-nearest column -- mirroring the row logic exactly.
  # -------------------------------------------------------------------------
  
  data_driven_gap_cutoff <- function(gaps, fallback = 4, tol_fraction = 0.6){
    nonzero <- gaps[gaps > 0]
    if(length(nonzero) == 0) return(fallback)
    rounded  <- round(nonzero)
    mode_gap <- as.numeric(names(sort(table(rounded), decreasing = TRUE))[1])
    max(mode_gap * tol_fraction, fallback)
  }
  
  score_name <- function(name, variant_map = disease_variant_map){
    clean <- tolower(trimws(name))
    if(nchar(clean) == 0) return(Inf)
    if(clean %in% names(variant_map)) return(0)
    min(stringdist(clean, names(variant_map), method = "osa"))
  }
  
  # Given sorted centers (row or column), build midpoint boundaries and
  # return a function assigning any value to the nearest bucket.
  make_bucket_assigner <- function(centers_sorted, ids_sorted){
    if(length(centers_sorted) == 1){
      return(function(v) rep(ids_sorted, length(v)))
    }
    midpoints  <- (centers_sorted[-1] + centers_sorted[-length(centers_sorted)]) / 2
    boundaries <- c(-Inf, midpoints, Inf)
    function(v) ids_sorted[findInterval(v, boundaries)]
  }
  
  d <- page_data
  d$text_lower <- tolower(d$text)
  d$x_center   <- d$x + d$width / 2
  
  # ---- 1. Locate header row, map words to canonical columns -----------------
  d$col_match <- vapply(d$text, function(t){
    m <- suppressMessages(tryCatch(match_column_name(t), error = function(e) NA_character_))
    if(is.na(m)) "" else m
  }, character(1))
  
  header_candidates <- d[d$col_match != "", ]
  if(nrow(header_candidates) == 0) stop("Could not locate header row (no column keywords found).")
  
  header_y <- as.integer(names(sort(table(header_candidates$y), decreasing = TRUE))[1])
  header_words <- header_candidates[header_candidates$y == header_y, ]
  header_words <- header_words[order(header_words$x_center), ]
  header_words <- header_words[!duplicated(header_words$col_match), ]  # one x per column
  
  header_row_all <- d[d$y == header_y, ]
  unmatched_header_words <- header_row_all$text[header_row_all$col_match == ""]
  if(length(unmatched_header_words) > 0){
    warning("Unrecognized column header word(s) on header row: ",
            paste(unmatched_header_words, collapse = ", "))
  }
  
  # ---- 2. Isolate table body --------------------------------------------------
  footer_y <- suppressWarnings(min(d$y[d$text_lower %in% c("page", "figure")], na.rm = TRUE))
  if(!is.finite(footer_y) || footer_y <= header_y) footer_y <- max(d$y) + 1
  
  body <- d[d$y > header_y & d$y < footer_y, ]
  if(nrow(body) == 0) stop("No table body content found below header row.")
  
  # ---- 3. Determine Disease column's true x position -------------------------
  # Candidate Disease-column words: anything left of the first real header
  # column (with margin). Among these, only trust the x of words that sit
  # on a line WITH numeric data -- that anchors the true disease-name start,
  # excluding sidebar/caption text (which never shares a line with numbers).
  first_header_x <- min(header_words$x_center)
  is_num <- grepl("^(NR|[0-9,]+)$", body$text)
  
  candidate_name_words <- body[body$x_center < first_header_x - sidebar_gap/2 & !is_num, ]
  numeric_rows_y <- unique(body$y[is_num])
  anchor_words   <- candidate_name_words[candidate_name_words$y %in% numeric_rows_y, ]
  
  if(nrow(anchor_words) > 0){
    disease_x <- as.numeric(names(sort(table(round(anchor_words$x_center, -1)), decreasing = TRUE))[1])
  } else {
    warning("No disease-name words found on a data-bearing line; using fallback position.")
    disease_x <- first_header_x - sidebar_gap
  }
  
  # ---- 4. Build column boundaries + assign every word to a column -----------
  col_centers <- c(disease_x, header_words$x_center)
  col_ids     <- c("Disease", header_words$col_match)
  ord <- order(col_centers)
  col_centers <- col_centers[ord]
  col_ids     <- col_ids[ord]
  
  assign_column <- make_bucket_assigner(col_centers, col_ids)
  body$column <- assign_column(body$x_center)
  
  # Discard sidebar text: Disease-column words sitting well left of the
  # anchored disease_x (i.e. words that only got bucketed into the Disease
  # column because there's nothing closer, but are really off in a sidebar).
  body <- body[!(body$column == "Disease" & !grepl("^(NR|[0-9,]+)$", body$text) &
                   body$x_center < disease_x - sidebar_gap), ]
  
  # ---- 5. Link Disease-column words into per-disease row entries ------------
  name_words <- body[body$column == "Disease", ]
  name_words <- name_words[order(name_words$y, name_words$x_center), ]
  
  if(nrow(name_words) == 0) stop("No disease-name text found in Disease column.")
  
  y_diffs   <- c(0, diff(name_words$y))
  gap_cut   <- data_driven_gap_cutoff(y_diffs[y_diffs > 0])
  name_words$run_id <- cumsum(y_diffs >= gap_cut)
  
  run_summary <- name_words |>
    dplyr::group_by(run_id) |>
    dplyr::summarise(mean_y = mean(y), name = paste(text, collapse = " "), .groups = "drop")
  
  # Merge adjacent runs where combining produces an exact lookup match that
  # neither run achieves alone (resolves ambiguous wraps like "AD (Non-" /
  # "Cholera)" where the y-gap alone can't distinguish wrap vs new row).
  i <- 1
  merged <- list()
  while(i <= nrow(run_summary)){
    current_name <- run_summary$name[i]
    current_y    <- run_summary$mean_y[i]
    if(i < nrow(run_summary)){
      combined_name <- paste(current_name, run_summary$name[i + 1])
      if(score_name(combined_name) == 0 && score_name(current_name) > 0){
        merged[[length(merged) + 1]] <- data.frame(
          mean_y = mean(c(current_y, run_summary$mean_y[i + 1])),
          name   = combined_name
        )
        i <- i + 2
        next
      }
    }
    merged[[length(merged) + 1]] <- data.frame(mean_y = current_y, name = current_name)
    i <- i + 1
  }
  run_summary <- do.call(rbind, merged)
  run_summary <- run_summary[order(run_summary$mean_y), ]
  run_summary$row_id <- seq_len(nrow(run_summary))
  
  # ---- 6. Build row boundaries from these canonical row centers -------------
  assign_row <- make_bucket_assigner(run_summary$mean_y, run_summary$row_id)
  
  # ---- 7. Assign all DATA words (non-Disease) to rows ------------------------
  data_words <- body[body$column != "Disease", ]
  if(nrow(data_words) == 0) stop("No data values found in table body.")
  
  data_words$row_id <- assign_row(data_words$y)
  
  cell_counts <- table(data_words$row_id, data_words$column)
  if(any(cell_counts > 1)){
    bad <- which(cell_counts > 1, arr.ind = TRUE)
    warning("Some (row, column) cells received more than one value -- ",
            "Affected: ", paste(sprintf("row %s / %s", rownames(cell_counts)[bad[,1]],
                                        colnames(cell_counts)[bad[,2]]), collapse = "; "))
  }
  
  # ---- 8. Pivot data words wide, attach disease names ------------------------
  wide <- data_words |>
    dplyr::select(row_id, column, text) |>
    dplyr::group_by(row_id, column) |>
    dplyr::summarise(text = paste(text, collapse = ""), .groups = "drop") |>
    tidyr::pivot_wider(names_from = column, values_from = text)
  
  wide$Disease_raw <- run_summary$name[match(wide$row_id, run_summary$row_id)]
  
  # ---- 9. Fuzzy-match disease names, warn on anything unresolved ------------
  wide$Disease <- vapply(wide$Disease_raw, function(nm){
    if(is.na(nm)) return(NA_character_)
    suppressMessages(tryCatch(match_disease_name(nm), error = function(e) NA_character_))
  }, character(1))
  
  unresolved <- wide[is.na(wide$Disease) & !is.na(wide$Disease_raw), ]
  if(nrow(unresolved) > 0){
    warning("Unrecognized/unexpected disease name(s), kept as raw text: ",
            paste(unique(unresolved$Disease_raw), collapse = "; "))
    wide$Disease <- dplyr::coalesce(wide$Disease, wide$Disease_raw)
  }
  
  no_name <- wide[is.na(wide$Disease_raw), ]
  if(nrow(no_name) > 0){
    warning(nrow(no_name), " data row(s) have no matched disease name at all.")
  }
  
  wide |>
    dplyr::select(-row_id, -Disease_raw) |>
    dplyr::select(Disease, dplyr::everything())
}

# extract_table_rows <- function(pdf_text){
#   
#   page <- pdf_text[grepl("Table\\s*1\\b", pdf_text, ignore.case = TRUE)]
#   if(length(page) == 0) return(NULL)
#   
#   lines <- str_split(page, "\\n")[[1]]
#   lines <- lines[nzchar(trimws(lines))]
#   
#   header_keywords <- c("Disease", "AJK", "Balochistan", "GB", "ICT", "KP", "Punjab", "Sindh", "Total")
#   
#   keyword_counts <- vapply(lines, function(ln){
#     sum(sapply(header_keywords, function(k) grepl(k, ln, ignore.case = TRUE)))
#   }, integer(1))
#   
#   start <- which(keyword_counts >= 4)
#   if(length(start) == 0) return(NULL)
#   
#   header_line   <- lines[start[1]]
#   header_tokens <- str_split(str_trim(header_line), "\\s{2,}")[[1]]
#   matched_cols  <- vapply(header_tokens, match_column_name, character(1))
#   
#   if(!("Disease" %in% matched_cols)){
#     header_tokens <- c("Disease", header_tokens)
#     matched_cols  <- c("Disease", matched_cols)
#   }
#   
#   if(any(is.na(matched_cols))){
#     
#     new_tokens  <- list()
#     new_matches <- list()
#     
#     for(idx in seq_along(header_tokens)){
#       
#       if(!is.na(matched_cols[idx])){
#         new_tokens[[idx]]  <- header_tokens[idx]
#         new_matches[[idx]] <- matched_cols[idx]
#         next
#       }
#       
#       split_attempt <- split_glued_token(header_tokens[idx])
#       
#       if(!is.null(split_attempt)){
#         resolved <- vapply(split_attempt, match_column_name, character(1))
#         if(!any(is.na(resolved))){
#           message(sprintf('Split glued header token "%s" -> %s',
#                           header_tokens[idx], paste(split_attempt, collapse = " + ")))
#           new_tokens[[idx]]  <- split_attempt
#           new_matches[[idx]] <- resolved
#           next
#         }
#       }
#       
#       new_tokens[[idx]]  <- header_tokens[idx]
#       new_matches[[idx]] <- NA_character_
#     }
#     
#     header_tokens <- unlist(new_tokens)
#     matched_cols  <- unlist(new_matches)
#     
#     if(any(is.na(matched_cols))){
#       stop("Unmatched column header(s): ",
#               paste(header_tokens[is.na(matched_cols)], collapse = ", "))
#     }
#   }
#   
#   rows <- lines[(start[1] + 1):length(lines)]
#   figure_idx <- which(grepl("^\\s*Figure", rows, ignore.case = TRUE))
#   if(length(figure_idx) > 0){
#     rows <- rows[seq_len(min(figure_idx) - 1)]
#   }
#   rows <- rows[!grepl("Page\\s*\\|", rows, ignore.case = TRUE)]   # <-- drop footer lines
#   rows <- merge_wrapped_lines(rows, n_cols = length(matched_cols))
#   
#   list(
#     rows    = rows,
#     columns = matched_cols
#   )
# }

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
      
    
    for (i_row in 1:63){ # Issue with 163. 
      
      week <- link_metadata$week[i_row]
      year <- link_metadata$year[i_row]
      link <- link_metadata$link[i_row]
      
      print(paste0('Loading report: Week ', week, ' ', year))
      
      tmp <- download_pdf(link)
      
      if (is.null(tmp)){
        fs::dir_create(fs::path_dir(failed_log))
        write(sprintf("[%s] Week %s, %s — %s: %s",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      week, year, link, 'Cannot load link'),
              file = failed_log, append = file.exists(failed_log))
        next
      }
      
      pdf_pages <- pdftools::pdf_text(tmp)
      
      target_page_idx <- grep("Table\\s*1\\b", pdf_pages, ignore.case = TRUE)[1]
      
      if (is.na(target_page_idx)){
        unlink(tmp)
        fs::dir_create(fs::path_dir(failed_log))
        write(sprintf("[%s] Week %s, %s — %s: %s",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      week, year, link, 'Table 1 not found in PDF'),
              file = failed_log, append = file.exists(failed_log))
        next
      }
      
      pdf_data_all <- pdftools::pdf_data(tmp)
      page_data    <- pdf_data_all[[target_page_idx]]
      
      unlink(tmp)   # clean up now that both extractions are done
      
      table_out <- extract_pdf_table_data(page_data)
      print(table_out, m = 30)
      
      # if (is.null(pdf_text)){
      #   fs::dir_create(fs::path_dir(failed_log))
      #   write(sprintf("[%s] Week %s, %s — %s: %s",
      #                 format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      #                 week, year, link, 'Cannot load link'), file = failed_log, append = file.exists(failed_log))
      #   next
      # } 
      # 
      # rows <- extract_table_rows(pdf_text)
      # if (is.null(rows)){
      #   fs::dir_create(fs::path_dir(failed_log))
      #   write(sprintf("[%s] Week %s, %s — %s: %s",
      #                 format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      #                 week, year, link, 'Cannot extract rows'), file = failed_log, append = file.exists(failed_log))
      #   next
      # } 
      # 
      # cases_table <- convert_cases_table(rows)
      
      write_cases_to_csv(cases_table, link_metadata[i_row,], file_loc = 'Data/PAK_IDSR_Data.csv')
      
      }
      
    }
    
  } else {
    # Will extract data for weeks missing from ... 
    
  }
}

# week 13, 2025. Punjab* AWD (S.
# Week 34 2024 Meningi s