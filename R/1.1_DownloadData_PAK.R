
# Perform NIH Pakistan Weekly Bulletin extraction
#   To Do: 
#    - better identification of key table (rather than relying on it being labelled table 1)
#    - Understand how the fuzzy matching actually works
#    - email mandersonloake@gmail.com with any unexpected outputs (or if output is good)
#    - fix up dates matching each week (currently assumes week 1 = 01-01 to 01-07)
#    - add data validation step

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
  "Diphtheria (Probable)"   = c("diphtheria (probable)", "diphtheria probable"),
  "CCHF"                    = c("cchf", "crimean congo hemorrhagic fever", "crimean-congo haemorrhagic fever"),
  "Rubella (CRS)"           = c("rubella (crs)", "rubella crs", "congenital rubella syndrome")
)

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
    dat = pdftools::pdf_data(tmp)
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
  # e.g. "baloch" -> "balochistan". Guard against very short strings
  # producing spurious/ambiguous prefix hits.
  if(nchar(clean) >= 4){
    
    prefix_hits <- which(startsWith(names(variant_map), clean))
    
    if(length(prefix_hits) == 1){
      
      matched_name <- names(variant_map)[prefix_hits]
      canonical    <- unname(variant_map[prefix_hits])
      
      message(sprintf('Prefix matched "%s" -> "%s" (canonical: "%s")',
                      raw, matched_name, canonical))
      
      return(canonical)
    }
    
    if(length(prefix_hits) > 1){
      message(sprintf('Ambiguous prefix match for "%s": multiple candidates (%s) — skipping prefix match',
                      raw, paste(names(variant_map)[prefix_hits], collapse = ", ")))
    }
  }
  
  # ---- 3. Distance-based fuzzy match (flagged) ----------------------------
  # Tolerance scales with string length:
  #   <= 4 chars : 0 errors (exact only — already caught above, so no-op here)
  #   5-7 chars  : 1 error allowed
  #   8+ chars   : 2 errors allowed
  n <- nchar(clean)
  tol <- dplyr::case_when(
    n <= 4 ~ 0,
    n <= 7 ~ 1,
    TRUE   ~ 2
  )
  tol <- min(tol, max_dist)
  
  dists <- stringdist(clean, names(variant_map), method = "osa")
  best  <- which.min(dists)
  
  if(dists[best] <= tol){
    
    matched_name <- names(variant_map)[best]
    canonical    <- unname(variant_map[best])
    
    message(sprintf('Fuzzy matched "%s" -> "%s" (canonical: "%s", distance: %d)',
                    raw, matched_name, canonical, dists[best]))
    
    return(canonical)
  }
  
  NA_character_
}

match_column_name  <- function(x) fuzzy_match(x, column_variant_map,  max_dist = 2)
match_disease_name <- function(x) fuzzy_match(x, disease_variant_map, max_dist = 2)

merge_wrapped_lines <- function(lines, n_cols){
  
  # If the disease name runs over two/three lines then messes up table extraction
  # This detects it and fixes it
  
  get_tokens <- function(x){
    t <- str_split(str_trim(x), "\\s{2,}")[[1]]
    t[nzchar(t)]
  }
  
  is_numeric_tok <- function(tok) grepl("^(NR|[0-9,]+)$", tok)
  
  n <- length(lines)
  i <- 1
  out <- character(0)
  
  while(i <= n){
    
    name_parts  <- character(0)
    data_tokens <- character(0)
    
    # Keep consuming lines, splitting each into name vs. numeric tokens,
    # until we've collected all the data columns we need.
    while(i <= n && length(data_tokens) < n_cols - 1){
      tokens <- get_tokens(lines[i])
      is_num <- is_numeric_tok(tokens)
      name_parts  <- c(name_parts, tokens[!is_num])
      data_tokens <- c(data_tokens, tokens[is_num])
      i <- i + 1
    }
    
    # Peek ahead: if the very next line is pure name-text with NO numbers,
    # it's a trailing continuation of the name we just finished
    # (handles "Name1 / data / Name2" sandwich pattern).
    if(i <= n){
      next_tokens <- get_tokens(lines[i])
      if(length(next_tokens) > 0 && !any(is_numeric_tok(next_tokens))){
        name_parts <- c(name_parts, next_tokens)
        i <- i + 1
      }
    }
    
    full_name <- paste(name_parts, collapse = " ")
    
    if(length(data_tokens) == n_cols - 1 && nchar(full_name) > 0){
      out <- c(out, paste(c(full_name, data_tokens), collapse = "   "))
    } else if(length(name_parts) > 0 || length(data_tokens) > 0){
      warning("Could not assemble complete row near: ", full_name)
    }
  }
  
  out
}

# ---- Row extraction (newline-based, as originally written) -------------

extract_table_rows <- function(pdf_text){
  
  page <- pdf_text[grepl("Table\\s*1\\b", pdf_text, ignore.case = TRUE)]
  if(length(page) == 0) return(NULL)
  
  lines <- str_split(page, "\\n")[[1]]
  lines <- lines[nzchar(trimws(lines))]
  
  start <- grep("Disease|AJK|Punjab|Sindh|Total", lines, ignore.case = TRUE)
  if(length(start) == 0) return(NULL)
  
  header_line   <- lines[start[1]]
  header_tokens <- str_split(str_trim(header_line), "\\s{2,}")[[1]]
  matched_cols  <- vapply(header_tokens, match_column_name, character(1))
  
  if(any(is.na(matched_cols))){
    warning("Unmatched column header(s): ",
            paste(header_tokens[is.na(matched_cols)], collapse = ", "))
  }
  
  rows <- lines[(start[1] + 1):length(lines)]
  rows <- rows[!grepl("Figure", rows, ignore.case = TRUE)]
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
    warning(sum(split_lengths != length(matched_cols)),
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
    warning(sprintf(
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
    # Will extract data from all reports
    for (i_row in 1:nrow(link_metadata)){
      link = link_metadata$link[i_row]
      pdf_text = download_pdf_text(link)
      rows = extract_table_rows(pdf_text)
      cases_table = convert_cases_table(rows)
      write_cases_to_csv(cases_table, metadata = link_metadata[i_row,], file_loc = 'Data/PAK_IDSR_Data.csv')
    }
    
  } else {
    # Will extract data for weeks missing from ... 
    
  }
}

