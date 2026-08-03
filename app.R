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
# At least 10 total weeks of history (9 lookback weeks + the current week)
# are required before a location/disease series is evaluated.
CUSUM_LOOKBACK    <- 9
CUSUM_GUARD_BAND  <- 2
CUSUM_BASELINE_N  <- CUSUM_LOOKBACK - CUSUM_GUARD_BAND  # 7
CUSUM_MIN_POINTS  <- 10

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
raw_data <- read_csv(DATA_PATH, show_col_types = FALSE) %>%
  mutate(
    Disease  = as.character(Disease),
    Province = as.character(Province),
    Cases    = suppressWarnings(as.numeric(Cases)),
    Week     = as.integer(Week),
    Year     = as.integer(Year)
  ) %>%
  filter(!is.na(Cases), !is.na(Week), !is.na(Year))

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
    summarise(Cases = sum(Cases, na.rm = TRUE), .groups = "drop") %>%
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
    summarise(Cases = sum(Cases, na.rm = TRUE), .groups = "drop") %>%
    mutate(join_key = if (location == "National") "National" else location) %>%
    left_join(compliance_data, by = c("join_key" = "Region", "Week" = "Week", "Year" = "Year")) %>%
    mutate(Projected = ifelse(is.na(Compliance) | Compliance <= 0, NA_real_, Cases / (Compliance / 100))) %>%
    select(Disease, Year, Week, Reported = Cases, Projected, Compliance)
}



# ---- Helper: CUSUM/C2-style aberration stats at a specific (year, week) --
# `series` must be a single disease/location series covering ALL available
# years, arranged ascending by Year then Week (as produced by
# get_all_disease_series() %>% filter(Disease == ...), or get_trend_data()).
# Because the lookback is positional (the 9 rows immediately before the
# target row) rather than restricted to the same calendar year, a target
# week early in a year -- e.g. week 2 -- correctly pulls its baseline from
# the tail end of the PREVIOUS year's data rather than being treated as
# having no history. Returns NULL if there isn't enough history before this
# week (need CUSUM_MIN_POINTS rows up to and including it) or the baseline
# has zero variance (a flat baseline can't produce a meaningful SD-based
# threshold).
compute_cusum_stats_at <- function(series, year, week, value_col = "Reported") {
  series <- series %>% arrange(Year, Week)
  idx <- which(series$Year == year & series$Week == week)
  if (length(idx) == 0) return(NULL)
  idx <- idx[length(idx)]
  if (idx < CUSUM_MIN_POINTS) return(NULL)
  
  values <- series[[value_col]]
  current <- values[idx]
  window9 <- values[(idx - CUSUM_LOOKBACK):(idx - 1)]        # 9 pts before current (can span into prior year)
  baseline <- window9[seq_len(CUSUM_BASELINE_N)]              # drop 2 most recent adjacent pts, keep prior 7
  if (is.na(current) || any(is.na(baseline))) return(NULL)
  
  mu    <- mean(baseline)
  sigma <- sd(baseline)
  if (is.na(sigma) || sigma == 0) return(NULL)
  
  T1 <- mu + 2 * sigma
  T2 <- mu + 3 * sigma
  T3 <- mu + 4 * sigma
  level <- if (current > T3) 3L else if (current > T2) 2L else if (current > T1) 1L else 0L
  
  list(current = current, mu = mu, sigma = sigma, z = (current - mu) / sigma,
       T1 = T1, T2 = T2, T3 = T3, level = level, year = year, week = week)
}

# Convenience wrapper: evaluate the LAST (most recent) row of a series --
# used by the Alerts tab, which always looks at each location's latest week.
compute_cusum_stats <- function(series, value_col = "Reported") {
  series <- series %>% arrange(Year, Week)
  n <- nrow(series)
  if (n == 0) return(NULL)
  compute_cusum_stats_at(series, series$Year[n], series$Week[n], value_col = value_col)
}

# Convenience wrapper: just the z-score (in SD units) at a specific week, or
# NA if there isn't enough history / a usable baseline. Used to colour the
# map and the weekly summary table.
cusum_z_at <- function(series, year, week, value_col = "Reported") {
  stats <- compute_cusum_stats_at(series, year, week, value_col = value_col)
  if (is.null(stats)) NA_real_ else stats$z
}

# ---- Helper: alerts -- every (location, disease) combo above threshold ----
# Returns one row per location/disease pair whose most recent reporting
# week exceeds T1 (mu + 2*sigma) under the CUSUM/C2 method above, each
# evaluated at that location's own most recent reporting week. The Alerts
# tab groups these by disease and lists locations inline (e.g. "Pertussis:
# National (6.8SD), ICT (2.4SD)").
compute_alerts_long <- function(value_col = "Reported") {
  rows <- lapply(location_choices, function(loc) {
    series <- get_all_disease_series(loc)
    if (nrow(series) == 0) return(NULL)
    diseases_here <- sort(unique(series$Disease))
    
    res <- lapply(diseases_here, function(dis) {
      s <- series %>% filter(Disease == dis) %>% arrange(Year, Week)
      stats <- compute_cusum_stats(s, value_col = value_col)
      if (is.null(stats) || stats$level == 0L) return(NULL)
      data.frame(
        Location = loc, Disease = dis, Year = stats$year, Week = stats$week,
        Current = stats$current, Mu = stats$mu, Sigma = stats$sigma,
        Z = stats$z, Level = stats$level, stringsAsFactors = FALSE
      )
    })
    bind_rows(res)
  })
  out <- bind_rows(rows)
  if (is.null(out) || nrow(out) == 0) return(data.frame(
    Location = character(), Disease = character(), Year = integer(), Week = integer(),
    Current = numeric(), Mu = numeric(), Sigma = numeric(), Z = numeric(), Level = integer(),
    stringsAsFactors = FALSE
  ))
  
  # Order locations within a disease the same way they appear in
  # location_choices (National first, then districts alphabetically)
  out$Location <- factor(out$Location, levels = location_choices)
  out %>% arrange(Disease, desc(Level), desc(Z))
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
    tags$link(rel = "stylesheet", type = "text/css", href = "who_brand.css?v=7"),
    tags$title("WHO EMRO | Pakistan IDSR Dashboard")
  ),
  
  # ---- WHO branded header ----
  # Inline styles are applied alongside the who-header/.who-title classes
  # (in www/who_brand.css) as a safety net -- if a browser or CDN ever
  # serves a stale cached copy of the CSS file, the header still renders
  # correctly because these rules don't depend on the external file at all.
  div(
    class = "who-header",
    style = "display:flex; flex-direction:row-reverse; align-items:center; justify-content:space-between; padding:18px 28px; background-color:#FFFFFF; border-bottom:3px solid #00205C;",
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
          "Alpha Version")
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
        style = "max-width: 980px; margin: 24px 0; padding: 0 28px;",
        div(
          class = "info-card",
          h4("About this dashboard"),
          p("This dashboard supports the World Health Organisation (WHO) Regional Office for the Eastern Medierranean (EMRO)
            in monitoring disease case reporting from Pakistan, provided by the 
            Integrated Disease Surveillance and Response (IDSR) bulletins."),
          p("Check the ", strong("Alerts"), " tab for a scan of every region and disease for unusually large ",
            "increases, use ", strong("Data visualisation"), " to explore weekly trends by disease and region, and ",
            strong("Weekly summary table"), " for the full week-by-week breakdown.")
        ),
        div(
          class = "info-card",
          h4("Data sources and limitations"),
          tags$ul(
            tags$li(strong("Suspected vs confirmed cases: "), "Case counts reported through IDSR are predominantly ",
                    "suspected cases rather than laboratory-confirmed, and should be ",
                    "interpreted as an early-warning signal rather than a confirmed burden estimate."),
            tags$li(strong("Reporting compliance: "), "Not every expected reporting site submits data every week. ",
                    "The \u201cCompliance %\u201d reflects the share of expected reports actually received for a region ",
                    "in a given week. Weeks or regions with low compliance will under-represent true case counts."),
            tags$li(strong("Projected total cases: "), "To account for incomplete reporting, this dashboard offers a ",
                    "simple projection: reported cases \u00f7 (compliance % / 100). Where compliance data is missing or ",
                    "unusable for a given week/region (for example, no expected-report figure is available), no ",
                    "projection is shown for that week rather than assuming full compliance. This projection also ",
                    "assumes non-reporting sites have a similar case rate to reporting sites, which may not hold, ",
                    "particularly during active outbreaks or access constraints \u2014 treat projected figures as a rough ",
                    "estimate, not a precise figure."),
            tags$li(strong("Geographic coverage: "), "Regions reflect those published in the NIH IDSR bulletins ",
                    "(Balochistan, Gilgit-Baltistan, Islamabad Capital Territory, Khyber Pakhtunkhwa, Sindh, and Azad ",
                    "Jammu & Kashmir). Where a region has no data for a given week this is shown as a gap rather than ",
                    "assumed to be zero."),
            tags$li(strong("CUSUM alert detection method: "), "Alerts are generated with a CUSUM/C2 aberration detection method. ",
                    "For a given disease/location, take the 9 weeks immediately before the week being evaluated. ",
                    "Drop the 2 most recent of those 9 weeks (a \u201cguard band\u201d), so an emerging cluster in the ",
                    "most recent weeks can't inflate its own baseline. ",
                    "Calculate the mean (\u03bc) and standard deviation (\u03c3) of the remaining 7 baseline weeks. ",
                    "Compare the current week's count against three escalating thresholds: T1 = \u03bc + 2\u03c3 ",
                    "(yellow), T2 = \u03bc + 3\u03c3 (medium red), and T3 = \u03bc + 4\u03c3 (dark red).")
          )
        ),
        div(
          class = "info-card",
          h4("How to use this dashboard"),
          tags$ol(
            tags$li("Alerts: a scan across every region and disease for any combination whose most recent week ",
                    "triggers the CUSUM/C2 aberration detection method described above, listed by disease with ",
                    "National shown first where it also alerts."),
            tags$li("Data visualisation: choose a disease and region, toggle between reported / projected / both ",
                    "case counts, and show or hide individual years. The map highlights whichever region is selected."),
            tags$li("Weekly summary table: review the full week-by-week table with conditional shading for a chosen ",
                    "location.")
          )
        )
      )
    ),
    
    # ---------------------------------------------------------
    tabPanel(
      "Alerts",
      div(
        style = "padding-top: 22px; padding-bottom: 40px;",
        sidebarLayout(
          sidebarPanel(
            width = 3,
            radioButtons(
              "case_type_alerts", "Case counts to evaluate",
              choices = c("Reported cases" = "reported", "Projected total cases" = "projected"),
              selected = "reported"
            ),
            tags$hr(),
            tags$p(
              style = "font-size: 12px; color: #555;",
              "Lists every disease that triggers a ",
              strong("CUSUM-style aberration detection method"), " (rolling 9-week baseline, most recent 2 weeks ",
              "dropped). See the ", strong("Home"), " tab for a full explanation of how it's ",
              "calculated and a link to the methodology reference."
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
        style = "padding: 18px 28px;",
        div(
          style = "background-color:#F4F5F6; border:1px solid #DDE1E4; border-radius:6px; padding:10px 22px; margin-bottom:20px; display:flex; gap:24px; align-items:flex-end;",
          div(style = "flex: 1 1 0;", selectInput("disease", "Disease", choices = disease_choices, selected = disease_choices[1], width = "100%")),
          div(style = "flex: 1 1 0;", selectInput("location", "Location", choices = location_choices, selected = "National", width = "100%"))
        ),
        fluidRow(
          column(
            width = 6,
            div(
              class = "viz-panel",
              style = "background-color:#FFFFFF; border:1px solid #E0E4E8; border-radius:8px; padding:20px 22px 16px 22px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
              h4("Weekly case trend", style = "margin-top:0; margin-bottom:2px; font-size:17px;"),
              uiOutput("trend_subtitle"),
              plotlyOutput("trend_plot", height = "420px"),
              div(
                class = "viz-panel-controls",
                style = "margin-top:12px; padding-top:12px; border-top:1px solid #EEF1F4; min-height:150px;",
                uiOutput("year_selector"),
                radioButtons(
                  "case_type", "Case counts to show",
                  choices = c("Reported cases only" = "reported",
                              "Projected total cases" = "projected",
                              "Both" = "both"),
                  selected = "reported", inline = TRUE
                ),
                tags$p(
                  style = "font-size: 12px; color: #555; margin-bottom: 0;",
                  "Hover over any point to see its year, week, case count, and reporting compliance %."
                )
              )
            )
          ),
          column(
            width = 6,
            div(
              class = "viz-panel",
              style = "background-color:#FFFFFF; border:1px solid #E0E4E8; border-radius:8px; padding:20px 22px 16px 22px; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
              h4("Deviation from expected baseline (SD), by region", style = "margin-top:0; margin-bottom:2px; font-size:17px;"),
              uiOutput("map_subtitle"),
              plotOutput("region_map", height = "420px"),
              div(
                class = "viz-panel-controls",
                style = "margin-top:12px; padding-top:12px; border-top:1px solid #EEF1F4; min-height:150px;",
                radioButtons(
                  "case_type_map", "Case counts to colour the map by",
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
        style = "padding-top: 22px;",
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
            tags$p(
              style = "font-size: 12px; color: #555;",
              "Cell shading reflects how many standard deviations a week's cases are above or below its expected ",
              "baseline (CUSUM/C2 method: rolling 9-week window, most recent 2 weeks dropped, mean/SD of the prior ",
              "7 weeks). Darker red indicates more SD above baseline; darker green indicates more SD below."
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
        style = "max-width: 900px; margin: 24px 0; padding: 0 28px;",
        div(
          class = "info-card",
          h4("Primary data source"),
          p("Weekly IDSR bulletins published by Pakistan's National Institute of Health (NIH):"),
          tags$a(href = "https://www.nih.org.pk/phb/weekly-bulletin", target = "_blank",
                 "https://www.nih.org.pk/phb/weekly-bulletin")
        ),
        div(
          class = "info-card",
          h4("Alert detection methodology"),
          p("The Alerts tab and the SD-based shading on the map and weekly summary table use a modified CUSUM/C2 ",
            "aberration detection method (rolling 9-week baseline, most recent 2 weeks dropped as a guard band). ",
            "See the ", strong("Home"), " tab for a full explanation of how it's calculated."),
          tags$a(href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC3320440/", target = "_blank",
                 "Hutwagner et al., Comparing Aberration Detection Methods with Simulated Data")
        ),
        div(
          class = "info-card",
          h4("Contact"),
          p("For questions about this dashboard or its data pipeline, contact mandersonloake@gmail.com.")
        )
      )
    )
  ),
  
  div(
    class = "who-footer",
    style = "padding:16px 28px; font-size:12px; color:#555555; background-color:#E6EFF9; border-top:1px solid #C9DEF3;",
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
    d <- trend_data_all()
    validate(need(nrow(d) > 0, "No data available for this selection."))
    years_selected <- if (is.null(input$years_selected)) unique(d$Year) else as.integer(input$years_selected)
    d <- d %>% filter(Year %in% years_selected)
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
    
    dl <- dl %>% mutate(
      Year_f = factor(Year, levels = sort(unique(dl$Year))),
      group_id = paste(Year, Series)
    )
    
    # Ticks for every week, but only every 5th week gets a text label
    # (keeps the axis readable across a full 52-week year).
    week_breaks <- seq(floor(min(dl$Week, na.rm = TRUE)), ceiling(max(dl$Week, na.rm = TRUE)), by = 1)
    week_labels <- ifelse(week_breaks %% 5 == 0, as.character(week_breaks), "")
    
    p <- ggplot(dl, aes(x = Week, y = Cases, group = group_id, color = Year_f,
                        linetype = Series, text = tooltip)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.4) +
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
    req(input$disease)
    locs <- location_choices[location_choices != "National"]
    metric <- if (identical(input$case_type_map, "projected")) "Projected" else "Reported"
    
    region_list <- lapply(locs, function(loc) {
      s <- get_trend_data(input$disease, loc)
      if (nrow(s) == 0) return(data.frame(region = loc, z = NA_real_))
      yr <- max(s$Year, na.rm = TRUE)
      wk <- max(s$Week[s$Year == yr], na.rm = TRUE)
      z <- cusum_z_at(s, yr, wk, value_col = metric)
      data.frame(region = loc, z = z, year = yr, week = wk)
    })
    bind_rows(region_list)
  })
  
  output$map_subtitle <- renderUI({
    req(input$disease, input$location)
    div(class = "viz-subtitle",
        paste0("Disease: ", input$disease))
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
    label_df <- data.frame(
      x = coords[, 1], y = coords[, 2],
      label = ifelse(is.na(sf_map$z), sf_map$province, paste0(sf_map$province, "\n(", round(sf_map$z, 1), " SD)"))
    )
    
    p <- ggplot(sf_map) +
      geom_sf(aes(fill = z_capped), color = "#8A8F94", linewidth = 0.3) +
      scale_fill_gradient2(
        low = "#1A9850", mid = "#FFFFFF", high = who_red_dark, midpoint = 0,
        limits = c(-4, 4), na.value = "#E6E7E8", name = "SD from\nbaseline",
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
    value_col <- if (identical(input$case_type_alerts, "projected")) "Projected" else "Reported"
    compute_alerts_long(value_col = value_col)
  })
  
  output$alerts_output <- renderUI({
    alerts <- alerts_reactive()
    label <- if (identical(input$case_type_alerts, "projected")) "projected total" else "reported"
    
    note <- p(
      style = "font-size: 12.5px; color:#555;",
      "Alerts use a CUSUM aberration detection method (rolling 9-week baseline, most recent ",
      "2 weeks dropped) applied to each location's own ", strong(label), " cases."
    )
    
    if (nrow(alerts) == 0) {
      return(tagList(
        note,
        div(class = "qv-none",
            "No disease currently has any location more than 2 standard deviations above its expected baseline.")
      ))
    }
    
    level_class <- c(`1` = "qv-box-l1", `2` = "qv-box-l2", `3` = "qv-box-l3")
    
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
    
    tagList(note, lines)
  })
}

shinyApp(ui, server)