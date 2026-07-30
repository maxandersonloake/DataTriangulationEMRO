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

THRESHOLD_PCT <- 30  # % increase vs previous 3-week average that triggers a "quick view" flag

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

# ---- Helper: % change vs mean of previous 3 weeks --------------------------
pct_change_vs_prev3 <- function(series, year, week, value_col = "Reported") {
  cur <- series[[value_col]][series$Year == year & series$Week == week]
  if (length(cur) == 0 || is.na(cur[1])) return(NA_real_)
  cur <- cur[1]

  prev <- series[[value_col]][series$Year == year & series$Week %in% (week - 3):(week - 1)]
  prev <- prev[!is.na(prev)]
  if (length(prev) == 0) return(NA_real_)

  avg_prev <- mean(prev, na.rm = TRUE)
  if (is.na(avg_prev) || avg_prev == 0) return(NA_real_)

  (cur - avg_prev) / avg_prev * 100
}

# ---- Helper: alerts -- every (location, disease) combo >= threshold ------
# Returns one row per location/disease pair that individually crosses the
# threshold, each evaluated at that location's own most recent reporting
# week. The Alerts tab groups these by disease and lists locations inline
# (e.g. "Pertussis: National (300% increase), ICT (42% increase)").
compute_alerts_long <- function(value_col = "Reported", threshold = THRESHOLD_PCT) {
  rows <- lapply(location_choices, function(loc) {
    series <- get_all_disease_series(loc)
    if (nrow(series) == 0) return(NULL)
    yr <- max(series$Year, na.rm = TRUE)
    wk <- max(series$Week[series$Year == yr], na.rm = TRUE)
    diseases_here <- sort(unique(series$Disease))

    pct_vals <- sapply(diseases_here, function(dis) {
      pct_change_vs_prev3(series %>% filter(Disease == dis), yr, wk, value_col = value_col)
    })

    data.frame(Location = loc, Disease = diseases_here, Year = yr, Week = wk,
               PctChange = as.numeric(pct_vals), stringsAsFactors = FALSE)
  })
  out <- bind_rows(rows)
  out <- out %>% filter(!is.na(PctChange), PctChange >= threshold)

  # Order locations within a disease the same way they appear in
  # location_choices (National first, then districts alphabetically)
  out$Location <- factor(out$Location, levels = location_choices)
  out %>% arrange(Disease, Location)
}

# ---- Shared diverging colour scale for % change tables --------------------
# Deliberately wide neutral band: ordinary week-to-week noise (within
# +-30%) stays plain white. Colour only appears for genuinely large moves,
# and only reaches fully-saturated colour beyond +-100%.
PCT_BREAKS   <- c(-100, -60, -30, 30, 60, 100)
PCT_BG_PAL   <- c("#1B7837", "#5AAE61", "#C7E9C0", "#FFFFFF", "#FBD9DB", "#F0868B", "#D7263D")
PCT_FONT_PAL <- c("#FFFFFF", "#111111", "#111111", "#111111", "#111111", "#111111", "#FFFFFF")

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
    tags$link(rel = "stylesheet", type = "text/css", href = "who_brand.css?v=6"),
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
      tags$img(src = logo_uri, alt = "World Health Organization", style = "height:78px;")
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
                    "assumed to be zero.")
          )
        ),
        div(
          class = "info-card",
          h4("How to use this dashboard"),
          tags$ol(
            tags$li("Alerts: a scan across every region and disease for any combination exceeding a 30% rise ",
                    "versus its trailing 3-week average."),
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
        style = "padding-bottom: 40px;",
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
              "Lists every disease with at least one location (National or a district) ",
              strong(paste0(THRESHOLD_PCT, "% or more")), " above its own trailing 3-week average, each evaluated ",
              "at that location's most recent reporting week."
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
              h4("Weekly case trend", style = "margin-top:0; margin-bottom:12px; font-size:17px;"),
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
              h4("Percent change to rolling 3-week average, by region", style = "margin-top:0; margin-bottom:12px; font-size:17px;"),
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
            "Cell shading reflects the percentage change in a week's cases relative to the average of the ",
            "previous three weeks. Darker red indicates a larger increase; darker green indicates a larger decrease."
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

    # Build a long-format frame: one row per (Year, Week, Series) where
    # Series is Reported (solid) and/or Projected (dashed)
    long_list <- list()
    if (case_type %in% c("reported", "both")) {
      long_list[["Reported"]] <- d %>%
        transmute(Year, Week, Series = "Reported", Cases = Reported, Compliance,
                  tooltip = paste0("Year: ", Year, "<br>Week: ", Week,
                                    "<br>Reported cases: ", comma(Reported),
                                    ifelse(is.na(Compliance), "",
                                           paste0("<br>Reporting compliance: ", Compliance, "%"))))
    }
    if (case_type %in% c("projected", "both")) {
      long_list[["Projected"]] <- d %>%
        transmute(Year, Week, Series = "Projected", Cases = Projected, Compliance,
                  tooltip = paste0("Year: ", Year, "<br>Week: ", Week,
                                    "<br>Projected total cases: ", comma(Projected),
                                    ifelse(is.na(Compliance), "",
                                           paste0("<br>Reporting compliance: ", Compliance, "%"))))
    }
    dl <- bind_rows(long_list) %>% filter(!is.na(Cases))
    validate(need(nrow(dl) > 0, "No data available for this combination of filters."))

    dl <- dl %>% mutate(
      Year_f = factor(Year, levels = sort(unique(dl$Year))),
      group_id = paste(Year, Series)
    )

    p <- ggplot(dl, aes(x = Week, y = Cases, group = group_id, color = Year_f,
                        linetype = Series, text = tooltip)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.4) +
      scale_color_manual(values = cols, name = "Year") +
      scale_linetype_manual(values = c(Reported = "solid", Projected = "dashed"), name = "Series") +
      scale_x_continuous(breaks = pretty_breaks(n = 12)) +
      scale_y_continuous(labels = comma) +
      labs(
        title = paste0(input$disease, " \u2014 ", input$location),
        x = "Week", y = "Number of cases"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        text = element_text(family = "sans"),
        plot.title = element_text(color = who_navy, face = "plain", size = 18),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "#E6E7E8")
      )

    gg <- ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", y = -0.35, title = list(text = "")),
        margin = list(b = 90),
        hoverlabel = list(bgcolor = "#FFFFFF", font = list(color = who_navy, family = "Source Sans Pro"))
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
      if (nrow(s) == 0) return(data.frame(region = loc, pct = NA_real_))
      yr <- max(s$Year, na.rm = TRUE)
      wk <- max(s$Week[s$Year == yr], na.rm = TRUE)
      pct <- pct_change_vs_prev3(s, yr, wk, value_col = metric)
      data.frame(region = loc, pct = pct, year = yr, week = wk)
    })
    bind_rows(region_list)
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
    sf_map$pct_capped <- pmin(pmax(sf_map$pct, -50), 50)

    # Label points guaranteed to sit inside each polygon (unlike a plain
    # centroid, which can fall outside oddly-shaped or multi-part regions)
    label_pts <- suppressWarnings(sf::st_point_on_surface(sf_map))
    coords <- sf::st_coordinates(label_pts)
    label_df <- data.frame(
      x = coords[, 1], y = coords[, 2],
      label = ifelse(is.na(sf_map$pct), sf_map$province, paste0(sf_map$province, "\n", round(sf_map$pct, 1), "%"))
    )

    p <- ggplot(sf_map) +
      geom_sf(aes(fill = pct_capped), color = "#8A8F94", linewidth = 0.3) +
      scale_fill_gradient2(
        low = "#1A9850", mid = "#FFFFFF", high = who_red_dark, midpoint = 0,
        limits = c(-50, 50), na.value = "#E6E7E8", name = "% change",
        labels = function(x) paste0(x, "%")
      ) +
      ggrepel::geom_text_repel(
        data = label_df, aes(x = x, y = y, label = label),
        size = 3.3, color = who_navy, fontface = "bold", lineheight = 0.9,
        segment.color = who_navy, segment.size = 0.3,
        max.overlaps = Inf, box.padding = 0.4, seed = 42
      ) +
      labs(title = input$disease) +
      theme_void(base_size = 13) +
      theme(
        legend.position = "right",
        plot.title = element_text(color = who_navy, face = "plain", size = 18)
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

    pct_matrix <- sapply(weeks_to_show, function(w) {
      sapply(diseases_here, function(dis) {
        s <- series %>% filter(Disease == dis)
        pct_change_vs_prev3(s, yr, w, value_col = value_col)
      })
    })
    pct_wide <- as.data.frame(pct_matrix)
    colnames(pct_wide) <- paste0(week_cols, "_pct")

    list(
      display_values = values_wide_wide,
      pct = pct_wide,
      week_cols = week_cols,
      year = yr, n_wk = n_wk, diseases = diseases_here, weeks = weeks_to_show,
      value_col = value_col
    )
  })

  output$weekly_table <- renderDT({
    tbl <- weekly_table_reactive()

    display_df <- cbind(tbl$display_values, tbl$pct)
    pct_col_names <- paste0(tbl$week_cols, "_pct")
    pct_col_index <- which(names(display_df) %in% pct_col_names) - 1

    label <- if (tbl$value_col == "Projected") "projected total" else "reported"

    dt <- datatable(
      display_df,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible = FALSE, targets = pct_col_index))
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
      valueColumns = pct_col_names,
      backgroundColor = styleInterval(PCT_BREAKS, PCT_BG_PAL),
      color = styleInterval(PCT_BREAKS, PCT_FONT_PAL)
    )
    dt
  })

  # ---------------- Alerts tab (text lines, grouped by disease) ----------
  alerts_reactive <- reactive({
    value_col <- if (identical(input$case_type_alerts, "projected")) "Projected" else "Reported"
    compute_alerts_long(value_col = value_col, threshold = THRESHOLD_PCT)
  })

  output$alerts_output <- renderUI({
    alerts <- alerts_reactive()
    label <- if (identical(input$case_type_alerts, "projected")) "projected total" else "reported"

    note <- p(
      style = "font-size: 12.5px; color:#555;",
      "Increase percentages are calculated versus each location's own previous 3-week average, using ",
      strong(label), " cases."
    )

    if (nrow(alerts) == 0) {
      return(tagList(
        note,
        div(class = "qv-none",
            paste0("No disease currently has any location \u2265", THRESHOLD_PCT,
                   "% above its trailing 3-week average."))
      ))
    }

    # One entry per disease, ordered by its most severe (largest %) location
    disease_order <- alerts %>%
      group_by(Disease) %>%
      summarise(max_pct = max(PctChange, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(max_pct)) %>%
      pull(Disease)

    lines <- lapply(disease_order, function(dis) {
      d <- alerts %>% filter(Disease == dis)
      loc_text <- paste0(as.character(d$Location), " (", round(d$PctChange, 1), "% increase)")
      div(class = "qv-box", strong(dis), ": ", paste(loc_text, collapse = ", "))
    })

    tagList(note, lines)
  })
}

shinyApp(ui, server)
