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
#   Data/gadm/              <- auto-created cache for Pakistan boundaries
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
library(leaflet)
library(sf)

# geodata is used only to fetch Pakistan admin-1 boundaries for the map.
# It is wrapped in tryCatch everywhere so the rest of the dashboard keeps
# working even if the package isn't installed or there's no connectivity.
has_geodata <- requireNamespace("geodata", quietly = TRUE)

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

# Distinct WHO-palette colours cycled across past years so each year reads
# as a genuinely different colour (current year is always WHO Navy, solid).
who_year_palette <- c(who_blue, who_orange, who_green, who_magenta, who_yellow, who_purple)

DATA_PATH       <- "Data/PAK_IDSR_Data.csv"
COMPLIANCE_PATH <- "Data/PAK_IDSR_Compliance.csv"
GADM_CACHE_DIR  <- "Data/gadm"

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
compliance_data <- read_csv(COMPLIANCE_PATH, show_col_types = FALSE) %>%
  mutate(
    Region = as.character(Region),
    `Compliance (%)` = suppressWarnings(as.numeric(`Compliance (%)`)),
    Week = as.integer(Week),
    Year = as.integer(Year)
  ) %>%
  select(Region, Week, Year, Compliance = `Compliance (%)`)

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

# ---- Helper: WHO Navy (current year) + distinct WHO palette (past years) --
year_colors <- function(years, current_year) {
  years <- sort(unique(years))
  past_years <- sort(years[years != current_year])
  n_past <- length(past_years)

  cols <- character(0)
  if (n_past > 0) {
    pal <- rep(who_year_palette, length.out = n_past)
    cols <- setNames(pal, as.character(past_years))
  }
  cols[as.character(current_year)] <- who_navy
  cols
}

# ---- Helper: Pakistan admin-1 boundaries (cached) --------------------------
# Region name -> abbreviation used in our data
gadm_name_map <- c(
  "Azad Kashmir"          = "AJK",
  "Balochistan"           = "Balochistan",
  "Islamabad"             = "ICT",
  "Khyber Pakhtunkhwa"    = "KP",
  "Punjab"                = "Punjab",
  "Sindh"                 = "Sindh",
  "Gilgit-Baltistan"      = "GB",
  "Gilgit Baltistan"      = "GB",
  "FATA"                  = "KP"
)

load_pak_regions <- function() {
  if (!has_geodata) return(NULL)
  tryCatch({
    dir.create(GADM_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
    v <- geodata::gadm(country = "PAK", level = 1, path = GADM_CACHE_DIR)
    sf_obj <- sf::st_as_sf(v)
    name_col <- intersect(c("NAME_1"), names(sf_obj))[1]
    sf_obj$region_code <- gadm_name_map[sf_obj[[name_col]]]
    sf_obj$region_code[is.na(sf_obj$region_code)] <- sf_obj[[name_col]][is.na(sf_obj$region_code)]
    sf_obj
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
    tags$link(rel = "stylesheet", type = "text/css", href = "who_brand.css"),
    tags$title("WHO EMRO | Pakistan IDSR Dashboard")
  ),

  # ---- WHO branded header ----
  div(
    class = "who-header",
    if (!is.null(logo_uri)) {
      tags$img(src = logo_uri, alt = "World Health Organization")
    } else {
      div(
        style = "color:#EF3842; border:2px dashed #EF3842; padding:6px 10px; font-size:12px;",
        paste0("LOGO NOT FOUND at www/", LOGO_FILENAME, " -- add the official WHO logo PNG there.")
      )
    },
    div(
      class = "who-header-text",
      div(class = "who-title", "Pakistan IDSR Surveillance Dashboard"),
      div(class = "who-subtitle", "Integrated Disease Surveillance and Response \u2014 WHO Eastern Mediterranean Region")
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
        style = "max-width: 980px; margin: 24px auto; padding: 0 16px;",
        div(
          class = "info-card",
          h4("About this dashboard"),
          p("This dashboard supports the Integrated Disease Surveillance and Response (IDSR) programme for ",
            strong("Pakistan"), ", triangulating weekly case notifications with reporting-site compliance data ",
            "published by Pakistan's National Institute of Health (NIH) in the weekly IDSR bulletins."),
          p("Use the ", strong("Data visualisation"), " tab to explore weekly trends by disease and region, and the ",
            strong("Weekly summary table"), " tab to scan for districts or diseases with unusually large increases.")
        ),
        div(
          class = "info-card",
          h4("Data sources and limitations"),
          tags$ul(
            tags$li(strong("Suspected vs confirmed cases: "), "Case counts reported through IDSR are predominantly ",
                    "suspected cases identified using standard syndromic case definitions at the point of care. ",
                    "They are not laboratory-confirmed unless stated otherwise in the source bulletin, and should be ",
                    "interpreted as an early-warning signal rather than a confirmed burden estimate."),
            tags$li(strong("Reporting compliance: "), "Not every expected reporting site submits data every week. ",
                    "The \u201cCompliance %\u201d reflects the share of expected reports actually received for a region ",
                    "in a given week. Weeks or regions with low compliance will under-represent true case counts."),
            tags$li(strong("Projected total cases: "), "To account for incomplete reporting, this dashboard offers a ",
                    "simple projection: reported cases \u00f7 (compliance % / 100). This assumes non-reporting sites have ",
                    "a similar case rate to reporting sites, which may not hold, particularly during active outbreaks ",
                    "or access constraints \u2014 treat projected figures as an indicative upper-bound, not a precise estimate."),
            tags$li(strong("Revisions: "), "Weekly figures may be revised in later bulletins as more complete data ",
                    "arrives; recent weeks are more likely to change than older ones."),
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
            tags$li("Data visualisation: choose a disease and region (or click a region on the map), toggle between ",
                    "reported / projected / both case counts, and show or hide individual years."),
            tags$li("Weekly summary table: scan the quick-view alerts for diseases or districts exceeding a 30% rise ",
                    "versus their trailing 3-week average, then review the full table with conditional shading.")
          )
        )
      )
    ),

    # ---------------------------------------------------------
    tabPanel(
      "Data visualisation",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("location", "Location", choices = location_choices, selected = "National"),
          selectInput("disease", "Disease", choices = disease_choices, selected = disease_choices[1]),
          radioButtons(
            "case_type", "Case counts to show",
            choices = c("Reported cases only" = "reported",
                        "Projected total cases" = "projected",
                        "Both" = "both"),
            selected = "reported"
          ),
          uiOutput("year_selector"),
          tags$hr(),
          tags$p(
            style = "font-size: 12px; color: #555;",
            "Hover over any point to see its year, week, case count",
            " (and reporting compliance %, where shown)."
          )
        ),
        mainPanel(
          width = 9,
          plotlyOutput("trend_plot", height = "480px"),
          tags$hr(),
          h4("Percentage change vs previous 3-week average, by region"),
          p(style = "font-size: 13px; color:#555;",
            "Click a region on the map to filter the chart above to that region. Colour reflects the % change in ",
            "reported cases for the selected disease, most recent available week."),
          leafletOutput("region_map", height = "420px")
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
          uiOutput("quick_view"),
          DTOutput("weekly_table")
        )
      )
    ),

    # ---------------------------------------------------------
    tabPanel(
      "References",
      div(
        style = "max-width: 900px; margin: 24px auto; padding: 0 16px;",
        div(
          class = "info-card",
          h4("Primary data source"),
          p("Weekly IDSR bulletins are published by Pakistan's National Institute of Health (NIH):"),
          tags$a(href = "https://www.nih.org.pk/idsr-bulletin/", target = "_blank",
                 "https://www.nih.org.pk/idsr-bulletin/")
        ),
        div(
          class = "info-card",
          h4("WHO Eastern Mediterranean Region"),
          tags$a(href = "https://www.emro.who.int/pak/programmes/disease-surveillance-and-response.html",
                 target = "_blank",
                 "WHO EMRO \u2014 Pakistan disease surveillance and response")
        ),
        div(
          class = "info-card",
          h4("Contact"),
          p("For questions about this dashboard, its data pipeline, or the WHO brand guidance it follows, ",
            "contact the WHO Health Emergencies Programme (WHE) Data Management and Analytics (DMA) team.")
        )
      )
    )
  ),

  div(
    class = "who-footer",
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
    checkboxGroupInput("years_selected", "Years to show", choices = yrs, selected = yrs, inline = TRUE)
  })

  output$trend_plot <- renderPlotly({
    d <- trend_data_all()
    validate(need(nrow(d) > 0, "No data available for this selection."))
    years_selected <- if (is.null(input$years_selected)) unique(d$Year) else as.integer(input$years_selected)
    d <- d %>% filter(Year %in% years_selected)
    validate(need(nrow(d) > 0, "No years selected."))

    current_year <- max(d$Year, na.rm = TRUE)
    cols <- year_colors(d$Year, current_year)

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
                                    "<br>Projected total cases: ", comma(Projected)))
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

    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", y = -0.2, title = list(text = "")),
        hoverlabel = list(bgcolor = "#FFFFFF", font = list(color = who_navy, family = "Source Sans Pro"))
      ) %>%
      config(displaylogo = FALSE)
  })

  # ---------------- Map ----------------
  region_change_data <- reactive({
    req(input$disease)
    locs <- location_choices[location_choices != "National"]
    metric <- if (identical(input$case_type, "projected")) "Projected" else "Reported"

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

  output$region_map <- renderLeaflet({
    if (is.null(pak_regions_sf)) {
      return(
        leaflet() %>% addTiles() %>% setView(lng = 69.35, lat = 30, zoom = 5) %>%
          addControl(
            "Map unavailable: Pakistan boundary data could not be downloaded in this environment.",
            position = "topright"
          )
      )
    }

    rc <- region_change_data()
    sf_map <- pak_regions_sf %>% left_join(rc, by = c("region_code" = "region"))

    pal <- colorNumeric(
      palette = colorRampPalette(c("#1A9850", "#FFFFFF", who_red_dark))(100),
      domain = c(-50, 50), na.color = "#CCCCCC"
    )

    leaflet(sf_map) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        layerId = ~region_code,
        fillColor = ~ifelse(is.na(pct), "#CCCCCC", pal(pmin(pmax(pct, -50), 50))),
        fillOpacity = 0.8,
        color = "#FFFFFF", weight = 1.5,
        highlightOptions = highlightOptions(weight = 3, color = who_navy, bringToFront = TRUE),
        label = ~paste0(
          region_code,
          ifelse(is.na(pct), ": no data", paste0(": ", round(pct, 1), "% vs prev 3-wk avg"))
        )
      ) %>%
      addLegend(
        pal = pal, values = seq(-50, 50, by = 10), title = "% change",
        position = "bottomright", labFormat = labelFormat(suffix = "%")
      )
  })

  observeEvent(input$region_map_shape_click, {
    clicked <- input$region_map_shape_click$id
    if (!is.null(clicked) && clicked %in% location_choices) {
      updateSelectInput(session, "location", selected = clicked)
    }
  })

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

    # Diverging red/green scale: only strong increases go bright red, only
    # strong decreases go deep green. Small fluctuations stay near-white.
    breaks <- c(-60, -30, -10, 0, 10, 30, 60, 100)
    pal    <- c("#1B7837", "#5AAE61", "#ACD8A7", "#FFFFFF", "#FDE3E4", "#F37176", "#EF3842", "#B0121A", "#7A0C12")

    formatStyle(
      dt,
      columns = tbl$week_cols,
      valueColumns = pct_col_names,
      backgroundColor = styleInterval(breaks, pal)
    )
  })

  output$quick_view <- renderUI({
    tbl <- weekly_table_reactive()
    latest_wk <- max(tbl$weeks)
    flagged <- lapply(tbl$diseases, function(dis) {
      pct <- tbl$pct[[paste0("Wk ", latest_wk, "_pct")]][tbl$diseases == dis]
      if (length(pct) == 0 || is.na(pct)) return(NULL)
      if (pct >= THRESHOLD_PCT) list(disease = dis, pct = pct) else NULL
    })
    flagged <- Filter(Negate(is.null), flagged)

    if (length(flagged) == 0) {
      return(div(
        class = "qv-none",
        paste0("No diseases in ", input$location_tbl, " exceeded a ", THRESHOLD_PCT,
               "% rise vs their trailing 3-week average in week ", latest_wk, ".")
      ))
    }

    flagged <- flagged[order(-sapply(flagged, function(x) x$pct))]
    boxes <- lapply(flagged, function(x) {
      div(class = "qv-box",
          strong(x$disease), paste0(" \u2014 up ", round(x$pct, 1), "% vs previous 3-week average (week ", latest_wk, ", ", input$location_tbl, ")"))
    })

    tagList(
      div(class = "qv-title", paste0("Quick view: diseases above ", THRESHOLD_PCT, "% increase (week ", latest_wk, ")")),
      boxes
    )
  })
}

shinyApp(ui, server)
