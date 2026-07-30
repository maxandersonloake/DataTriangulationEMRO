# ================================================================
# WHO IDSR Surveillance Dashboard
# ----------------------------------------------------------------
# WHO Colours
#   - Navy Blue (#00205C) as primary colour, WHO Blue (#009ADE) as secondary/accent colour
#   - Source Sans Pro typography
#   - WHO Emergency Red (#EF3842)
#
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

# ---- WHO brand colours -----------------------------------------
who_navy   <- "#00205C"
who_blue   <- "#009ADE"
who_blue10 <- "#E6EFF9"
who_red    <- "#EF3842"

DATA_PATH <- "Data/PAK_IDSR_Data.csv"

# Filename only, relative to the www/ folder next to app.R.
LOGO_FILENAME <- "logo.png"
LOGO_FULL_PATH <- file.path("Dashboard/who_dashboard/www", LOGO_FILENAME)

# Embed the logo as a base64 data URI, read directly off disk. This
# sidesteps Shiny's www/ URL routing entirely (the usual source of
# "logo won't show" issues), and prints a loud, specific warning at
# app startup -- check the R console -- if the file can't be found,
# instead of failing silently in the browser.
if (file.exists(LOGO_FULL_PATH)) {
  logo_uri <- base64enc::dataURI(file = LOGO_FULL_PATH, mime = "image/png")
} else {
  warning(
    "WHO logo NOT FOUND. R is looking for it at:\n  ",
    normalizePath(LOGO_FULL_PATH, mustWork = FALSE),
    "\nCheck the file exists there with that exact name/case, and that ",
    "app.R's working directory (getwd(): ", getwd(), ") is its own folder."
  )
  logo_uri <- NULL
}

# ---- Load data ----------------------------------------------------
raw_data <- read_csv(DATA_PATH, show_col_types = FALSE) %>%
  mutate(
    Disease = as.character(Disease),
    Province = as.character(Province),
    Cases = suppressWarnings(as.numeric(Cases)),
    Week = as.integer(Week),
    Year = as.integer(Year)
  ) %>%
  filter(!is.na(Cases), !is.na(Week), !is.na(Year))

disease_choices  <- sort(unique(raw_data$Disease))

# "Total" is the pre-computed national figure for each disease/week,
# not a real district -- exclude it from the district dropdown, and
# use it (via get_trend_data / get_all_disease_series below) as the
# source for "National" instead of summing the other districts.
location_choices <- c("National", sort(unique(raw_data$Province[raw_data$Province != "Total"])))

latest_year       <- max(raw_data$Year, na.rm = TRUE)

# ---- Helper: filter + aggregate for a given disease/location ------
get_trend_data <- function(disease, location) {
  d <- raw_data %>% filter(Disease == disease)
  if (location == "National") {
    d <- d %>% filter(Province == "Total")
  } else {
    d <- d %>% filter(Province == location)
  }
  d %>%
    group_by(Year, Week) %>%
    summarise(Cases = sum(Cases, na.rm = TRUE), .groups = "drop") %>%
    arrange(Year, Week)
}

# ---- Helper: national/district weekly series for ALL diseases -----
get_all_disease_series <- function(location) {
  d <- raw_data
  if (location == "National") {
    d <- d %>% filter(Province == "Total")
  } else {
    d <- d %>% filter(Province == location)
  }
  d %>%
    group_by(Disease, Year, Week) %>%
    summarise(Cases = sum(Cases, na.rm = TRUE), .groups = "drop")
}

# ---- Helper: % increase vs mean of previous 3 weeks ---------------
pct_increase_vs_prev3 <- function(series, disease, year, week) {
  cur <- series$Cases[series$Disease == disease & series$Year == year & series$Week == week]
  if (length(cur) == 0) return(NA_real_)
  
  prev <- series$Cases[series$Disease == disease & series$Year == year &
                         series$Week %in% (week - 3):(week - 1)]
  if (length(prev) == 0) return(NA_real_)
  
  avg_prev <- mean(prev, na.rm = TRUE)
  if (is.na(avg_prev) || avg_prev == 0) return(NA_real_)
  
  (cur - avg_prev) / avg_prev * 100
}

# ---- Helper: WHO Navy (current year) fading to WHO Blue (older) ---
# The current year is solid WHO Navy Blue. Every earlier year is WHO
# Blue, with opacity decreasing the further back it is -- the most
# recent past year is the least faded, the oldest year the most.
year_colors <- function(years, current_year) {
  years <- sort(unique(years))
  past_years <- sort(years[years != current_year])  # oldest -> most recent
  n_past <- length(past_years)
  
  cols <- character(0)
  if (n_past > 0) {
    alphas <- if (n_past == 1) 0.55 else seq(0.18, 0.65, length.out = n_past)
    past_cols <- vapply(alphas, function(a) grDevices::adjustcolor(who_blue, alpha.f = a), character(1))
    cols <- setNames(past_cols, as.character(past_years))
  }
  cols[as.character(current_year)] <- who_navy
  cols
}

# =================================================================
# UI
# =================================================================
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "who_brand.css")
  ),
  
  # ---- WHO branded header ----
  div(
    class = "who-header",
    if (!is.null(logo_uri)) {
      tags$img(src = logo_uri, alt = "World Health Organization")
    } else {
      div(
        style = "color:#EF3842; border:2px dashed #EF3842; padding:6px 10px; font-size:12px;",
        paste0("LOGO NOT FOUND at www/", LOGO_FILENAME, " -- see R console warning for the exact path checked.")
      )
    }#,
    # div(
    #   div(class = "who-title", "Pakistan IDSR Surveillance Dashboard"),
    #   div(class = "who-subtitle", "Integrated Disease Surveillance and Response")
    # )
  ),
  
  navbarPage(
    title = NULL,
    id = "who_nav",
    collapsible = TRUE,
    
    # ---------------------------------------------------------
    tabPanel(
      "Disease trends",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput("location", "Location", choices = location_choices, selected = "National"),
          selectInput("disease", "Disease", choices = disease_choices, selected = disease_choices[1]),
          tags$hr(),
          tags$p(
            style = "font-size: 12px; color: #555;",
            "Hover over any point to see its year, week and case count."
          )
        ),
        mainPanel(
          width = 9,
          plotlyOutput("trend_plot", height = "560px")
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
          tags$hr(),
          tags$p(
            style = "font-size: 12px; color: #555;",
            "Cell shading reflects the percentage increase in a week's cases ",
            "relative to the average of the previous three weeks. Darker red ",
            "indicates a larger increase."
          )
        ),
        mainPanel(
          width = 9,
          DTOutput("weekly_table")
        )
      )
    )
  ),
  
  div(
    class = "who-footer",
    "Data source: National Institute of Health (NIH) Pakistan, IDSR weekly bulletins.",
  )
)

# =================================================================
# SERVER
# =================================================================
server <- function(input, output, session) {
  
  # ---------------- Trends tab ----------------
  output$trend_plot <- renderPlotly({
    req(input$disease, input$location)
    
    d <- get_trend_data(input$disease, input$location)
    validate(need(nrow(d) > 0, "No data available for this selection."))
    
    current_year <- max(d$Year, na.rm = TRUE)
    d <- d %>%
      mutate(
        Year_f  = factor(Year, levels = sort(unique(Year))),
        is_current = Year == current_year,
        tooltip = paste0(
          "Year: ", Year,
          "<br>Week: ", Week,
          "<br>Cases: ", comma(Cases)
        )
      )
    
    cols <- year_colors(d$Year, current_year)
    
    p <- ggplot(d, aes(x = Week, y = Cases, group = Year_f, color = Year_f, text = tooltip)) +
      geom_line(aes(linewidth = is_current)) +
      geom_point(size = 1.6) +
      scale_color_manual(values = cols, name = "Year") +
      scale_linewidth_manual(values = c(`TRUE` = 1.4, `FALSE` = 0.9), guide = "none") +
      scale_x_continuous(breaks = pretty_breaks(n = 12)) +
      scale_y_continuous(labels = comma) +
      labs(
        title = paste0(input$disease, " \u2014 ", input$location),
        x = "Week",
        y = "Number of cases"
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
        legend = list(orientation = "h", y = -0.2, title = list(text = "Year")),
        hoverlabel = list(bgcolor = "#FFFFFF", font = list(color = who_navy, family = "Source Sans Pro"))
      ) %>%
      config(displaylogo = FALSE)
  })
  
  # ---------------- Weekly table tab ----------------
  output$weekly_table <- renderDT({
    req(input$location_tbl, input$n_weeks)
    
    series <- get_all_disease_series(input$location_tbl)
    validate(need(nrow(series) > 0, "No data available for this selection."))
    
    yr <- max(series$Year, na.rm = TRUE)
    weeks_available <- sort(unique(series$Week[series$Year == yr]))
    n_wk <- min(input$n_weeks, length(weeks_available))
    weeks_to_show <- tail(weeks_available, n_wk)
    
    diseases_here <- sort(unique(series$Disease))
    
    # Cases matrix (what's displayed)
    cases_wide <- series %>%
      filter(Year == yr, Week %in% weeks_to_show) %>%
      select(Disease, Week, Cases) %>%
      complete(Disease = diseases_here, Week = weeks_to_show, fill = list(Cases = 0)) %>%
      mutate(Week = paste0("Wk ", Week)) %>%
      pivot_wider(names_from = Week, values_from = Cases)
    
    # Keep week columns in chronological order
    week_cols <- paste0("Wk ", weeks_to_show)
    cases_wide <- cases_wide[, c("Disease", week_cols)]
    
    # Percentage-increase matrix, used only to colour cells (hidden columns)
    pct_matrix <- sapply(weeks_to_show, function(w) {
      sapply(diseases_here, function(dis) {
        pct_increase_vs_prev3(series, dis, yr, w)
      })
    })
    pct_wide <- as.data.frame(pct_matrix)
    colnames(pct_wide) <- paste0(week_cols, "_pct")
    pct_wide[is.na(pct_wide)] <- -999  # sentinel -> renders as "no increase" colour
    
    display_df <- cbind(cases_wide, pct_wide)
    
    pct_col_names <- paste0(week_cols, "_pct")
    pct_col_index <- which(names(display_df) %in% pct_col_names) - 1  # 0-based for DT
    
    dt <- datatable(
      display_df,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        columnDefs = list(list(visible = FALSE, targets = pct_col_index))
      ),
      caption = paste0(
        "Reported cases by disease, ", input$location_tbl,
        ", most recent ", n_wk, " weeks of ", yr
      )
    )
    
    dt <- formatStyle(
      dt,
      columns = week_cols,
      valueColumns = pct_col_names,
      backgroundColor = styleInterval(
        c(0, 25, 50, 100, 200),
        c("#FFFFFF", "#FDE3E4", "#F9B9BC", "#F37176", "#EF3842", "#B0121A")
      )
    )
    
    dt
  })
}

shinyApp(ui, server)
