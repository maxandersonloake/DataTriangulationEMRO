# ============================================================================
# Fetch yearly population estimates for Pakistan's admin-1 units from the
# WorldPop API (v3) and write them to Data/population/pakistan/.
#
# Run from the project root:   Rscript scripts/worldpop_fetch_population.R
# Re-run it each year (e.g. in January) so the new year has its own population
# estimates; the dashboard warns when cases per 100,000 is selected for a year
# that is missing from the file.
# Needs: httr2, dplyr, readr. Optional: WORLDPOP_API_KEY in .Renviron to raise
# the anonymous request limits.
# ============================================================================

library(httr2)
library(dplyr)

# ============================================================================
# 1. Name mappings: WorldPop unit name -> dashboard province code
# ============================================================================
# WorldPop still uses older unit names and lists FATA separately, so FATA is
# added to KP (as in the dashboard). The concordance is also shown on the
# dashboard's References tab -- keep the two in step.
wp_to_province <- c(
  "Punjab" = "Punjab", "Sind" = "Sindh", "K.P." = "KP", "F.A.T.A." = "KP",
  "Baluchistan" = "Balochistan", "Azad Kashmir" = "AJK",
  "F.C.T." = "ICT", "Northern Areas" = "GB"
)
admin1_names <- c(
  Punjab = "Punjab", Sindh = "Sindh", KP = "Khyber Pakhtunkhwa",
  Balochistan = "Balochistan", AJK = "Azad Jammu & Kashmir",
  ICT = "Islamabad Capital Territory", GB = "Gilgit-Baltistan"
)


# ============================================================================
# 2. Function to pull the population for one year
# ============================================================================
`%||%` <- function(a, b) if (is.null(a)) b else a

API     <- "https://api.worldpop.org/v3"
api_key <- Sys.getenv("WORLDPOP_API_KEY")

# Waits and retries (up to 5 times) if the API says "too many requests".
wp_get <- function(path, query = list()) {
  req <- request(paste0(API, path)) |> req_url_query(!!!query) |> req_timeout(120)
  if (nzchar(api_key)) req <- req_headers(req, `X-API-Key` = api_key)
  req |>
    req_retry(
      max_tries    = 5,
      is_transient = \(r) resp_status(r) %in% c(429, 503),
      after        = \(r) as.numeric(resp_header(r, "Retry-After") %||% 10)
    ) |>
    req_perform() |>
    resp_body_json()
}

# WorldPop_Name is WorldPop's own name for the unit; where two WorldPop units
# are combined (FATA + KP) they are joined with " + ".
get_pak_population <- function(year) {
  res <- wp_get("/countries/PAK/units", list(year = year))

  raw <- bind_rows(lapply(res$units, function(u)
    data.frame(WorldPop_Name = u$name, pop = u$total_population)))
  raw$Province <- unname(wp_to_province[raw$WorldPop_Name])

  # Stop (rather than quietly write wrong numbers) if WorldPop renames, adds or
  # drops a unit, or if the units no longer add up to the country total.
  if (anyNA(raw$Province))
    stop("No dashboard name for WorldPop unit(s): ", paste(raw$WorldPop_Name[is.na(raw$Province)], collapse = ", "))
  stopifnot(
    setequal(raw$Province, names(admin1_names)),                      # all 7 dashboard units, none extra
    abs(sum(raw$pop) - res$total$total_population) < 1                # units add up to the national total
  )

  raw |>
    group_by(Province) |>
    summarise(WorldPop_Name = paste(WorldPop_Name, collapse = " + "),
              Population = round(sum(pop)), .groups = "drop") |>
    mutate(Year = year, Admin1_Name = unname(admin1_names[Province]), Source = "WorldPop API v3",
           Retrieved = as.character(Sys.Date())) |>
    select(Year, Province, Admin1_Name, WorldPop_Name, Population, Source, Retrieved)
}


# ============================================================================
# 3. Loop through the years and save
# ============================================================================
# From the first year of case data to the current year, so simply re-running the
# script each year adds the new year. (WorldPop's estimates run to 2030.)
first_year <- 2023
years <- first_year:min(as.integer(format(Sys.Date(), "%Y")), 2030)

pop <- bind_rows(lapply(years, get_pak_population))

dir.create("Data/population/pakistan", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(pop, "Data/population/pakistan/PAK_Admin1_Population.csv")
