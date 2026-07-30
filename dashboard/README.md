# Pakistan IDSR Dashboard — WHO EMRO

## 1. Where these files go

Posit Connect Cloud deploys straight from a GitHub/GitLab repo and expects
`app.R` at the path you point it to. The cleanest setup is to put `app.R`
**at the root of your existing `DataTriangulationEMRO` RProject**, alongside
your existing `Data/` folder:

```
DataTriangulationEMRO/
├── DataTriangulationEMRO.Rproj
├── app.R                      <- from this delivery
├── www/
│   ├── who_brand.css          <- from this delivery
│   └── logo.png               <- YOU add the official WHO logo here
└── Data/
    ├── PAK_IDSR_Data.csv
    ├── PAK_IDSR_Compliance.csv
    └── pakistan_admin1.geojson  <- from this delivery (map boundaries)
```

**Note:** this is a change from your original app, where `app.R` lived in
`Dashboard/who_dashboard/` but read data from a top-level `Data/` folder
(those two locations didn't actually line up). Posit Connect Cloud needs
one self-contained folder, so I've moved everything to the project root.
If you'd rather keep a `Dashboard/who_dashboard/` subfolder, that's fine —
just make sure `Data/` sits at the same level as `app.R`, or update
`DATA_PATH`/`COMPLIANCE_PATH` at the top of `app.R` accordingly.

You still need to add the **official WHO logo PNG** at `www/logo.png` —
per WHO brand guidance this isn't something I can fabricate or bundle.

## 2. R packages required

```r
install.packages(c(
  "shiny", "dplyr", "tidyr", "readr", "ggplot2", "plotly", "DT",
  "scales", "base64enc", "leaflet", "sf"
))
```

`sf` and `leaflet` need system geospatial libraries (GDAL/GEOS/PROJ).
Posit Connect Cloud's standard R build image includes these, but if you
test locally on Windows, the easiest route is installing from CRAN
binaries (`install.packages("sf")` pulls prebuilt binaries on Windows —
no separate GDAL install needed).

**No internet access is required at runtime for the map.** Earlier I had
the app download Pakistan's boundaries on first load — that failed in
your test environment, presumably because outbound internet wasn't
available there. I've replaced it with a boundary file bundled directly
in the repo (`Data/pakistan_admin1.geojson`, ~75KB): seven province/
territory polygons (Balochistan, GB, ICT, KP, Sindh, AJK, Punjab),
dissolved from openly-licensed district-level OSM-derived data
(source: the `click_that_hood` project). The map now reads this static
file with `sf::st_read()` — nothing to download, nothing to cache, and
it'll render identically in any environment. If the file is ever missing
or unreadable, the map tab shows a message instead of crashing.

Before deploying, run `renv::init()` then `renv::snapshot()` in the
project so Posit Connect Cloud can reproduce your package versions (see
their docs: https://docs.posit.co/connect-cloud/how-to/r/renv.html).

## 3. Deploying to Posit Connect Cloud

1. Push this project (including `app.R`, `www/`, and `Data/`) to a GitHub
   repository.
2. In Posit Connect Cloud, choose **Publish > Shiny app**, connect your
   GitHub account, and select the repo/branch.
3. Point it at `app.R` in the repo root.
4. Connect Cloud will read your `renv.lock` to install the right package
   versions automatically.

## 4. Design decisions / assumptions made while building this

- **Map boundaries**: bundled as a static file rather than downloaded at
  runtime (see §2). Punjab is included on the map shape even though it
  isn't in your current CSVs — it just shows grey ("no data") since there's
  nothing to colour it with.
- **Map style**: no tile basemap (no OpenStreetMap/CartoDB background) —
  just Pakistan's province outlines on a plain background, zoomed to fit,
  with each region always labelled. This avoids depending on any external
  map tile server at runtime and keeps focus on the data itself.
- **Alerts tab**: scans *every* region (not just the one selected
  elsewhere in the dashboard) and every disease, flagging any combination
  ≥30% above its trailing 3-week average, each evaluated at its own most
  recent reporting week. The old per-location "quick view" on the Weekly
  summary table tab has been removed in favour of this, with a pointer
  left in its place.
- **Weekly table colour scale**: revised to be much less saturated in the
  middle of the range — pale green/red starts right around ±30%, and only
  reaches fully-saturated colour at ±100% or beyond. Text colour flips to
  white on the darkest cells so it stays legible throughout.
- **Header**: logo now sits on the right, title text on the left at a
  much larger size (32px).
- **Projected total cases** = reported cases ÷ (compliance % / 100),
  computed per region/week from `PAK_IDSR_Compliance.csv`. Where
  compliance is 0% or missing, the projection is left blank (undefined)
  rather than showing `Inf`.
- **Map metric** (on the Data visualisation tab): colours the % change vs
  the trailing 3-week average using whichever case-type (reported/
  projected) is toggled, for the most recent available week per region.
- **Year colours**: current year is always solid WHO Navy; earlier years
  cycle through a set of distinct WHO secondary colours (blue, orange,
  green, magenta, yellow, purple) rather than shades of one colour.

## 5. Open questions for you

- Should the map's % change metric always use **reported** cases
  regardless of the toggle, or follow the toggle as it does now?
- Any preference on which years should be pre-selected by default in the
  line chart (currently: all years selected)?
- The boundary file's district→province groupings (e.g. which district
  falls in KP vs Balochistan) came from my own knowledge of Pakistan's
  administrative geography, not an official source — if you spot a
  misplaced district on the map, let me know and I'll correct the
  mapping and re-dissolve the shapes.
